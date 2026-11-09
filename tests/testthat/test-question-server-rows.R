# Unit coverage for question_server()'s dynamic rules/filters row machinery
# via shiny::testServer. testServer's inputs are NULL until set, which exactly
# models the not-yet-initialized selectize state the seen-registry logic
# disambiguates, so the capture invariants are testable here without a
# browser. These tests target the module's observable contract (state and the
# returned question reactive), so they survive the internal refactors intact.

qs_ast = function() {
  q2r::parse_qmd("# Q1\n\nSome text.\n\n```{r}\n1 + 1\n```\n", quiet = TRUE)
}

qs_question = function(rules = list(), filters = list()) {
  q = markermd_question(1L, "Q", markermd_node_selection(), rules)
  q@filters = filters
  q
}

test_that("question_server loads initial rules and filters into its row state", {
  q = qs_question(
    rules = list(
      markermd:::new_markermd_rule(node_type = "Chunk", verb = "has content", values = "foo"),
      markermd:::new_markermd_rule()
    ),
    filters = list(
      markermd_filter_group(
        conditions = list(markermd:::new_markermd_filter_condition(type = "has text", value = "x")),
        negate = FALSE
      )
    )
  )

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      expect_length(session$returned$question()@rules, 2)
      expect_length(session$returned$question()@filters, 1)
      expect_false(is.null(output$rules_ui))
      expect_false(is.null(output$filters_ui))
    }
  )
})

test_that("pending rule edits are captured into state before an add", {
  q = qs_question(rules = list(markermd:::new_markermd_rule()))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_1-verb` = "has content")
      session$setInputs(`rule_1-values` = "quantile")
      session$setInputs(add_rule = 1)

      rules = session$returned$question()@rules
      expect_length(rules, 2)
      expect_null(names(rules))
      expect_equal(rules[[1]]@verb, "has content")
      expect_equal(rules[[1]]@values, "quantile")
      expect_equal(rules[[2]]@verb, "has at least")
    }
  )
})

test_that("deleting a rule keeps the survivor's key, values and inputs", {
  q = qs_question(rules = list(
    markermd:::new_markermd_rule(verb = "has content", values = "aaa"),
    markermd:::new_markermd_rule(verb = "has content", values = "bbb")
  ))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_1-delete` = 1)

      rules = session$returned$question()@rules
      expect_length(rules, 1)
      expect_null(names(rules))
      expect_equal(rules[[1]]@values, "bbb")

      # The survivor is still rule_2: its inputs keep editing it, and the next
      # rule takes a fresh key rather than the vacated one
      session$setInputs(`rule_2-verb` = "has content", `rule_2-values` = "ccc")
      expect_equal(session$returned$question()@rules[[1]]@values, "ccc")
      session$setInputs(add_rule = 1)
      expect_length(session$returned$question()@rules, 2)
      expect_match(output$rules_ui$html, "rule_3-verb", fixed = TRUE)
      expect_no_match(output$rules_ui$html, "rule_1-verb", fixed = TRUE)
    }
  )
})

test_that("a second delete event for a rule key already gone is dropped", {
  q = qs_question(rules = list(
    markermd:::new_markermd_rule(verb = "has content", values = "aaa"),
    markermd:::new_markermd_rule(verb = "has content", values = "bbb"),
    markermd:::new_markermd_rule(verb = "has content", values = "ccc")
  ))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_2-delete` = 1)
      session$setInputs(`rule_2-delete` = 2)

      rules = session$returned$question()@rules
      expect_equal(vapply(rules, function(r) r@values, ""), c("aaa", "ccc"))
    }
  )
})

test_that("a second structural event before the client re-reports keeps node types", {
  # Row inputs are never frozen, so between a structural change and the
  # client's re-report every input still holds the value the row shows; a
  # second Add inside that window must capture those values, not clear them
  # (a real freezeReactiveValue() would have set them to NULL, which the rules
  # machine reads as the deliberate clear "Any node")
  q = qs_question(rules = list(
    markermd:::new_markermd_rule(node_type = "Chunk", verb = "has content", values = "foo")
  ))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_1-node_types` = "Chunk", `rule_1-verb` = "has content", `rule_1-values` = "foo")
      session$setInputs(add_rule = 1)
      session$setInputs(add_rule = 2)

      rules = session$returned$question()@rules
      expect_length(rules, 3)
      expect_equal(rules[[1]]@node_type, "Chunk")
      expect_equal(rules[[1]]@verb, "has content")
      expect_equal(rules[[1]]@values, "foo")
    }
  )
})

test_that("clearing a reported node-type multiselect means Any node", {
  q = qs_question(rules = list(
    markermd:::new_markermd_rule(node_type = "Chunk", verb = "has at least", values = 1L)
  ))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_1-node_types` = "Chunk", `rule_1-verb` = "has at least", `rule_1-values` = 1L)
      session$setInputs(`rule_1-node_types` = NULL)
      expect_equal(session$returned$question()@rules[[1]]@node_type, "Any node")
    }
  )
})

test_that("a rule whose multiselect never reported keeps its stored node type", {
  # The async-init invariant: an uninitialised node-type multiselect reports
  # NULL; a capture triggered by another input's edit must keep the stored
  # node type rather than reading the NULL as a deliberate clear ("Any node")
  q = qs_question(rules = list(
    markermd:::new_markermd_rule(node_type = "Chunk", verb = "has content", values = "foo")
  ))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_1-verb` = "has at least")

      r = session$returned$question()@rules[[1]]
      expect_equal(r@node_type, "Chunk")
      expect_equal(r@verb, "has at least")
    }
  )
})

test_that("filter groups capture pending edits before structural changes and cascade on empty", {
  q = qs_question()

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(add_filter_group = 1)
      expect_length(session$returned$question()@filters, 1)

      # The negate toggle is a pending edit; adding a condition is the
      # structural change that must capture it first
      session$setInputs(`filter_group_1-negate` = TRUE)
      session$setInputs(`filter_group_1-add_condition` = 1)

      filters = session$returned$question()@filters
      expect_true(filters[[1]]@negate)
      expect_length(filters[[1]]@conditions, 2)

      # Deleting the conditions one by one: the second delete empties the
      # group, which cascades to deleting the group itself
      session$setInputs(`filter_1_2-delete` = 1)
      expect_length(session$returned$question()@filters[[1]]@conditions, 1)

      session$setInputs(`filter_1_1-delete` = 1)
      expect_length(session$returned$question()@filters, 0)

      # The next group and its conditions take fresh keys
      session$setInputs(add_filter_group = 2)
      expect_match(output$filters_ui$html, "filter_2_1-type", fixed = TRUE)
      session$setInputs(`filter_group_2-add_condition` = 1)
      expect_match(output$filters_ui$html, "filter_2_2-type", fixed = TRUE)
    }
  )
})

test_that("deleting a filter group keeps the other groups' keys and drops a repeat event", {
  fg = function(v) {
    markermd_filter_group(
      conditions = list(markermd:::new_markermd_filter_condition(type = "has text", value = v)),
      negate = FALSE
    )
  }
  q = qs_question(filters = list(fg("a"), fg("b"), fg("c")))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`filter_group_2-delete` = 1)
      session$setInputs(`filter_group_2-delete` = 2)

      filters = session$returned$question()@filters
      expect_equal(vapply(filters, function(g) g@conditions[[1]]@value, ""), c("a", "c"))
      expect_match(output$filters_ui$html, "filter_3_1-type", fixed = TRUE)
      expect_no_match(output$filters_ui$html, "filter_2_1-type", fixed = TRUE)

      # Group 3's inputs still edit group 3 (the update observer requires
      # every condition's type input, as a bound page always has)
      session$setInputs(`filter_1_1-type` = "has text", `filter_3_1-type` = "has text", `filter_3_1-value` = "zz")
      expect_equal(session$returned$question()@filters[[2]]@conditions[[1]]@value, "zz")
    }
  )
})

test_that("a second add-group event before the client re-reports keeps the stored kinds", {
  # The filters machine still freezes a condition's value input on a type
  # change, so a NULL for a noted node-type multiselect is possible here and
  # must keep the stored kinds (an empty kind set would match nothing)
  q = qs_question(filters = list(
    markermd_filter_group(
      conditions = list(markermd:::new_markermd_filter_condition(type = "node type", value = "Chunk")),
      negate = FALSE
    )
  ))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qs_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`filter_1_1-type` = "node type", `filter_1_1-value` = "Chunk",
                        `filter_1_1-negate` = FALSE, `filter_group_1-negate` = FALSE)
      session$setInputs(add_filter_group = 1)
      session$setInputs(`filter_1_1-type` = NULL, `filter_1_1-value` = NULL,
                        `filter_1_1-negate` = NULL, `filter_group_1-negate` = NULL)
      session$setInputs(add_filter_group = 2)

      filters = session$returned$question()@filters
      expect_length(filters, 3)
      expect_equal(filters[[1]]@conditions[[1]]@value, "Chunk")
    }
  )
})
