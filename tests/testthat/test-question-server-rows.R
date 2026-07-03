# Unit coverage for question_server()'s dynamic rules/filters row machinery
# via shiny::testServer. testServer's inputs are NULL until set, which exactly
# models the not-yet-initialised selectize state the seen-registry logic
# disambiguates, so the capture/reindex invariants are testable here without a
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
      expect_equal(rules[["1"]]@verb, "has content")
      expect_equal(rules[["1"]]@values, "quantile")
      expect_equal(rules[["2"]]@verb, "has at least")
    }
  )
})

test_that("deleting a rule re-indexes the survivor with its values preserved", {
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
      expect_equal(names(rules), "1")
      expect_equal(rules[["1"]]@values, "bbb")
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

      r = session$returned$question()@rules[["1"]]
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
    }
  )
})
