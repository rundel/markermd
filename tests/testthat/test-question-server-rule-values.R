# The rules row machine's value handling, via shiny::testServer like
# test-question-server-rows.R: what the question state holds after edits that
# the browser tests cannot see (an invalid value, a no-op report, the names the
# keyed row list must not leak into the serialized template).

qv_ast = function() {
  q2r::parse_qmd("# Q1\n\nSome text.\n\n```{r}\n1 + 1\n```\n", quiet = TRUE)
}

qv_question = function(rules = list()) {
  markermd_question(1L, "Q", markermd_node_selection(node_ids = "q1"), rules)
}

test_that("an edited question serializes its rules as an array that passes the schema", {
  # The row machine keys rules "1".."n" internally; the question state and the
  # serialized template must not carry those names, or the YAML holds a
  # mapping where the schema requires an array
  q = qv_question(rules = list(markermd:::new_markermd_rule(verb = "has content", values = "x")))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qv_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(add_rule = 1)
      edited = session$returned$question()
      expect_length(edited@rules, 2)
      expect_null(names(edited@rules))

      template = markermd_template(original_ast = qv_ast(), questions = list(edited))
      path = tempfile(fileext = ".yaml")
      qmd = tempfile(fileext = ".qmd")
      writeLines(c("# Q1", "", "Some text."), qmd)
      markermd::write_template_yaml(template, path, source_path = qmd)
      expect_true(isTRUE(markermd::validate_template_file(path)))
      expect_null(names(yaml::read_yaml(path)$questions[[1]]$rules))
    }
  )
})

test_that("a named rules list still serializes as an array", {
  q = qv_question(rules = list(
    "1" = markermd:::new_markermd_rule(),
    "2" = markermd:::new_markermd_rule(verb = "has content", values = "x")
  ))
  out = markermd:::question_to_list(q)
  expect_null(names(out$rules))
  expect_length(out$rules, 2)
})

test_that("a no-op report of a has-between rule leaves the serialized question unchanged", {
  # The client reports the numeric bounds as integers while the defaults are
  # doubles; normalizing on write keeps the editor's dirty flag honest
  q = qv_question(rules = list(markermd:::new_markermd_rule(verb = "has between")))
  before = markermd:::question_to_list(q)

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qv_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_1-verb` = "has between", `rule_1-values_min` = 0L, `rule_1-values_max` = 10L)
      expect_identical(markermd:::question_to_list(session$returned$question()), before)
    }
  )
})

test_that("an invalid rule value keeps the stored values and is reported, a verb change takes defaults", {
  q = qv_question(rules = list(markermd:::new_markermd_rule(verb = "has between", values = c(2, 4))))

  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(qv_ast()), initial_question = q),
    {
      session$flushReact()
      session$setInputs(`rule_1-verb` = "has between", `rule_1-values_min` = 6L, `rule_1-values_max` = 4L)
      expect_equal(session$returned$question()@rules[[1]]@values, c(2, 4))
      expect_match(output$rule_warnings$html, "minimum must be")

      session$setInputs(`rule_1-values_min` = NA)
      expect_equal(session$returned$question()@rules[[1]]@values, c(2, 4))
      expect_match(output$rule_warnings$html, "both a minimum and a maximum")

      session$setInputs(`rule_1-values_min` = 1L, `rule_1-values_max` = 3L)
      expect_equal(session$returned$question()@rules[[1]]@values, c(1, 3))
      expect_null(output$rule_warnings$html)

      session$setInputs(`rule_1-verb` = "has at least")
      expect_equal(session$returned$question()@rules[[1]]@values, 1L)
    }
  )
})

test_that("rule badges warn about an invalid regex and pass document-scope rules", {
  ast = qv_ast()
  bad = qv_question(rules = list(markermd:::new_markermd_rule(verb = "lacks content", values = "mean(")))
  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(ast), initial_question = bad),
    {
      session$flushReact()
      badge = output[["rule_1-status"]]$html
      expect_match(badge, "triangle-exclamation")
      expect_match(badge, "Invalid regular expression")
    }
  )

  # A document-scope question has no selected nodes by design; its rules
  # evaluate against the whole document in the editor as in grading
  doc = markermd_question(
    1L, "Document",
    rules = list(markermd:::new_markermd_rule(node_type = "Chunk", verb = "has at least", values = 1L)),
    scope = "document"
  )
  shiny::testServer(
    markermd:::question_server,
    args = list(id = "q1", ast = shiny::reactive(ast), initial_question = doc),
    {
      session$flushReact()
      badge = output[["rule_1-status"]]$html
      expect_match(badge, "fa-check")
      expect_no_match(badge, "No nodes selected")
    }
  )
})
