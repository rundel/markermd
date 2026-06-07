# Build a template exercising every rule verb, plus the path of the assignment
# it was built from.
build_serialization_fixture = function() {
  qmd = system.file(
    "examples/test_assignment/student1-excellent/assignment.qmd",
    package = "markermd"
  )
  ast = markermd:::parse_assignment_document(qmd)

  template = markermd::markermd_template(
    original_ast = ast,
    questions = list(
      markermd::markermd_question(
        1L, "Q2",
        markermd::markermd_node_selection(node_ids = "question-2-basic-programming"),
        list(
          markermd::markermd_rule("Any node", "has content", "*quantile*"),
          markermd::markermd_rule("Chunk", "has at least", 1L),
          markermd::markermd_rule("Heading", "has between", c(1, 3))
        )
      ),
      markermd::markermd_question(
        2L, "Q3",
        markermd::markermd_node_selection(node_ids = "question-3-data-visualization"),
        list(
          markermd::markermd_rule("Markdown", "lacks content", "*TODO*"),
          markermd::markermd_rule("Chunk", "has name", "*plot*"),
          markermd::markermd_rule("Heading", "has at most", 2L)
        )
      )
    ),
    metadata = markermd::markermd_metadata()
  )

  list(template = template, qmd = qmd)
}

# Write a small template YAML by hand, returning the file path.
write_yaml_lines = function(lines) {
  path = tempfile(fileext = ".yaml")
  writeLines(lines, path)
  path
}


test_that("write_template_yaml -> read_template_yaml round-trips questions and rules", {
  fix = build_serialization_fixture()
  path = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(fix$template, path, source_path = fix$qmd)

  back = markermd::read_template_yaml(path, require_ast = TRUE)

  expect_length(back@questions, 2)
  expect_equal(vapply(back@questions, function(q) q@id, integer(1)), c(1L, 2L))
  expect_equal(vapply(back@questions, function(q) q@name, character(1)), c("Q2", "Q3"))
  expect_equal(back@questions[[1]]@selected_nodes@node_ids, "question-2-basic-programming")
  expect_equal(back@questions[[2]]@selected_nodes@node_ids, "question-3-data-visualization")

  orig_rules = unlist(lapply(fix$template@questions, function(q) q@rules), recursive = FALSE)
  back_rules = unlist(lapply(back@questions, function(q) q@rules), recursive = FALSE)
  expect_equal(length(orig_rules), length(back_rules))
  for (i in seq_along(orig_rules)) {
    expect_equal(back_rules[[i]]@node_type, orig_rules[[i]]@node_type)
    expect_equal(back_rules[[i]]@verb, orig_rules[[i]]@verb)
    expect_equal(back_rules[[i]]@values, orig_rules[[i]]@values)
  }
})

test_that("polymorphic rule values keep their shape and type through a round-trip", {
  fix = build_serialization_fixture()
  path = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(fix$template, path, source_path = fix$qmd)
  back = markermd::read_template_yaml(path, require_ast = TRUE)

  between = back@questions[[1]]@rules[[3]]
  expect_equal(between@verb, "has between")
  expect_length(between@values, 2)
  expect_equal(between@values, c(1, 3))

  count = back@questions[[1]]@rules[[2]]
  expect_equal(count@verb, "has at least")
  expect_length(count@values, 1)
  expect_true(is.integer(count@values))

  pattern = back@questions[[1]]@rules[[1]]
  expect_equal(pattern@verb, "has content")
  expect_true(is.character(pattern@values))
  expect_equal(pattern@values, "*quantile*")
})

test_that("a hand-authored template file deserializes to the expected objects", {
  fix = build_serialization_fixture()
  path = write_yaml_lines(c(
    'format_version: "3.0"',
    sprintf('source: {path: "%s"}', fix$qmd),
    "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules:",
    "  - {node_type: Any node, verb: has content, pattern: \"*quantile*\"}"
  ))

  tmpl = markermd::read_template_yaml(path, require_ast = TRUE)
  expect_s3_class(tmpl, "markermd::markermd_template")
  expect_length(tmpl@questions, 1)
  expect_equal(tmpl@questions[[1]]@rules[[1]]@values, "*quantile*")
})

test_that("invalid template files are rejected by the S7 validators", {
  fix = build_serialization_fixture()
  src = sprintf('source: {path: "%s"}', fix$qmd)

  unknown_verb = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: [{node_type: Any node, verb: has bananas, pattern: x}]}"
  ))
  expect_error(markermd::read_template_yaml(unknown_verb), "Rule verb must be one of")

  bad_range = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: [{node_type: Heading, verb: has between, min: 5, max: 1}]}"
  ))
  expect_error(markermd::read_template_yaml(bad_range), "minimum must be <= maximum")

  empty_id = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    '- {id: 1, name: Q2, node_ids: [""], rules: []}'
  ))
  expect_error(markermd::read_template_yaml(empty_id), "non-empty")

  dup_names = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: []}",
    "- {id: 2, name: Q2, node_ids: [question-3-data-visualization], rules: []}"
  ))
  expect_error(markermd::read_template_yaml(dup_names), "names must be unique")
})

test_that("a missing format_version is reported", {
  fix = build_serialization_fixture()
  path = write_yaml_lines(c(
    sprintf('source: {path: "%s"}', fix$qmd),
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: []}"
  ))
  expect_error(markermd::read_template_yaml(path), "format_version")
})

test_that("an unresolvable source degrades for grading but errors for editing", {
  path = write_yaml_lines(c(
    'format_version: "3.0"',
    "source: {path: does-not-exist.qmd}",
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: []}"
  ))

  # mark(): grading proceeds with an empty AST
  graded = markermd::read_template_yaml(path, require_ast = FALSE)
  expect_s3_class(graded, "markermd::markermd_template")
  expect_equal(graded@metadata@total_nodes, 0L)

  # template(): editing needs the AST, so this must error
  expect_error(
    markermd::read_template_yaml(path, require_ast = TRUE),
    "Could not locate the assignment document"
  )
})

test_that("the assignment argument overrides an unresolvable stored path", {
  fix = build_serialization_fixture()
  path = write_yaml_lines(c(
    'format_version: "3.0"',
    "source: {path: does-not-exist.qmd}",
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: []}"
  ))

  tmpl = markermd::read_template_yaml(path, assignment = fix$qmd, require_ast = TRUE)
  expect_gt(tmpl@metadata@total_nodes, 0L)
})

test_that("validate_template_file checks files against the JSON Schema", {
  skip_if_not_installed("jsonvalidate")
  fix = build_serialization_fixture()

  good = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(fix$template, good, source_path = fix$qmd)
  expect_true(isTRUE(markermd::validate_template_file(good)))

  bad = write_yaml_lines(c(
    'format_version: "3.0"',
    "questions:",
    "- {id: 1, name: Q2, rules: [{node_type: Any node}]}"  # rule missing verb
  ))
  expect_false(isTRUE(markermd::validate_template_file(bad)))
})
