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
          markermd::markermd_rule("Heading", "has at most", 2L),
          markermd::markermd_rule(c("Markdown", "Raw Block"), "has at least", 1L)
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

test_that("a rule accepts a vector of node types and rejects bad ones", {
  rule = markermd::markermd_rule(c("Markdown", "Raw Block"), "has at least", 1L)
  expect_equal(rule@node_type, c("Markdown", "Raw Block"))

  expect_error(markermd::markermd_rule(c("Markdown", "Markdown"), "has at least", 1L), "unique")
  expect_error(markermd::markermd_rule("Bogus", "has at least", 1L), "must be one of")
  expect_error(markermd::markermd_rule(character(0), "has at least", 1L), "at least one")
})

test_that("a multi-type rule round-trips and loads from a hand-authored sequence", {
  fix = build_serialization_fixture()
  path = write_yaml_lines(c(
    'format_version: "3.0"',
    sprintf('source: {path: "%s"}', fix$qmd),
    "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules:",
    "  - {node_type: [Markdown, Raw Block], verb: has at least, count: 1}"
  ))

  tmpl = markermd::read_template_yaml(path, require_ast = TRUE)
  expect_equal(tmpl@questions[[1]]@rules[[1]]@node_type, c("Markdown", "Raw Block"))
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

  good_multi = write_yaml_lines(c(
    'format_version: "3.0"',
    sprintf('source: {path: "%s"}', fix$qmd),
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: [{node_type: [Markdown, Raw Block], verb: has at least, count: 1}]}"
  ))
  expect_true(isTRUE(markermd::validate_template_file(good_multi)))

  bad_member = write_yaml_lines(c(
    'format_version: "3.0"',
    sprintf('source: {path: "%s"}', fix$qmd),
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: [{node_type: [Markdown, Bogus], verb: has at least, count: 1}]}"
  ))
  expect_false(isTRUE(markermd::validate_template_file(bad_member)))

  empty_types = write_yaml_lines(c(
    'format_version: "3.0"',
    sprintf('source: {path: "%s"}', fix$qmd),
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: [{node_type: [], verb: has at least, count: 1}]}"
  ))
  expect_false(isTRUE(markermd::validate_template_file(empty_types)))
})


test_that("question filters round-trip through YAML and only appear when present", {
  fix = build_serialization_fixture()

  q = fix$template@questions[[1]]
  q@filters = list(
    markermd::markermd_filter_group(conditions = list(
      markermd::markermd_filter_condition(type = "node type", value = "Div"),
      markermd::markermd_filter_condition(type = "has class", value = "hint")
    )),
    markermd::markermd_filter_group(conditions = list(
      markermd::markermd_filter_condition(type = "has text", value = "Q[0-9]+")
    ))
  )
  fix$template@questions[[1]] = q

  path = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(fix$template, path, source_path = fix$qmd)

  # the filter-less question emits no filters key at all
  raw = yaml::read_yaml(path)
  expect_true("filters" %in% names(raw$questions[[1]]))
  expect_false("filters" %in% names(raw$questions[[2]]))

  back = markermd::read_template_yaml(path, require_ast = TRUE)
  filters = back@questions[[1]]@filters
  expect_length(filters, 2)
  expect_length(filters[[1]]@conditions, 2)
  expect_equal(filters[[1]]@conditions[[1]]@type, "node type")
  expect_equal(filters[[1]]@conditions[[1]]@value, "Div")
  expect_equal(filters[[1]]@conditions[[2]]@type, "has class")
  expect_equal(filters[[1]]@conditions[[2]]@value, "hint")
  expect_equal(filters[[2]]@conditions[[1]]@type, "has text")
  expect_equal(filters[[2]]@conditions[[1]]@value, "Q[0-9]+")

  expect_equal(back@questions[[2]]@filters, list())

  # empty groups are dropped on write
  q@filters = list(markermd::markermd_filter_group())
  fix$template@questions[[1]] = q
  markermd::write_template_yaml(fix$template, path, source_path = fix$qmd)
  expect_false("filters" %in% names(yaml::read_yaml(path)$questions[[1]]))
})

test_that("filter negation round-trips and is omitted when FALSE", {
  fix = build_serialization_fixture()

  q = fix$template@questions[[1]]
  q@filters = list(
    markermd::markermd_filter_group(
      conditions = list(
        markermd::markermd_filter_condition(type = "node type", value = "Div"),
        markermd::markermd_filter_condition(type = "has class", value = "hint", negate = TRUE)
      ),
      negate = TRUE
    )
  )
  fix$template@questions[[1]] = q

  path = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(fix$template, path, source_path = fix$qmd)

  raw = yaml::read_yaml(path)$questions[[1]]$filters[[1]]
  expect_true(isTRUE(raw$negate))
  expect_null(raw$conditions[[1]]$negate)
  expect_true(isTRUE(raw$conditions[[2]]$negate))

  back = markermd::read_template_yaml(path, require_ast = TRUE)
  group = back@questions[[1]]@filters[[1]]
  expect_true(group@negate)
  expect_false(group@conditions[[1]]@negate)
  expect_true(group@conditions[[2]]@negate)

  skip_if_not_installed("jsonvalidate")
  expect_true(isTRUE(markermd::validate_template_file(path)))
})

test_that("hand-authored filters load and bad condition types are rejected", {
  fix = build_serialization_fixture()
  src = sprintf('source: {path: "%s"}', fix$qmd)

  path = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules: []",
    "  filters:",
    "  - conditions:",
    "    - {type: has class, value: hint}"
  ))
  tmpl = markermd::read_template_yaml(path, require_ast = TRUE)
  expect_length(tmpl@questions[[1]]@filters, 1)
  expect_equal(tmpl@questions[[1]]@filters[[1]]@conditions[[1]]@value, "hint")

  bad = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules: []",
    "  filters:",
    "  - conditions:",
    "    - {type: has bananas, value: x}"
  ))
  expect_error(markermd::read_template_yaml(bad), "Filter condition type must be one of")
})

test_that("validate_template_file accepts filters and rejects malformed ones", {
  skip_if_not_installed("jsonvalidate")
  fix = build_serialization_fixture()
  src = sprintf('source: {path: "%s"}', fix$qmd)

  good = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules: []",
    "  filters:",
    "  - conditions:",
    "    - {type: node type, value: Div}",
    "    - {type: has class, value: hint}",
    "    - {type: has option, value: 'eval: false'}"
  ))
  expect_true(isTRUE(markermd::validate_template_file(good)))

  bad_type = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules: []",
    "  filters:",
    "  - conditions:",
    "    - {type: has bananas, value: x}"
  ))
  expect_false(isTRUE(markermd::validate_template_file(bad_type)))

  # 'node type' conditions must use a real node kind ('Any node' excluded)
  bad_kind = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules: []",
    "  filters:",
    "  - conditions:",
    "    - {type: node type, value: Any node}"
  ))
  expect_false(isTRUE(markermd::validate_template_file(bad_kind)))

  empty_conditions = write_yaml_lines(c(
    'format_version: "3.0"', src, "questions:",
    "- id: 1",
    "  name: Q2",
    "  node_ids: [question-2-basic-programming]",
    "  rules: []",
    "  filters:",
    "  - conditions: []"
  ))
  expect_false(isTRUE(markermd::validate_template_file(empty_conditions)))
})


test_that("save_template_to_db -> load_template_from_db round-trips questions and rules", {
  fix = build_serialization_fixture()
  d = tempfile("tmpldb_"); dir.create(d)

  expect_null(markermd:::load_template_from_db(d))

  markermd:::save_template_to_db(d, fix$template, source_path = fix$qmd)
  back = markermd:::load_template_from_db(d, base_dir = d, assignment = fix$qmd, require_ast = TRUE)

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


test_that("database round-trip preserves polymorphic value shapes and array-valued node_ids", {
  fix = build_serialization_fixture()
  d = tempfile("tmpldb_"); dir.create(d)
  markermd:::save_template_to_db(d, fix$template, source_path = fix$qmd)
  back = markermd:::load_template_from_db(d, base_dir = d, assignment = fix$qmd, require_ast = TRUE)

  between = back@questions[[1]]@rules[[3]]
  expect_equal(between@verb, "has between")
  expect_length(between@values, 2)
  expect_equal(between@values, c(1, 3))

  count = back@questions[[1]]@rules[[2]]
  expect_true(is.integer(count@values))
  expect_equal(count@values, 1L)

  pattern = back@questions[[1]]@rules[[1]]
  expect_true(is.character(pattern@values))
  expect_equal(pattern@values, "*quantile*")

  multi = back@questions[[2]]@rules[[4]]
  expect_equal(multi@node_type, c("Markdown", "Raw Block"))

  # a single node_id survives as a length-1 character vector (JSON array, not a
  # collapsed scalar) -- the fromJSON(simplifyVector = FALSE) shape assumption.
  expect_equal(back@questions[[1]]@selected_nodes@node_ids, "question-2-basic-programming")

  # the recorded source path round-trips so the assignment can be relocated
  expect_equal(attr(back, "markermd_source_raw"), fix$qmd)
})


test_that("load_template_from_db errors on missing format_version and honours require_ast", {
  d = tempfile("tmpldb_"); dir.create(d)
  markermd:::with_database(d, function(conn) markermd:::set_metadata(conn, "template", "{}"))
  expect_error(markermd:::load_template_from_db(d), "format_version")

  fix = build_serialization_fixture()
  d2 = tempfile("tmpldb_"); dir.create(d2)
  markermd:::save_template_to_db(d2, fix$template, source_path = "does-not-exist.qmd")

  back = markermd:::load_template_from_db(d2)
  expect_length(back@questions, 2)

  expect_error(
    markermd:::load_template_from_db(d2, require_ast = TRUE),
    "assignment document"
  )
})
