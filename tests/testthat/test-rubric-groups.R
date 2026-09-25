# Rubric item groups: the pure helpers (R/utils_rubric_groups.R), the shared
# score_question() (R/utils_mark_scores.R), the S7 class, the format 1.1
# serialization, the schema v3 database layer, the group-aware rubric
# import/export, the marks exclusivity check, and the exports.

rg_item = function(points, description, group = NA_character_, hotkey = NA_integer_) {
  markermd_rubric_item(hotkey = hotkey, points = points, description = description, group = group)
}

# Q3a of the plan's worked example: 12 points, negative mode, a capped
# "Sample game" group, a capped "Documentation" group and an exclusive
# "Write-up" tier
rg_items = function() {
  list(
    looks_good = rg_item(0, "Looks good"),
    rank = rg_item(-1.5, "rank() compares with > instead of >="),
    sg_a = rg_item(-1, "Sample game does not call every method", "sample"),
    sg_b = rg_item(-1, "Sample game does not show progression", "sample"),
    sg_c = rg_item(-0.5, "Sample game has no invalid guess", "sample"),
    doc_a = rg_item(-1, "Class docstring is the placeholder", "docs"),
    doc_b = rg_item(-0.5, "Docstrings are incomplete", "docs"),
    wu_a = rg_item(-1.5, "Write-up is inside an HTML comment", "writeup"),
    wu_b = rg_item(-1, "Write-up is inaccurate", "writeup")
  )
}

rg_groups = function() {
  list(
    sample = markermd_rubric_group(label = "Sample game", floor = -2),
    docs = markermd_rubric_group(label = "Documentation", floor = -1),
    writeup = markermd_rubric_group(label = "Write-up", exclusive = TRUE)
  )
}

rg_state = function(mode = "negative", total = 12, above = TRUE, below = TRUE) {
  markermd_grade_state(
    current_score = if (mode == "negative") total else 0, total_score = total,
    grading_mode = mode, bound_above_zero = above, bound_below_max = below
  )
}

# Builds an initialized project whose database stores a two-question
# template (Q1, Q2), with two student repo directories
rg_project = function() {
  d = tempfile("markproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  dir.create(file.path(d, "repos", "hw01-team02"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")

  template_path = file.path(d, "template.yaml")
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: Q1",
    "  - id: 2",
    "    name: Q2"
  ), template_path)

  suppressMessages(init_project(d))
  suppressMessages(template_import(template_path, project = d))
  d
}

rg_write_yaml = function(lines) {
  path = tempfile(fileext = ".yaml")
  writeLines(lines, path)
  path
}

# The worked example as a format 1.1 rubric file for Q1
rg_grouped_yaml = function(question = "Q1", exclusive = TRUE) {
  rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    paste0("- name: ", question),
    "  scoring:",
    "    total_score: 12",
    "    grading_mode: negative",
    "  items:",
    "  - points: 0",
    "    description: Looks good",
    "  - points: -1.5",
    "    description: Wrong comparison",
    "  - group: Sample game",
    "    floor: -2",
    "    items:",
    "    - points: -1",
    "      description: Sample A",
    "    - points: -1",
    "      description: Sample B",
    "    - points: -0.5",
    "      description: Sample C",
    "  - group: Write-up",
    if (exclusive) "    exclusive: true",
    "    items:",
    "    - points: -1.5",
    "      description: Write-up absent",
    "    - points: -1",
    "      description: Write-up partial"
  ))
}

rg_descriptions = function(items) unname(vapply(items, function(item) item@description, character(1)))
rg_item_groups = function(items) unname(vapply(items, function(item) item@group, character(1)))


# --- score_question() -------------------------------------------------------

test_that("score_question applies group floors before the question bounds in negative mode", {
  items = rg_items()
  groups = rg_groups()

  # Nothing selected: every group contributes exactly zero
  res = markermd:::score_question(items, character(0), groups, rg_state())
  expect_equal(res$score, 12)
  expect_equal(res$without_groups, 12)
  expect_equal(res$groups$applied, c(0, 0, 0))

  # The plan's example: -1.5 ungrouped, sample -2.5 floored to -2,
  # docs -1.5 floored to -1, write-up -1
  selected = c("rank", "sg_a", "sg_b", "sg_c", "doc_a", "doc_b", "wu_b")
  res = markermd:::score_question(items, selected, groups, rg_state())
  expect_equal(res$base, 12)
  expect_equal(res$ungrouped, -1.5)
  expect_equal(res$groups$group_id, c("sample", "docs", "writeup"))
  expect_equal(res$groups$label, c("Sample game", "Documentation", "Write-up"))
  expect_equal(res$groups$sum, c(-2.5, -1.5, -1))
  expect_equal(res$groups$applied, c(-2, -1, -1))
  expect_equal(res$raw, 6.5)
  expect_equal(res$score, 6.5)
  expect_equal(res$without_groups, 5.5)
})

test_that("score_question applies ceilings in positive mode and keeps the question bounds", {
  items = list(
    a = rg_item(3, "A", "demo"), b = rg_item(3, "B", "demo"), c = rg_item(5, "C")
  )
  groups = list(demo = markermd_rubric_group(label = "Demonstrations", ceiling = 4))

  res = markermd:::score_question(items, c("a", "b"), groups, rg_state("positive", 10))
  expect_equal(res$groups$sum, 6)
  expect_equal(res$groups$applied, 4)
  expect_equal(res$score, 4)
  expect_equal(res$without_groups, 6)

  # The question's upper bound still applies after the ceiling
  res = markermd:::score_question(items, c("a", "b", "c"), groups, rg_state("positive", 8))
  expect_equal(res$raw, 9)
  expect_equal(res$score, 8)
  expect_equal(res$without_groups, 8)

  # ... unless it is off
  res = markermd:::score_question(items, c("a", "b", "c"), groups, rg_state("positive", 8, below = FALSE))
  expect_equal(res$score, 9)
  expect_equal(res$without_groups, 11)
})

test_that("score_question handles both bounds, empty and unknown groups", {
  items = list(
    a = rg_item(-3, "A", "g"), b = rg_item(4, "B", "g"), c = rg_item(2, "C", "gone")
  )
  groups = list(
    g = markermd_rubric_group(label = "Both", floor = -2, ceiling = 1),
    empty = markermd_rubric_group(label = "Empty", floor = -5)
  )
  state = rg_state("positive", 10)

  res = markermd:::score_question(items, "a", groups, state)
  expect_equal(res$groups$applied, c(-2, 0))
  expect_equal(res$score, 0)

  res = markermd:::score_question(items, "b", groups, state)
  expect_equal(res$groups$applied[1], 1)

  # an item whose group id is unknown counts as ungrouped
  res = markermd:::score_question(items, c("b", "c"), groups, state)
  expect_equal(res$ungrouped, 2)
  expect_equal(res$score, 3)

  # no groups at all, and no items at all
  res = markermd:::score_question(items, "c", list(), state)
  expect_equal(nrow(res$groups), 0)
  expect_equal(res$score, 2)
  res = markermd:::score_question(list(), character(0), list(), rg_state())
  expect_equal(res$score, 12)
})

test_that("score_question matches the pre-group formula for a group-less rubric", {
  items = list(a = rg_item(-2, "A"), b = rg_item(-1, "B"), c = rg_item(20, "C"))
  expect_equal(markermd:::score_question(items, "a", list(), rg_state())$score, 10)
  expect_equal(markermd:::score_question(items, c("a", "b"), list(), rg_state("negative", 2))$score, 0)
  expect_equal(markermd:::score_question(items, c("a", "b"), list(), rg_state("negative", 2, above = FALSE))$score, -1)
  expect_equal(markermd:::score_question(items, "c", list(), rg_state("positive", 10))$score, 10)
  expect_equal(markermd:::score_question(items, "c", list(), rg_state("positive", 10, below = FALSE))$score, 20)
})


# --- Order, blocks and moves ------------------------------------------------

test_that("group_contiguous_order pulls a group's members together at its first member", {
  expect_equal(markermd:::group_contiguous_order(c(NA, "g", NA, "g", "h", NA, "h")), c(1, 2, 4, 3, 5, 7, 6))
  expect_equal(markermd:::group_contiguous_order(c("g", "g", NA)), 1:3)
  expect_equal(markermd:::group_contiguous_order(character(0)), integer(0))

  items = rg_items()[c("sg_a", "looks_good", "sg_b", "doc_a", "rank", "doc_b")]
  expect_equal(
    names(markermd:::normalize_item_order(items)),
    c("sg_a", "sg_b", "looks_good", "doc_a", "doc_b", "rank")
  )
  expect_identical(markermd:::normalize_item_order(list()), list())
})

test_that("rubric_blocks splits the order into item and group blocks, empty groups last", {
  ids = c("a", "b", "c", "d", "e")
  group_of = c(NA, "g1", "g1", NA, "g2")
  blocks = markermd:::rubric_blocks(ids, group_of, c("g1", "g2", "g3"))

  expect_equal(vapply(blocks, function(b) b$type, character(1)), c("item", "group", "item", "group", "group"))
  expect_equal(blocks[[2]]$group, "g1")
  expect_equal(blocks[[2]]$ids, c("b", "c"))
  expect_equal(blocks[[4]]$ids, "e")
  expect_equal(blocks[[5]]$group, "g3")
  expect_length(blocks[[5]]$ids, 0)
  expect_equal(markermd:::blocks_to_ids(blocks), ids)

  expect_length(markermd:::rubric_blocks(character(0), character(0)), 0)
  expect_length(markermd:::rubric_blocks(character(0), character(0), "g"), 1)
})

test_that("move_in_blocks moves items within their run and groups as whole blocks", {
  ids = c("a", "b", "c", "d", "e")
  group_of = c(NA, "g1", "g1", NA, "g2")
  move = function(id, direction) markermd:::move_in_blocks(ids, group_of, id, direction)

  # an ungrouped item jumps over a whole group rather than entering it
  expect_equal(move("a", 1), c("b", "c", "a", "d", "e"))
  # ... and wraps from the top to the end
  expect_equal(move("a", -1), c("b", "c", "d", "e", "a"))
  # a grouped item moves within its group, wrapping within it
  expect_equal(move("c", -1), c("a", "c", "b", "d", "e"))
  expect_equal(move("b", -1), c("a", "c", "b", "d", "e"))
  expect_equal(move("c", 1), c("a", "c", "b", "d", "e"))
  # a group moves as a block among the top-level blocks
  expect_equal(move("g1", 1), c("a", "d", "b", "c", "e"))
  expect_equal(move("g1", -1), c("b", "c", "a", "d", "e"))
  expect_equal(move("g2", 1), c("e", "a", "b", "c", "d"))
  # a single-member group moves as a block, and nothing moves in a single-item group
  expect_equal(move("e", 1), ids)
  # unknown ids and empty groups are no-ops
  expect_equal(move("zzz", 1), ids)
  expect_equal(markermd:::move_in_blocks("a", NA_character_, "a", 1), "a")
})

test_that("move_item_to_group appends to the target run and leaves a group in place", {
  ids = c("a", "b", "c", "d", "e")
  group_of = c(NA, "g1", "g1", NA, "g2")

  # into a group with members: after its last member
  moved = markermd:::move_item_to_group(ids, group_of, "a", "g2")
  expect_equal(moved$ids, c("b", "c", "d", "e", "a"))
  expect_equal(unname(moved$group_of), c("g1", "g1", NA, "g2", "g2"))
  expect_equal(names(moved$group_of), moved$ids)

  # into a group with no members yet: at the end of the list
  moved = markermd:::move_item_to_group(ids, group_of, "b", "g9")
  expect_equal(moved$ids, c("a", "c", "d", "e", "b"))
  expect_equal(unname(moved$group_of), c(NA, "g1", NA, "g2", "g9"))

  # out of a group: right after the group it left
  moved = markermd:::move_item_to_group(ids, group_of, "b", NA)
  expect_equal(moved$ids, c("a", "c", "b", "d", "e"))
  expect_equal(unname(moved$group_of), c(NA, "g1", NA, NA, "g2"))

  # out of a group it was the only member of: stays put
  moved = markermd:::move_item_to_group(ids, group_of, "e", NA)
  expect_equal(moved$ids, ids)
  expect_true(all(is.na(moved$group_of[c("a", "d", "e")])))

  # no-ops
  expect_equal(markermd:::move_item_to_group(ids, group_of, "a", NA)$ids, ids)
  expect_equal(markermd:::move_item_to_group(ids, group_of, "b", "g1")$ids, ids)
})


# --- The S7 class -----------------------------------------------------------

test_that("markermd_rubric_group enforces the sign rules and a label", {
  g = markermd_rubric_group(label = "Examples", floor = -2)
  expect_equal(g@floor, -2)
  expect_true(is.na(g@ceiling))
  expect_false(g@exclusive)

  expect_error(markermd_rubric_group(label = "X", floor = 1), "floor must be less than or equal to 0")
  expect_error(markermd_rubric_group(label = "X", ceiling = -1), "ceiling must be greater than or equal to 0")
  expect_error(markermd_rubric_group(label = " "), "label cannot be empty")
  expect_error(markermd_rubric_group(label = "X", exclusive = NA), "exclusive must be TRUE or FALSE")

  expect_error(rg_item(1, "A", ""), "group must be a non-empty string or NA")
  expect_true(is.na(rg_item(1, "A")@group))
})


# --- YAML format 1.1 --------------------------------------------------------

test_that("a grouped rubric round-trips through YAML as format 1.1 with the nesting rebuilt", {
  rubric = list(
    format_version = "1.1",
    questions = list(list(
      name = "Q3a", scoring = rg_state(), items = unname(rg_items()), groups = rg_groups()
    ))
  )
  path = tempfile(fileext = ".yaml")
  write_rubric_yaml(rubric, path)

  parsed = yaml::read_yaml(path)
  expect_equal(parsed$format_version, "1.1")
  entries = parsed$questions[[1]]$items
  expect_length(entries, 5)
  expect_equal(entries[[3]]$group, "Sample game")
  expect_equal(entries[[3]]$floor, -2)
  expect_null(entries[[3]]$ceiling)
  expect_null(entries[[3]]$exclusive)
  expect_length(entries[[3]]$items, 3)
  expect_true(entries[[5]]$exclusive)
  expect_null(entries[[5]]$floor)
  expect_false(any(grepl("group_id|hotkey", readLines(path))))

  back = read_rubric_yaml(path)
  expect_equal(back$format_version, "1.1")
  q = back$questions[[1]]
  expect_equal(rg_descriptions(q$items), rg_descriptions(rg_items()))
  # hotkeys follow the flat position, across group boundaries
  expect_equal(unname(vapply(q$items, function(i) i@hotkey, integer(1))), 1:9)
  # the groups come back with transient ids, in file order
  expect_equal(names(q$groups), c("group_1", "group_2", "group_3"))
  expect_equal(rg_item_groups(q$items), c(NA, NA, "group_1", "group_1", "group_1", "group_2", "group_2", "group_3", "group_3"))
  expect_equal(q$groups$group_1@label, "Sample game")
  expect_equal(q$groups$group_1@floor, -2)
  expect_true(q$groups$group_3@exclusive)
  expect_true(is.na(q$groups$group_3@floor))

  # writing it again gives the same file
  path2 = tempfile(fileext = ".yaml")
  write_rubric_yaml(back, path2)
  expect_equal(readLines(path2), readLines(path))
})

test_that("a group with no items is not written, and an empty question still writes items: []", {
  rubric = list(questions = list(list(
    name = "Q1", scoring = NULL,
    items = list(rg_item(-1, "A")),
    groups = list(g = markermd_rubric_group(label = "Unused"))
  )))
  path = tempfile(fileext = ".yaml")
  write_rubric_yaml(rubric, path)
  expect_equal(yaml::read_yaml(path)$format_version, "1.0")
  expect_false(any(grepl("Unused", readLines(path))))
})

test_that("read_rubric_yaml rejects nested groups, duplicate labels, bad bounds and non-contiguous writes", {
  nested = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - group: Outer",
    "    items:",
    "    - group: Inner",
    "      items: []"
  ))
  expect_error(read_rubric_yaml(nested), "nested group")

  dupes = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - group: Same",
    "    items: []",
    "  - group: Same",
    "    items: []"
  ))
  expect_error(read_rubric_yaml(dupes), "duplicate group labels")

  bad_floor = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - group: G",
    "    floor: 2",
    "    items: []"
  ))
  expect_error(read_rubric_yaml(bad_floor), "floor must be")

  unlabelled = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - group: ''",
    "    items: []"
  ))
  expect_error(read_rubric_yaml(unlabelled), "non-empty 'group' label")

  # descriptions stay unique across groups
  cross = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - points: -1",
    "    description: Same",
    "  - group: G",
    "    items:",
    "    - points: -1",
    "      description: Same"
  ))
  expect_error(read_rubric_yaml(cross), "duplicate rubric item descriptions")

  # a file newer than this markermd understands is refused
  too_new = rg_write_yaml(c("format_version: '9.9'", "questions: []"))
  expect_error(read_rubric_yaml(too_new), "newer")

  # the writer refuses a group whose items are not contiguous, or unknown
  items = rg_items()[c("sg_a", "rank", "sg_b")]
  expect_error(
    write_rubric_yaml(list(questions = list(list(name = "Q", items = unname(items), groups = rg_groups()))), tempfile()),
    "not contiguous"
  )
  expect_error(
    write_rubric_yaml(list(questions = list(list(name = "Q", items = unname(items), groups = list()))), tempfile()),
    "unknown group"
  )
  expect_error(
    write_rubric_yaml(list(questions = list(list(name = "Q", items = list(), groups = list("x")))), tempfile()),
    "markermd_rubric_group"
  )
})

test_that("validate_rubric_file accepts grouped files and rejects nested groups or a positive floor", {
  skip_if_not_installed("jsonvalidate")

  expect_true(validate_rubric_file(rg_grouped_yaml()))

  nested = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - group: Outer",
    "    items:",
    "    - group: Inner",
    "      items: []"
  ))
  expect_false(validate_rubric_file(nested))

  bad_floor = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - group: G",
    "    floor: 2",
    "    items: []"
  ))
  expect_false(validate_rubric_file(bad_floor))

  extra = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - group: G",
    "    weight: 2",
    "    items: []"
  ))
  expect_false(validate_rubric_file(extra))
})


# --- Database ---------------------------------------------------------------

test_that("item groups are saved, ordered by their first member, and deleted in place", {
  dir = tempfile("groupdb")
  dir.create(dir)

  markermd:::save_item_group(dir, "Q1", "group_0", markermd_rubric_group(label = "Later", ceiling = 3))
  markermd:::save_item_group(dir, "Q1", "group_1", markermd_rubric_group(label = "Earlier", floor = -1, exclusive = TRUE))
  markermd:::save_item_group(dir, "Q1", "group_2", markermd_rubric_group(label = "Empty"))
  markermd:::save_rubric_items(dir, "Q1", list(
    a = rg_item(-1, "a"),
    b = rg_item(-1, "b", "group_1"),
    c = rg_item(-1, "c", "group_0"),
    d = rg_item(-1, "d", "group_1")
  ))

  # the write normalized the order: group_1's members are contiguous
  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, c("a", "b", "d", "c"))
  expect_equal(rg_item_groups(items), c(NA, "group_1", "group_1", "group_0"))
  expect_equal(unname(vapply(items, function(i) i@hotkey, integer(1))), 1:4)

  groups = markermd:::load_item_groups(dir, "Q1")
  expect_named(groups, c("group_1", "group_0", "group_2"))
  expect_equal(groups$group_1@label, "Earlier")
  expect_equal(groups$group_1@floor, -1)
  expect_true(groups$group_1@exclusive)
  expect_equal(groups$group_0@ceiling, 3)
  expect_true(is.na(groups$group_0@floor))

  # an update keeps the id and changes the settings
  markermd:::save_item_group(dir, "Q1", "group_0", markermd_rubric_group(label = "Renamed", ceiling = 1))
  groups = markermd:::load_item_groups(dir, "Q1")
  expect_equal(groups$group_0@label, "Renamed")
  expect_equal(groups$group_0@ceiling, 1)

  # a label already used by another group of the question is refused
  expect_error(
    markermd:::save_item_group(dir, "Q1", "group_2", markermd_rubric_group(label = "Renamed")),
    "UNIQUE"
  )

  # deleting a group ungroups its members where they are
  markermd:::delete_item_group(dir, "Q1", "group_1")
  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, c("a", "b", "d", "c"))
  expect_equal(rg_item_groups(items), c(NA, NA, NA, "group_0"))
  expect_named(markermd:::load_item_groups(dir, "Q1"), c("group_0", "group_2"))

  # a single-item edit keeps the item's group
  markermd:::save_rubric_item(dir, "Q1", "c", rg_item(-5, "c edited", "group_0"))
  expect_equal(markermd:::load_rubric_items(dir, "Q1")$c@group, "group_0")

  # the initial app state carries the groups too
  state = markermd:::initialize_database_state(dir, mt_template_for_groups(c("Q1", "Q2")))
  expect_named(state$rubric_groups$Q1, c("group_0", "group_2"))
  expect_length(state$rubric_groups$Q2, 0)
})

test_that("group_selection_conflicts lists repositories with more than one member selected", {
  dir = tempfile("groupconf")
  dir.create(dir)
  markermd:::save_rubric_items(dir, "Q1", list(a = rg_item(-1, "a", "g"), b = rg_item(-1, "b", "g"), c = rg_item(-1, "c")))
  markermd:::save_grade_selection(dir, "Q1", "repoA", "a", TRUE)
  markermd:::save_grade_selection(dir, "Q1", "repoA", "b", TRUE)
  markermd:::save_grade_selection(dir, "Q1", "repoB", "a", TRUE)
  markermd:::save_grade_selection(dir, "Q1", "repoB", "b", TRUE)
  markermd:::save_grade_selection(dir, "Q1", "repoB", "b", FALSE)
  markermd:::save_grade_selection(dir, "Q1", "repoC", "a", TRUE)
  markermd:::save_grade_selection(dir, "Q1", "repoC", "c", TRUE)

  conflicts = markermd:::load_group_selection_conflicts(dir, "Q1", c("a", "b"))
  expect_equal(conflicts$assignment_repo, "repoA")
  expect_equal(conflicts$item_ids[[1]], c("a", "b"))

  expect_equal(nrow(markermd:::load_group_selection_conflicts(dir, "Q1", "a")), 0)
  expect_equal(nrow(markermd:::load_group_selection_conflicts(dir, "Q2", c("a", "b"))), 0)
})

test_that("a schema-version-2 database gains items.group_id and the item_groups table", {
  dir = tempfile("groupmigrate")
  dir.create(file.path(dir, ".markermd"), recursive = TRUE)

  conn = DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".markermd", "markermd.sqlite"))
  DBI::dbExecute(conn, "
    CREATE TABLE items (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      question_name TEXT NOT NULL,
      item_id TEXT NOT NULL,
      hotkey INTEGER,
      position INTEGER,
      points REAL NOT NULL,
      description TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      UNIQUE(question_name, item_id)
    )
  ")
  DBI::dbExecute(conn, "CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL, updated_at TEXT NOT NULL)")
  DBI::dbExecute(conn, "INSERT INTO metadata VALUES ('schema_version', '2', '2024-01-01 00:00:00')")
  DBI::dbAppendTable(conn, "items", data.frame(
    question_name = "Q1", item_id = c("item_0", "item_1"), hotkey = 1:2, position = 1:2,
    points = -1, description = c("a", "b"),
    created_at = "2024-01-01 00:00:00", updated_at = "2024-01-01 00:00:00"
  ))
  DBI::dbDisconnect(conn)

  # any markermd access migrates in place: the rubric is all-ungrouped
  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, c("item_0", "item_1"))
  expect_true(all(is.na(rg_item_groups(items))))
  expect_length(markermd:::load_item_groups(dir, "Q1"), 0)

  markermd:::with_database(dir, function(conn) {
    expect_equal(markermd:::get_metadata(conn, "schema_version"), markermd:::markermd_db_version())
    expect_true("group_id" %in% DBI::dbListFields(conn, "items"))
    expect_true(DBI::dbExistsTable(conn, "item_groups"))
  })

  # and groups work from then on
  markermd:::save_item_group(dir, "Q1", "group_0", markermd_rubric_group(label = "G", floor = -1))
  markermd:::save_rubric_items(dir, "Q1", list(item_0 = rg_item(-1, "a", "group_0"), item_1 = rg_item(-1, "b")))
  expect_equal(markermd:::load_rubric_items(dir, "Q1")$item_0@group, "group_0")
})

test_that("next_item_ids mints group ids with their own prefix", {
  expect_equal(markermd:::next_item_ids(c("item_3", "group_1"), 2, prefix = "group_"), c("group_2", "group_3"))
  expect_equal(markermd:::next_item_ids(character(0), 1, prefix = "group_"), "group_0")
})


# --- Import / export --------------------------------------------------------

test_that("rubric_import creates groups, orders their members together, and exports them back", {
  d = rg_project()
  suppressMessages(rubric_import(rg_grouped_yaml(), project = d))

  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(
    rg_descriptions(items),
    c("Looks good", "Wrong comparison", "Sample A", "Sample B", "Sample C", "Write-up absent", "Write-up partial")
  )
  groups = markermd:::load_item_groups(d, "Q1")
  expect_named(groups, c("group_0", "group_1"))
  expect_equal(rg_item_groups(items), c(NA, NA, "group_0", "group_0", "group_0", "group_1", "group_1"))
  expect_equal(groups$group_0@label, "Sample game")
  expect_equal(groups$group_0@floor, -2)
  expect_true(groups$group_1@exclusive)

  out = file.path(d, "rubric.yaml")
  rubric_export(out, project = d)
  exported = read_rubric_yaml(out)
  expect_equal(exported$format_version, "1.1")
  q1 = exported$questions[[1]]
  # a file read back carries transient group ids in file order
  expect_equal(names(q1$groups), c("group_1", "group_2"))
  expect_equal(unname(vapply(q1$groups, function(g) g@label, character(1))), c("Sample game", "Write-up"))
  expect_equal(rg_item_groups(q1$items), c(NA, NA, "group_1", "group_1", "group_1", "group_2", "group_2"))
  expect_equal(unname(vapply(q1$items, function(i) i@hotkey, integer(1))), 1:7)
  # Q2 has no groups
  expect_length(exported$questions[[2]]$groups, 0)

  # export -> replace import round trip is faithful
  suppressMessages(rubric_import(out, project = d, mode = "replace", question = "Q1"))
  again = markermd:::load_rubric_items(d, "Q1")
  expect_equal(rg_descriptions(again), rg_descriptions(items))
  regrouped = markermd:::load_item_groups(d, "Q1")
  expect_length(regrouped, 2)
  expect_equal(unname(vapply(regrouped, function(g) g@label, character(1))), c("Sample game", "Write-up"))
})

test_that("rubric_import append matches groups by label, appends to them and updates their bounds", {
  d = rg_project()
  suppressMessages(rubric_import(rg_grouped_yaml(), project = d))

  more = rg_write_yaml(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - points: -0.5",
    "    description: Debug output left in",
    "  - group: Sample game",
    "    floor: -3",
    "    items:",
    "    - points: -1",
    "      description: Sample D",
    "  - group: Style",
    "    floor: -1",
    "    items:",
    "    - points: -0.5",
    "      description: Dead code"
  ))
  suppressMessages(rubric_import(more, project = d, mode = "append"))

  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(
    rg_descriptions(items),
    c("Looks good", "Wrong comparison", "Sample A", "Sample B", "Sample C", "Sample D",
      "Write-up absent", "Write-up partial", "Debug output left in", "Dead code")
  )
  groups = markermd:::load_item_groups(d, "Q1")
  expect_named(groups, c("group_0", "group_1", "group_2"))
  expect_equal(rg_item_groups(items), c(NA, NA, rep("group_0", 4), "group_1", "group_1", NA, "group_2"))
  expect_equal(groups$group_0@floor, -3)
  expect_equal(groups$group_2@label, "Style")
  expect_equal(unname(vapply(items, function(i) i@hotkey, integer(1))), c(1:10))
})

test_that("rubric_import refuses to make a group exclusive over conflicting selections", {
  d = rg_project()
  suppressMessages(rubric_import(rg_grouped_yaml(exclusive = FALSE), project = d))
  # both write-up items selected for team01 while the group is not exclusive
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_5", TRUE)
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_6", TRUE)
  markermd:::save_grade_selection(d, "Q1", "hw01-team02", "item_5", TRUE)

  before = markermd:::with_database(d, markermd:::load_all_items)
  expect_error(
    rubric_import(rg_grouped_yaml(exclusive = TRUE), project = d, mode = "append"),
    "hw01-team01 / Q1, group \"Write-up\""
  )
  # nothing was written
  expect_equal(markermd:::with_database(d, markermd:::load_all_items)$item_id, before$item_id)
  expect_false(markermd:::load_item_groups(d, "Q1")$group_1@exclusive)

  # replace mode deletes the conflicting selections with the items (and
  # mints fresh group ids), so it goes through
  suppressMessages(rubric_import(rg_grouped_yaml(exclusive = TRUE), project = d, mode = "replace"))
  groups = markermd:::load_item_groups(d, "Q1")
  writeup = groups[[which(vapply(groups, function(g) g@label, character(1)) == "Write-up")]]
  expect_true(writeup@exclusive)
  expect_length(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), 0)
})

test_that("marks_import and marks_set reject two selected items of an exclusive group", {
  d = rg_project()
  suppressMessages(rubric_import(rg_grouped_yaml(), project = d))

  marks = rg_write_yaml(c(
    "format_version: '1.0'",
    "repos:",
    "- name: hw01-team01",
    "  questions:",
    "  - name: Q1",
    "    items:",
    "    - Write-up absent",
    "    - Write-up partial"
  ))
  expect_error(marks_import(marks, project = d), "exclusive group \"Write-up\"")
  expect_length(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), 0)

  expect_error(
    marks_set("hw01-team01", "Q1", items = c("Sample A", "Write-up absent", "Write-up partial"), project = d),
    "exclusive group"
  )

  # one of them, plus every item of a capped group, is fine
  suppressMessages(marks_set(
    "hw01-team01", "Q1",
    items = c("Sample A", "Sample B", "Sample C", "Write-up partial"), project = d
  ))
  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_true(sel$item_6)
  expect_false(sel$item_5)
})

test_that("collect_score_data and export_scores apply the group bounds and report the adjustment", {
  d = rg_project()
  suppressMessages(rubric_import(rg_grouped_yaml(), project = d))
  # team01: all three sample-game items (-2.5, floored to -2) and the
  # ungrouped -1.5 -> 12 - 1.5 - 2 = 8.5, against 8 without the floor
  for (item in c("item_1", "item_2", "item_3", "item_4")) {
    markermd:::save_grade_selection(d, "Q1", "hw01-team01", item, TRUE)
  }
  # team02: within the floor
  markermd:::save_grade_selection(d, "Q1", "hw01-team02", "item_2", TRUE)

  scores = markermd:::collect_score_data(d, c("Q1", "Q2"), c("hw01-team01", "hw01-team02"))
  expect_equal(names(scores), c("question_name", "assignment_repo", "score", "bound_adjustment"))
  team01 = scores[scores$question_name == "Q1" & scores$assignment_repo == "hw01-team01", ]
  expect_equal(team01$score, 8.5)
  expect_equal(team01$bound_adjustment, 0.5)
  team02 = scores[scores$question_name == "Q1" & scores$assignment_repo == "hw01-team02", ]
  expect_equal(team02$score, 11)
  expect_equal(team02$bound_adjustment, 0)
  expect_true(all(is.na(scores$bound_adjustment[scores$question_name == "Q2"])))

  suppressMessages(export_scores(d))
  csv = utils::read.csv(file.path(d, "scores.csv"), check.names = FALSE)
  expect_equal(csv$Q1, c(8.5, 11))

  capped = build_scores_table_data(scores, c("Q1", "Q2"), c("hw01-team01", "hw01-team02"), value = "bound_adjustment")
  expect_equal(capped$Q1, c(0.5, 0))
  expect_true(all(is.na(capped$Q2)))
})

test_that("export_comments lists items as before by default and under group headings on request", {
  d = rg_project()
  suppressMessages(rubric_import(rg_grouped_yaml(), project = d))
  for (item in c("item_1", "item_2", "item_4", "item_6")) {
    markermd:::save_grade_selection(d, "Q1", "hw01-team01", item, TRUE)
  }
  markermd:::save_comment(d, "Q1", "hw01-team01", "See the notes above.")
  markermd:::save_grade_selection(d, "Q1", "hw01-team02", "item_3", TRUE)

  suppressMessages(export_comments(d))
  expect_identical(readLines(file.path(d, "comments", "hw01-team01.md")), c(
    "## Q1",
    "",
    "- Wrong comparison",
    "- Sample A",
    "- Sample C",
    "- Write-up partial",
    "- See the notes above."
  ))
  expect_false(any(grepl("floor|Sample game", readLines(file.path(d, "comments", "hw01-team01.md")))))

  suppressMessages(export_comments(d, group_headings = TRUE))
  expect_identical(readLines(file.path(d, "comments", "hw01-team01.md")), c(
    "## Q1",
    "",
    "- Wrong comparison",
    "- See the notes above.",
    "",
    "### Sample game",
    "",
    "- Sample A",
    "- Sample C",
    "",
    "### Write-up",
    "",
    "- Write-up partial"
  ))
  # a question with only grouped items goes straight to its subheading
  expect_identical(readLines(file.path(d, "comments", "hw01-team02.md")), c(
    "## Q1",
    "",
    "### Sample game",
    "",
    "- Sample B"
  ))

  expect_error(export_comments(d, group_headings = "yes"), "TRUE")
  result = suppressMessages(export_marks(d, group_headings = TRUE))
  expect_named(result, c("scores", "comments"))
})


# --- The Assignments table marker -------------------------------------------

test_that("the Assignments table marks scores the group bounds changed", {
  fx = list(
    repo_list = c("repoA", "repoB"),
    validation_results = list(),
    repo_errors = list(),
    all_progress = c(repoA = 1L, repoB = 1L),
    question_names = "Q1",
    graded_pairs = data.frame(assignment_repo = c("repoA", "repoB"), question_name = "Q1", stringsAsFactors = FALSE)
  )
  score_data = data.frame(
    question_name = "Q1", assignment_repo = c("repoA", "repoB"),
    score = c(8.5, 11), bound_adjustment = c(0.5, 0), stringsAsFactors = FALSE
  )
  scores = build_scores_table_data(score_data, "Q1", fx$repo_list)
  capped = build_scores_table_data(score_data, "Q1", fx$repo_list, value = "bound_adjustment")

  df = markermd:::build_repo_table_data(
    visible = fx$repo_list, visible_idx = 1:2, repo_list = fx$repo_list,
    collection = list(repo = fx$repo_list), artifact_paths = list(repoA = NA, repoB = NA),
    repo_to_github = list(), validation_results = fx$validation_results,
    template_obj = mt_template_for_groups("Q1"), repo_errors = fx$repo_errors,
    all_progress = fx$all_progress, graded_pairs = fx$graded_pairs, question_names = "Q1",
    scores_table = scores, capped_table = capped
  )
  expect_equal(df$Q1, c(8.5, 11))
  expect_equal(df$capped_Q1, c(0.5, 0))

  summary = build_scores_summary_rows(scores, "Q1")
  html = as.character(htmltools::renderTags(htmltools::as.tags(
    markermd:::repo_table_reactable(df, summary, c(Q1 = 12), click_input = "score_clicked")
  ))$html)
  expect_match(html, "rubric-cap-mark")
  expect_match(html, "Group bounds applied: 0.5 pts", fixed = TRUE)
  # the adjustment column travels with the data but is hidden
  expect_match(html, '"id":"capped_Q1"[^}]*"show":false')

  # without a capped table there is no marker
  plain = markermd:::build_repo_table_data(
    visible = fx$repo_list, visible_idx = 1:2, repo_list = fx$repo_list,
    collection = list(repo = fx$repo_list), artifact_paths = list(repoA = NA, repoB = NA),
    repo_to_github = list(), validation_results = fx$validation_results,
    template_obj = mt_template_for_groups("Q1"), repo_errors = fx$repo_errors,
    all_progress = fx$all_progress, graded_pairs = fx$graded_pairs, question_names = "Q1",
    scores_table = scores
  )
  expect_false("capped_Q1" %in% names(plain))
  html = as.character(htmltools::renderTags(htmltools::as.tags(
    markermd:::repo_table_reactable(plain, summary, c(Q1 = 12))
  ))$html)
  expect_no_match(html, "rubric-cap-mark")
})

test_that("format_points_signed and the group chips and subtotal render as designed", {
  expect_equal(markermd:::format_points_signed(-2), "-2 pts")
  expect_equal(markermd:::format_points_signed(1), "+1 pt")
  expect_equal(markermd:::format_points_signed(0), "0 pts")

  chips = as.character(markermd:::rubric_group_chips(markermd_rubric_group(label = "G", floor = -2, ceiling = 3)))
  expect_match(chips, "floor -2")
  expect_match(chips, "ceiling 3")
  expect_no_match(chips, "pick one")
  chips = as.character(markermd:::rubric_group_chips(markermd_rubric_group(label = "G", exclusive = TRUE)))
  expect_match(chips, "pick one")

  capped = as.character(markermd:::rubric_group_subtotal(-2.5, -2))
  expect_match(capped, 'class="raw">-2.5 pts', fixed = TRUE)
  expect_match(capped, 'class="eff">-2 pts', fixed = TRUE)
  within = as.character(markermd:::rubric_group_subtotal(-1, -1))
  expect_no_match(within, "raw")

  ui = as.character(markermd:::mark_rubric_group_ui(
    "rubric_module-group_0", markermd_rubric_group(label = "Sample game", floor = -2),
    list(), subtotal = data.frame(sum = -2.5, applied = -2)
  ))
  expect_match(ui, "rubric-group-capped")
  expect_match(ui, "Sample game")
  expect_match(ui, "rubric_module-group_0-settings_popover")

  breakdown = as.character(markermd:::score_breakdown_ui(
    markermd:::score_question(rg_items(), c("rank", "sg_a", "sg_b", "sg_c", "wu_b"), rg_groups(), rg_state()),
    rg_state()
  ))
  expect_match(breakdown, "Start (negative mode)", fixed = TRUE)
  expect_match(breakdown, "-2.5, floor -2", fixed = TRUE)
  expect_match(breakdown, "Without group bounds: ", fixed = TRUE)
  expect_match(breakdown, "bounded to 0 to 12", fixed = TRUE)
})
