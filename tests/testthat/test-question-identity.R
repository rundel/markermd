# Question identity across template imports, regenerations and restores
# (the 2026-11 follow-up review's Phase 2: grading follows question ids)

sn_template_file = function(d, file, questions) {
  lines = c(
    paste0("format_version: '", markermd:::markermd_template_version(), "'"),
    "source:", "  path: hw1.qmd", "questions:"
  )
  for (q in questions) {
    lines = c(lines, paste0("  - id: ", q$id), paste0("    name: ", q$name))
  }
  path = file.path(d, file)
  writeLines(lines, path)
  path
}

test_that("swapping two question names in one import swaps their grading", {
  d = sn_graded_project()
  sn_template_file(d, "swap.yaml", list(list(id = 1, name = "Q2"), list(id = 2, name = "Q1")))

  expect_warning(suppressMessages(template_import("swap.yaml", project = d)), "Carried the grading")
  # Question id 1 (formerly Q1) keeps its four items under its new name
  expect_length(markermd:::load_rubric_items(d, "Q2"), 4)
  expect_setequal(sn_selected(d, "Q2", "hw01-team01"), c("Issue A", "Minor"))
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), "Full credit")
})

test_that("a rename onto a removed question's leftover grading is refused", {
  d = sn_graded_project()
  # Q2 is removed (its grading stays) and Q1 is renamed onto "Q2"
  sn_template_file(d, "onto.yaml", list(list(id = 1, name = "Q2")))
  expect_error(suppressMessages(template_import("onto.yaml", project = d)), "already hold")
  expect_equal(markermd:::project_question_names(d)$names, c("Q1", "Q2"))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)
})

test_that("restoring the template undoes a regeneration that shifted the ids", {
  d = sn_graded_project()
  # A regenerated template inserts a question at id 2, shifting Q2 to id 3
  sn_template_file(d, "regen.yaml", list(
    list(id = 1, name = "Q1"), list(id = 2, name = "Q1b"), list(id = 3, name = "Q2")
  ))
  expect_warning(suppressMessages(template_import("regen.yaml", project = d)), "regenerated")
  # Grading followed the ids: Q2's rows now sit under Q1b
  expect_setequal(sn_selected(d, "Q1b", "hw01-team01"), "Full credit")

  expect_warning(
    suppressMessages(snapshot_restore("latest", project = d, part = "template")),
    "Carried the grading"
  )
  expect_equal(markermd:::project_question_names(d)$names, c("Q1", "Q2"))
  expect_setequal(sn_selected(d, "Q2", "hw01-team01"), "Full credit")
  expect_length(markermd:::load_rubric_items(d, "Q1b"), 0)
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
})

test_that("question_node_id_problems lists unknown and nested selections", {
  path = tempfile(fileext = ".qmd")
  writeLines(c("# A {#a}", "", "text", "", "## B {#b}", "", "more", "", "# C {#c}", "", "end"), path)
  ast = markermd:::parse_assignment_document(path)
  q = function(name, ids) markermd_question(
    id = 1L, name = name, selected_nodes = markermd_node_selection(node_ids = ids)
  )
  problems = markermd:::question_node_id_problems(ast, list(
    q("fine", c("a", "c")), q("stray", c("a", "zzz")), q("nested", c("a", "b"))
  ))
  expect_length(problems, 2)
  expect_match(problems[1], "\"stray\".*not in the document: #zzz")
  expect_match(problems[2], "\"nested\".*inside another selected section: #b")
})

test_that("the rename guard refuses taken and blank names and trims the rest", {
  path = tempfile(fileext = ".qmd")
  writeLines(c("# A {#a}", "", "text"), path)
  ast_obj = markermd:::parse_assignment_document(path)
  taken = c("Q2")
  shiny::testServer(
    markermd:::question_server,
    args = list(
      ast = shiny::reactive(ast_obj),
      initial_question = markermd_question(id = 1L, name = "Q1"),
      name_available = function(name) !name %in% taken
    ),
    {
      # The mock session's sendCustomMessage (the field reset) is a no-op
      suppressWarnings({
        session$setInputs(question_name = "Q2")
        expect_equal(state()@name, "Q1")
        session$setInputs(question_name = "   ")
        expect_equal(state()@name, "Q1")
      })
      session$setInputs(question_name = "  Question one  ")
      expect_equal(state()@name, "Question one")
    }
  )
})

test_that("destroying a question server stops its observers", {
  path = tempfile(fileext = ".qmd")
  writeLines(c("# A {#a}", "", "text"), path)
  ast_obj = markermd:::parse_assignment_document(path)
  shiny::testServer(
    markermd:::question_server,
    args = list(ast = shiny::reactive(ast_obj), initial_question = markermd_question(id = 1L, name = "Q1")),
    {
      srv = session$returned
      srv$destroy()
      session$setInputs(question_name = "Renamed")
      expect_equal(state()@name, "Q1")
    }
  )
})
