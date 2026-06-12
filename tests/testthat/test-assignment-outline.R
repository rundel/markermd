test_that("assignment_outline reports section anchors and content counts", {
  dir = system.file("examples/test_assignment/repos/student1-excellent", package = "markermd")
  out = markermd::assignment_outline(dir)

  expect_s3_class(out, "data.frame")
  expect_true(all(c(
    "type", "id", "level", "title", "n_subsections",
    "n_chunk", "n_code_block", "n_markdown", "n_list", "n_table",
    "n_div", "n_other", "n_content"
  ) %in% names(out)))

  # The real Pandoc heading anchors are present and usable as node_ids
  expect_true("question-2-basic-programming" %in% out$id)
  expect_true("question-3-data-visualization" %in% out$id)

  q2 = out[out$id == "question-2-basic-programming", ]
  expect_equal(q2$type, "heading")
  expect_equal(q2$level, 2L)
  expect_gte(q2$n_chunk, 1L)
  expect_gt(q2$n_content, 0L)

  expect_match(attr(out, "source_file"), "assignment\\.qmd$")
})

test_that("assignment_outline accepts a file path and includes id'd divs", {
  file = system.file("examples/div_assignment/assignment.qmd", package = "markermd")
  out = markermd::assignment_outline(file)

  expect_gt(nrow(out), 0)
  expect_true("div" %in% out$type)

  div = out[out$type == "div", ][1, ]
  expect_true(is.na(div$level))
  expect_true(nzchar(div$id))
})

test_that("the anchors from assignment_outline resolve in a written template", {
  file = system.file("examples/test_assignment/repos/student1-excellent/assignment.qmd", package = "markermd")
  out = markermd::assignment_outline(file)
  anchor = out$id[out$id == "question-2-basic-programming"]

  template = markermd::markermd_template(
    original_ast = markermd:::parse_assignment_document(file),
    questions = list(
      markermd::markermd_question(
        1L, "Q2",
        markermd::markermd_node_selection(node_ids = anchor),
        list(markermd::markermd_rule("Chunk", "has at least", 1L))
      )
    ),
    metadata = markermd::markermd_metadata()
  )

  path = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(template, path, source_path = file)
  back = markermd::read_template_yaml(path, require_ast = TRUE)
  expect_equal(back@questions[[1]]@selected_nodes@node_ids, anchor)
})
