# Mark Scores Module
#
# Shiny module for the Scores tab: one interactive table of each repository's
# per-question scores and total, with the per-question summary statistics
# pinned beneath as footer rows, recomputed from the grading database. The
# data and table builders live in utils_mark_scores.R.

# Mark Scores UI
#
# id: Character. Module namespace ID

mark_scores_ui = function(id) {
  ns = shiny::NS(id)

  bslib::card(
    class = "h-100",
    bslib::card_header(class = "bg-light", "Scores"),
    bslib::card_body(
      class = "p-2",
      # Clickable cells (question scores and repository names) read as links
      # on hover; the click itself is handled by the table's onClick
      shiny::tags$style(shiny::HTML("
        .score-link { cursor: pointer; }
        .score-link:hover { text-decoration: underline; }
      ")),
      shiny::uiOutput(ns("note")),
      reactable::reactableOutput(ns("scores_table")),
      shiny::uiOutput(ns("caption"), class = "small text-muted mt-2")
    )
  )
}

# Mark Scores Server
#
# id: Character. Module namespace ID
# root: Project root holding the grading database
# template_obj: markermd_template object whose questions are the columns, or
#   NULL when the project has no template
# repo_list: Character vector of repository names, in display order
# grading_data_version: Reactive bumped by the rubric module after every
#   grading write, so the table re-reads the database only then
# refresh: Optional reactive whose invalidation also triggers a recompute. The
#   app passes the navbar selection so opening the tab picks up a question
#   total edited in the rubric pane, which does not bump the version.
# Returns: List with table (reactive of the current scores table data) and
#   score_clicked (reactive of the last clicked cell as a list with repo and
#   question, the latter NULL for a click on the repository name; an event,
#   so the same cell clicked twice fires twice)

mark_scores_server = function(id, root, template_obj, repo_list, grading_data_version, refresh = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    question_names = if (is.null(template_obj)) character(0) else template_question_names(template_obj)
    has_data = length(question_names) > 0 && length(repo_list) > 0

    # The table and caption read this once per invalidation; the outputs are
    # suspended while the tab is hidden, so a hidden tab costs nothing
    scores = shiny::reactive({
      grading_data_version()
      if (!is.null(refresh)) {
        refresh()
      }

      totals = question_total_scores(root, question_names)
      table_data = build_scores_table_data(
        collect_score_data(root, question_names, repo_list),
        question_names,
        repo_list
      )
      list(
        totals = totals,
        table = table_data,
        summary = build_scores_summary_rows(table_data, question_names)
      )
    })

    output$note = shiny::renderUI({
      if (length(question_names) == 0) {
        shiny::p(
          "No questions to score: store a grading template first.",
          class = "text-muted fst-italic m-2"
        )
      } else if (length(repo_list) == 0) {
        shiny::p("No repositories to score.", class = "text-muted fst-italic m-2")
      }
    })

    output$scores_table = reactable::renderReactable({
      shiny::req(has_data)
      scores_reactable(
        scores()$table, scores()$summary, scores()$totals,
        click_input = session$ns("score_clicked")
      )
    })

    output$caption = shiny::renderUI({
      shiny::req(has_data)
      scores_caption(scores()$summary, length(repo_list))
    })

    # For shinytest2 assertions (namespaced under this module's id)
    shiny::exportTestValues(
      scores_table = scores()$table,
      scores_summary = scores()$summary
    )

    list(
      table = shiny::reactive(scores()$table),
      score_clicked = shiny::reactive(input$score_clicked)
    )
  })
}
