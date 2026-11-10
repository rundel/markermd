# Mark Content Pane Module
#
# Shiny module for the grading interface's left Content pane: repo selection
# and navigation, the rendered-report iframe, the source view, and the
# per-question scroll/highlight wiring.

# The DOM id of the Grade tab's Source view

source_view_id = "markermd-source-editor"

# Group consecutive line numbers into start/end ranges
#
# line_numbers: Sorted integer vector of line numbers

group_consecutive_lines = function(line_numbers) {
  if (length(line_numbers) == 0) {
    return(list())
  }

  ranges = list()
  start = line_numbers[1]
  prev = line_numbers[1]

  for (line_number in line_numbers[-1]) {
    if (line_number == prev + 1) {
      prev = line_number
    } else {
      ranges[[length(ranges) + 1]] = list(start = start, end = prev)
      start = line_number
      prev = line_number
    }
  }

  ranges[[length(ranges) + 1]] = list(start = start, end = prev)
  ranges
}

# Map a question's selected nodes to line ranges in the repo document
#
# Highlights every line of the document as written whose enclosing node-id
# chain contains one of the question's selected ids (a selected heading
# covers its whole section, nested subsections included; a selected id'd div
# covers the lines between its fences). The chains are found on the
# normalized text the parser saw (knitr chunk headers rewritten, see
# normalize_knitr_chunks()) and the ranges mapped back to the file's lines.
#
# document_lines: Character vector of the document's lines as written
# node_ids: Character vector of selected node ids (header or div ids)
# repo_ast: q2r pandoc AST of the repo document
# stop_ids: Optional ids where the question's heading sections end, the other
#   questions' selections (see other_question_heading_ids()). With stops a
#   line belongs to the question when its innermost enclosing heading is one
#   the question covers (question_covered_indices()) or it sits in a selected
#   div, so the highlight ends where grading's section does.

map_content_to_lines = function(document_lines, node_ids, repo_ast, stop_ids = NULL) {
  if (length(node_ids) == 0) {
    return(list())
  }

  normalized = normalize_knitr_chunks(document_lines)
  chains = line_node_id_chains(normalized, repo_ast)
  in_question = if (length(stop_ids) == 0) {
    function(chain) any(node_ids %in% chain)
  } else {
    records = q2r_flatten(repo_ast)
    heading_ids = vapply(records, function(r) {
      if (S7::S7_inherits(r$node, q2r::pandoc_header)) r$node@attr@id else NA_character_
    }, character(1))
    covered = question_covered_indices(repo_ast, node_ids, stop_ids)
    covered_headings = stats::na.omit(heading_ids[covered])
    all_headings = stats::na.omit(heading_ids)
    div_ids = classify_selected_ids(repo_ast, node_ids)$div_ids
    function(chain) {
      if (any(div_ids %in% chain)) {
        return(TRUE)
      }
      headings = chain[chain %in% all_headings]
      length(headings) > 0 && headings[length(headings)] %in% covered_headings
    }
  }
  matched = which(vapply(chains, in_question, logical(1)))

  map_ranges_to_original(group_consecutive_lines(matched), attr(normalized, "line_map"))
}

# The source lines of a repo's document as written, or NULL when the repo
# has no document on disk
#
# repo_name: Character. Repository name
# collection: Collection data frame (see parse_repo_documents())

repo_document_lines = function(repo_name, collection) {
  repo_rows = collection$repo == repo_name
  if (!any(repo_rows)) {
    return(NULL)
  }

  file_path = collection$path[repo_rows][1]
  if (!file.exists(file_path)) {
    return(NULL)
  }

  readLines(file_path, warn = FALSE)
}

# Source-view content for a repo: the code view HTML (with the given line
# ranges highlighted) plus the document lines, or NULL when the repo has no
# document
#
# repo_name: Character. Repository name
# collection: Collection data frame (see parse_repo_documents())
# highlight_ranges: Optional list of start/end line ranges to highlight

get_raw_document_content = function(repo_name, collection, highlight_ranges = NULL) {
  raw_content_lines = repo_document_lines(repo_name, collection)
  if (is.null(raw_content_lines)) {
    return(NULL)
  }

  document_path = collection$path[collection$repo == repo_name][1]

  # One fixed id for the Source view, so a question switch can update its
  # highlight in place (update_code_view_highlight())
  formatted_content = as.character(htmltools::div(
    class = "h-100 p-2",
    code_view_ui(
      source_view_id, raw_content_lines, file_code_language(document_path),
      highlight_ranges = highlight_ranges, font_size = 11, class = "h-100 w-100"
    )
  ))

  return(list(
    content = formatted_content,
    lines = raw_content_lines
  ))
}

# Mark Content UI
#
# id: Character. Module namespace ID

mark_content_ui = function(id) {
  ns = shiny::NS(id)

  bslib::card(
    class = "h-100",
    full_screen = TRUE,
    bslib::card_header(
      class = "bg-light",
      shiny::div(
        class = "d-flex justify-content-between align-items-center gap-3 w-100",
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::span("Content"),
          bslib::popover(
            fa_icon("keyboard", class = "text-muted"),
            title = "Keyboard shortcuts",
            shiny::tags$ul(
              class = "small mb-0 ps-3",
              shiny::tags$li(shiny::tags$kbd("z"), " / ", shiny::tags$kbd("x"), ": previous / next repo"),
              shiny::tags$li(shiny::tags$kbd(","), " / ", shiny::tags$kbd("."), ": previous / next question"),
              shiny::tags$li(shiny::tags$kbd("0"), "-", shiny::tags$kbd("9"), ": toggle rubric items"),
              shiny::tags$li(shiny::tags$kbd("h"), ": toggle the html / source view")
            )
          )
        ),
        shiny::div(
          style = "display: flex; align-items: center; gap: 15px;",
          shiny::div(
            style = "font-size: 14px; font-weight: normal;",
            bslib::input_switch(
              ns("html_toggle"),
              "html",
              value = TRUE,
              width = "auto"
            )
          ),
          # Same fixed width as the rubric pane's question selector so the two
          # headers' prev/select/next controls line up
          shiny::div(
            style = "width: 220px; display: flex; align-items: center; gap: 5px;",
            bslib::tooltip(
              shiny::actionButton(
                ns("repo_prev_btn"),
                fa_icon("chevron-left"),
                class = "btn-sm",
                style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;"
              ),
              "Previous repo (z)"
            ),
            shiny::div(
              style = "flex: 1;",
              shiny::selectInput(
                ns("content_repo_select"),
                NULL,
                choices = NULL,
                width = "100%",
                selectize = TRUE
              )
            ),
            bslib::tooltip(
              shiny::actionButton(
                ns("repo_next_btn"),
                fa_icon("chevron-right"),
                class = "btn-sm",
                style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;"
              ),
              "Next repo (x)"
            )
          )
        )
      )
    ),
    bslib::card_body(
      class = "overflow-auto small p-0",
      # Kept inside the body: as a direct child of the card it would be
      # wrapped in a card body of its own, an empty band above the header
      # The report frame is sandboxed and so cross-origin: the question
      # highlight is sent to it as a message (see artifact_frame_script())
      shiny::tags$script(shiny::HTML("
        $(document).ready(function() {
          Shiny.addCustomMessageHandler('markermd_frame_highlight', function(msg) {
            var frame = document.getElementById('artifact-content-frame');
            if (!frame || !frame.contentWindow) return;
            frame.contentWindow.postMessage({type: 'markermd-highlight', targets: msg.targets, stops: msg.stops || []}, '*');
          });
        });
      ")),
      shiny::uiOutput(ns("content_display"), class = "h-100")
    )
  )
}

# Mark Content Server
#
# id: Character. Module namespace ID
# template: markermd_template. Static template object containing questions
# collection: Reactive returning the parsed collection (data frame with repo,
#   path and ast columns; see parse_repo_documents())
# artifact_paths: Named character vector mapping each repo to its local HTML
#   report path, or NA when none was found
# artifact_urls: Named character vector mapping each repo to the URL its
#   report is served under (see register_artifact_resources()), or NA
# use_qmd: Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
# selected_question: Reactive returning the current question name (from the
#   rubric module), driving the scroll/highlight targets
# external_repo: Reactive returning the repo selected in the Assignments
#   table, so both tabs stay on the same repository (optional)
# repos_dir: The directory holding the repositories, where a question on one
#   of the template's other files finds that file (optional)

mark_content_server = function(id, template, collection, artifact_paths, artifact_urls, use_qmd, selected_question, external_repo = NULL,
                               repos_dir = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Populate the repo selector, starting on the repo selected in the
    # Assignments table so the two tabs agree from the start
    shiny::observe({
      all_repos = names(artifact_paths)
      sel = if (!is.null(external_repo)) shiny::isolate(external_repo()) else NULL
      shiny::updateSelectInput(
        session, "content_repo_select",
        choices = stats::setNames(all_repos, all_repos),
        selected = if (!is.null(sel) && sel %in% all_repos) sel else all_repos[1]
      )
    })

    # Follow later table selections. Selecting the already-current value does
    # not re-fire the client input, so this cannot loop with the reverse sync
    # in the parent app.
    if (!is.null(external_repo)) {
      shiny::observe({
        repo = external_repo()
        if (!is.null(repo) && repo %in% names(artifact_paths)) {
          shiny::updateSelectInput(session, "content_repo_select", selected = repo)
        }
      }) |>
        shiny::bindEvent(external_repo(), ignoreInit = TRUE)
    }

    # HTML content reactive (only depends on repo and toggle, not question).
    # The report is embedded in a sandboxed iframe (see artifact_iframe()):
    # its figures, styles and scripts work, but it cannot reach the app. The
    # frame shows a served copy with the highlight script injected
    # (artifact_frame_url()); its load event re-sends the current highlight.
    html_content_reactive = shiny::reactive({
      shiny::req(input$content_repo_select)
      selected_repo = input$content_repo_select

      html_path = artifact_paths[[selected_repo]]

      if (!is.na(html_path) && file.exists(html_path)) {
        # When several HTML files were found for the repo a notice names the
        # one being shown (NULL otherwise, so the frame stands alone)
        shiny::div(
          class = "d-flex flex-column h-100",
          artifact_ambiguity_note(artifact_paths, selected_repo),
          artifact_iframe(
            src = artifact_frame_url(html_path, artifact_urls[[selected_repo]]),
            id = "artifact-content-frame",
            class = "w-100 flex-grow-1 border-0 bg-white",
            style = "min-height: 70vh;",
            onload = glue::glue(
              "Shiny.setInputValue('<<session$ns(\"artifact_loaded\")>>', Date.now(), {priority: 'event'});",
              .open = "<<", .close = ">>"
            )
          )
        )
      } else if (is.na(html_path)) {
        shiny::div(
          class = "text-center p-4",
          shiny::div(
            class = "d-flex align-items-center justify-content-center mb-3",
            fa_icon("exclamation-triangle", class = "fa-2x text-warning me-2"),
            shiny::h5("No Artifact Available", class = "text-muted mb-0")
          ),
          shiny::p(glue::glue("Repository '{selected_repo}' does not have an associated artifact."), class = "text-muted"),
          shiny::p("Add a rendered report to the project's artifacts directory to view it here.", class = "small text-muted"),
          shiny::actionLink(session$ns("show_raw_instead"), "View the raw source instead")
        )
      } else {
        shiny::p("Artifact file not found for selected repository.", class = "text-muted p-3")
      }
    }) |>
      shiny::bindEvent(input$content_repo_select, ignoreNULL = FALSE)

    # The no-artifact empty state offers the raw source view one click away
    shiny::observe({
      bslib::update_switch("html_toggle", value = FALSE)
    }) |>
      shiny::bindEvent(input$show_raw_instead)

    # Raw content reactive. Invalidated on repo change AND on the html/source
    # toggle: the inserted HTML re-runs its editor init script on every
    # insert anyway, so re-baking the current question's highlight ranges at
    # each (lazy) execution keeps the decorations fresh when toggling back to
    # the Source view after the question changed in HTML mode. Question
    # switches while the Source view is showing update the decorations in
    # place through the observer below rather than rebuilding the editor.
    raw_content_reactive = shiny::reactive({
      shiny::req(input$content_repo_select)
      selected_repo = input$content_repo_select

      raw_content = get_raw_document_content(
        selected_repo, collection(),
        highlight_ranges = shiny::isolate(highlight_ranges_reactive())
      )

      if (!is.null(raw_content)) {
        shiny::HTML(raw_content$content)
      } else {
        file_ext = if (use_qmd) ".qmd" else ".Rmd"
        shiny::p(paste("No", file_ext, "content available for selected repository."), class = "text-muted p-3")
      }
    }) |>
      shiny::bindEvent(input$content_repo_select, input$html_toggle, ignoreNULL = FALSE)

    # Separate reactive for highlight ranges (depends on both repo and question)
    highlight_ranges_reactive = shiny::reactive({
      if (is.null(input$content_repo_select) || is.null(selected_question())) {
        return(NULL)
      }
      selected_repo = input$content_repo_select
      current_question = selected_question()

      highlight_ranges = NULL

      if (!is.null(current_question) && !is.null(template)) {
        question_obj = template_question_by_name(template, current_question)


        if (!is.null(question_obj) && length(question_obj@selected_nodes@node_ids) > 0) {
          repo_ast = collection_ast_for(collection(), selected_repo)
          if (!is.null(repo_ast)) {
            # Map against the raw content lines (without building the code
            # view that get_raw_document_content() does)
            document_lines = repo_document_lines(selected_repo, collection())
            if (!is.null(document_lines)) {
              highlight_ranges = map_content_to_lines(
                document_lines,
                question_obj@selected_nodes@node_ids,
                repo_ast,
                other_question_heading_ids(template, question_obj)
              )
            }
          }
        }
      }

      return(highlight_ranges)
    })

    # Highlight path for question switches while the Source view is showing:
    # replace the view's highlighted lines in place. Repo switches and
    # html/source toggles re-render the view with the current ranges baked
    # in (see raw_content_reactive), so they need no update here.
    shiny::observe({
      if (isTRUE(input$html_toggle)) {
        return()
      }
      update_code_view_highlight(session, source_view_id, highlight_ranges_reactive())
    }) |>
      shiny::bindEvent(highlight_ranges_reactive(), ignoreNULL = FALSE, ignoreInit = TRUE)

    # The current question when it covers one of the template's other files:
    # the pane then shows that file from the repository, whatever the
    # html/source toggle says. Held in a reactiveVal, which only invalidates
    # when the value changes: a plain reactive over selected_question() would
    # rebuild the report frame (or the editor) on every question switch, even
    # between two questions on the document.
    file_question = shiny::reactiveVal(NULL)
    shiny::observe({
      question = if (is.null(template) || is.null(selected_question())) NULL else template_question_by_name(template, selected_question())
      question = if (!is.null(question) && is_file_question(question)) question else NULL
      if (!identical(question, shiny::isolate(file_question()))) {
        file_question(question)
      }
    })

    # Display content based on HTML toggle state
    output$content_display = shiny::renderUI({
      question = file_question()
      if (!is.null(question)) {
        shiny::req(input$content_repo_select, repos_dir)
        repo_dir = file.path(repos_dir, input$content_repo_select)
        found = find_repo_file(repo_dir, question@file)
        relative = if (is.na(found)) NA_character_ else as.character(fs::path_rel(found, normalizePath(repo_dir, winslash = "/")))
        return(file_view_ui(
          found, question@file, editor_id = "markermd-file-editor",
          note = if (!is.na(relative) && !identical(relative, question@file)) paste0("(found at ", relative, ")")
        ))
      }
      if (input$html_toggle) {
        html_content_reactive()
      } else {
        raw_content_reactive()
      }
    })

    # Send the current question's targets to the report frame, which scrolls
    # to the first and highlights them all (artifact_frame_script()). The
    # frame is sandboxed and cross-origin, so this is a postMessage rather
    # than a reach into its document. In the Source view the decorations are
    # handled by the observer above.
    scroll_to_question = function(question_name) {
      shiny::req(input$content_repo_select)
      if (!input$html_toggle) {
        return()
      }

      question_obj = template_question_by_name(template, question_name)
      if (is.null(question_obj)) {
        return()
      }
      node_ids = question_obj@selected_nodes@node_ids
      if (length(node_ids) == 0) {
        return()
      }

      # Node titles are the text fallback for a target the ids do not find:
      # a heading's text, or a div's first class label
      template_records = q2r_flatten(template@original_ast)
      id_to_title = function(id) {
        for (record in template_records) {
          if (S7::S7_inherits(record$node, q2r::pandoc_header) && record$node@attr@id == id) {
            return(q2r::ast_text(record$node))
          }
          if (S7::S7_inherits(record$node, q2r::pandoc_div) && nzchar(record$node@attr@id) && record$node@attr@id == id) {
            cls = node_classes(record$node)
            return(if (length(cls) > 0) paste0(".", cls[1]) else id)
          }
        }
        id
      }
      targets = lapply(node_ids, function(id) list(text = id_to_title(id), id = id))
      stops = as.list(other_question_heading_ids(template, question_obj))
      session$sendCustomMessage("markermd_frame_highlight", list(targets = targets, stops = stops))
    }

    # Handle question selection change for scrolling
    shiny::observe({
      scroll_to_question(selected_question())
    }) |>
      shiny::bindEvent(selected_question(), ignoreInit = FALSE)

    # Re-apply the current question's highlight whenever the artifact frame
    # (re)loads: the frame first renders only once the Grade tab is shown, and
    # it is re-inserted on repo switches and when toggling back from the
    # Source view, so this keeps the highlight in sync for all three cases.
    shiny::observe({
      scroll_to_question(selected_question())
    }) |>
      shiny::bindEvent(input$artifact_loaded)

    # Repo prev/next buttons wrap through the repo list (see navigate_button())
    navigate_button(input, session, "repo_prev_btn", "content_repo_select", names(artifact_paths), -1)
    navigate_button(input, session, "repo_next_btn", "content_repo_select", names(artifact_paths), 1)

    # selected_repo drives the rubric module's grading/comments and the parent
    # app's reverse table sync; set_repo_labels lets the rubric module apply
    # its graded check-mark labels without reaching into this module's session
    return(list(
      selected_repo = shiny::reactive(input$content_repo_select),
      set_repo_labels = function(choices, selected) {
        shiny::updateSelectInput(session, "content_repo_select", choices = choices, selected = selected)
      }
    ))
  })
}
