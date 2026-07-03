# Mark Content Pane Module
#
# Shiny module for the grading interface's left Content pane: repo selection
# and navigation, the rendered-report iframe, the Monaco source view, and the
# per-question scroll/highlight wiring.

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
# Highlights every line of the displayed (normalised) document whose
# enclosing node-id chain contains one of the question's selected ids (a
# selected heading covers its whole section, nested subsections included; a
# selected id'd div covers the lines between its fences).
#
# raw_content_lines: Character vector of the displayed (normalised) document
# node_ids: Character vector of selected node ids (header or div ids)
# repo_ast: q2r pandoc AST of the repo document

map_content_to_lines = function(raw_content_lines, node_ids, repo_ast) {
  if (length(node_ids) == 0) {
    return(list())
  }

  chains = line_node_id_chains(raw_content_lines, repo_ast)
  matched = which(vapply(chains, function(chain) {
    any(node_ids %in% chain)
  }, logical(1)))

  group_consecutive_lines(matched)
}

# Normalised source lines of a repo's document (so knitr chunk headers match
# the parsed AST and section-based highlighting lines up), or NULL when the
# repo has no document on disk
#
# repo_name: Character. Repository name
# collection: Collection data frame (see parse_assignment_collection())

repo_document_lines = function(repo_name, collection) {
  repo_rows = collection$repo == repo_name
  if (!any(repo_rows)) {
    return(NULL)
  }

  file_path = collection$path[repo_rows][1]
  if (!file.exists(file_path)) {
    return(NULL)
  }

  normalize_knitr_chunks(readLines(file_path, warn = FALSE))
}

# Source-view content for a repo: the Monaco editor HTML (with the given line
# ranges decorated) plus the normalised document lines, or NULL when the repo
# has no document
#
# repo_name: Character. Repository name
# collection: Collection data frame (see parse_assignment_collection())
# highlight_ranges: Optional list of start/end line ranges to decorate

get_raw_document_content = function(repo_name, collection, highlight_ranges = NULL) {
  raw_content_lines = repo_document_lines(repo_name, collection)
  if (is.null(raw_content_lines)) {
    return(NULL)
  }

  raw_content = paste(raw_content_lines, collapse = "\n")

  # One fixed editor id for the Source view: monaco_editor_config()
  # disposes the previously registered editor for this id when the
  # container is re-rendered, so cycling repos does not leak editors
  editor_id = "markermd-source-editor"

  editor_script = monaco_editor_config(
    editor_id, raw_content, "markdown",
    font_size = 11,
    decorations = monaco_line_decorations(highlight_ranges)
  )

  formatted_content = as.character(htmltools::tagList(
    htmltools::div(
      class = "h-100 p-2",
      htmltools::div(id = editor_id, class = "h-100 w-100 border rounded")
    ),
    htmltools::tags$script(htmltools::HTML(editor_script))
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
            shiny::icon("keyboard", class = "text-muted"),
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
          shiny::div(
            style = "min-width: 200px; display: flex; align-items: center; gap: 5px;",
            bslib::tooltip(
              shiny::actionButton(
                ns("repo_prev_btn"),
                shiny::icon("chevron-left"),
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
                shiny::icon("chevron-right"),
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
      shiny::uiOutput(ns("content_display"), class = "h-100")
    )
  )
}

# Mark Content Server
#
# id: Character. Module namespace ID
# template: markermd_template. Static template object containing questions
# collection: Parsed collection data (data frame with path and ast columns)
# artifact_paths: Named character vector mapping each repo to its local HTML
#   report path, or NA when none was found
# artifact_urls: Named character vector mapping each repo to the URL its
#   report is served under (see register_artifact_resources()), or NA
# use_qmd: Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
# selected_question: Reactive returning the current question name (from the
#   rubric module), driving the scroll/highlight targets
# external_repo: Reactive returning the repo selected in the Assignments
#   table, so both tabs stay on the same repository (optional)

mark_content_server = function(id, template, collection, artifact_paths, artifact_urls, use_qmd, selected_question, external_repo = NULL) {
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
    # The served report is embedded in a same-origin iframe so its figures and
    # styles load while staying isolated from the app document; data-loaded
    # tells the scroll/highlight JS when the document is ready.
    html_content_reactive = shiny::reactive({
      shiny::req(input$content_repo_select)
      selected_repo = input$content_repo_select

      html_path = artifact_paths[[selected_repo]]

      if (!is.na(html_path) && file.exists(html_path)) {
        shiny::tags$iframe(
          id = "artifact-content-frame",
          src = artifact_urls[[selected_repo]],
          class = "w-100 h-100 border-0 bg-white",
          style = "min-height: 70vh;",
          onload = glue::glue(
            "this.dataset.loaded = '1'; Shiny.setInputValue('<<session$ns(\"artifact_loaded\")>>', Date.now(), {priority: 'event'});",
            .open = "<<", .close = ">>"
          )
        )
      } else if (is.na(html_path)) {
        shiny::div(
          class = "text-center p-4",
          shiny::div(
            class = "d-flex align-items-center justify-content-center mb-3",
            shiny::icon("exclamation-triangle", class = "fa-2x text-warning me-2"),
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
        selected_repo, collection,
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
        question_obj = template_question(template, current_question)


        if (!is.null(question_obj) && length(question_obj@selected_nodes@node_ids) > 0) {
          repo_ast = collection_ast_for(collection, selected_repo)
          if (!is.null(repo_ast)) {
            # Map against the raw content lines (without paying for the full
            # Monaco editor build that get_raw_document_content() does)
            document_lines = repo_document_lines(selected_repo, collection)
            if (!is.null(document_lines)) {
              highlight_ranges = map_content_to_lines(
                document_lines,
                question_obj@selected_nodes@node_ids,
                repo_ast
              )
            }
          }
        }
      }

      return(highlight_ranges)
    })

    # Decoration path for question switches while the Source view is showing:
    # replace the registered editor's decorations in place. Repo switches and
    # html/source toggles re-render the editor with the current ranges baked
    # in (see raw_content_reactive), so they need no update here; running this
    # on the toggle would race the re-render and decorate the editor that is
    # about to be disposed.
    shiny::observe({
      if (isTRUE(input$html_toggle)) {
        return()
      }
      decorations = monaco_line_decorations(highlight_ranges_reactive())
      shinyjs::runjs(monaco_update_decorations_js("markermd-source-editor", decorations))
    }) |>
      shiny::bindEvent(highlight_ranges_reactive(), ignoreNULL = FALSE, ignoreInit = TRUE)

    # Display content based on HTML toggle state
    output$content_display = shiny::renderUI({
      if (input$html_toggle) {
        html_content_reactive()
      } else {
        raw_content_reactive()
      }
    })

    # Create scrolling callback function
    scroll_to_question = function(question_name) {
      shiny::req(input$content_repo_select)
      # Handle both HTML and raw content modes
      if (!input$html_toggle) {
        # Raw content mode - highlighting is handled by content display reactive
        # Just trigger a content update by invalidating the reactive
        return()
      }

      question_obj = template_question(template, question_name)


      if (!is.null(question_obj)) {
        # The stored node ids are the rendered element ids to scroll to
        node_ids = question_obj@selected_nodes@node_ids
        if (length(node_ids) > 0) {
          # Resolve node titles (used only as a text fallback in the scroll JS):
          # a heading's text, or a div's first class label.
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

          all_target_data = lapply(node_ids, function(id) {
            list(text = id_to_title(id), id = id)
          })

          js_target_data = jsonlite::toJSON(all_target_data, auto_unbox = TRUE)

          # Scroll and highlight inside the artifact iframe. The frame is
          # same-origin (served via addResourcePath), so this JS can reach its
          # document; the highlight style is injected into the report itself
          # because the app stylesheet does not apply inside the frame.
          scroll_js = glue::glue("
            (function attempt(retries) {
              retries = retries || 0;
              var frame = document.getElementById('artifact-content-frame');
              if (!frame || frame.dataset.loaded !== '1' || !frame.contentDocument || !frame.contentDocument.body) {
                if (retries < 50) setTimeout(function() { attempt(retries + 1); }, 200);
                return;
              }
              var doc = frame.contentDocument;
              var win = frame.contentWindow;

              if (!doc.getElementById('markermd-highlight-style')) {
                var st = doc.createElement('style');
                st.id = 'markermd-highlight-style';
                st.textContent =
                  '.section-highlight-wrapper { background-color: rgba(255, 235, 59, 0.15); border-left: 3px solid rgba(255, 193, 7, 0.8); padding: 8px; padding-left: 12px; margin: 8px 0; }' +
                  '.section-highlight-wrapper > * { margin-top: 0; }' +
                  '.section-highlight-wrapper > *:not(:last-child) { margin-bottom: 1rem; }' +
                  '.section-highlight-wrapper > *:last-child { margin-bottom: 0; }';
                doc.head.appendChild(st);
              }

              // Clear any existing highlights by unwrapping previous wrappers
              doc.querySelectorAll('.section-highlight-wrapper').forEach(function(wrapper) {
                var parent = wrapper.parentNode;
                while (wrapper.firstChild) parent.insertBefore(wrapper.firstChild, wrapper);
                parent.removeChild(wrapper);
              });

              // Find a target by data-anchor-id, then element id (a rendered
              // id'd div is <div id=...>), then heading text
              function findTarget(target) {
                var el = doc.querySelector('[data-anchor-id=\"' + target.id + '\"]') || doc.getElementById(target.id);
                if (!el) {
                  var headings = doc.querySelectorAll('h1, h2, h3, h4, h5, h6');
                  for (var i = 0; i < headings.length; i++) {
                    if (headings[i].textContent.trim().toLowerCase() === target.text.toLowerCase()) { el = headings[i]; break; }
                  }
                }
                return el;
              }

              var targetData = <<js_target_data>>;

              var firstTarget = targetData.length > 0 ? findTarget(targetData[0]) : null;
              if (firstTarget) {
                var rect = firstTarget.getBoundingClientRect();
                win.scrollTo({ top: win.scrollY + rect.top - 20, behavior: 'smooth' });
              }

              targetData.forEach(function(target) {
                var targetElement = findTarget(target);
                if (!targetElement) return;

                // A heading selects its whole section (its siblings until the
                // next same-or-higher heading); a rendered div already
                // contains its children, so wrap just the div element.
                var elementsToWrap = [targetElement];
                if (targetElement.tagName && targetElement.tagName.match(/^H[1-6]$/)) {
                  var currentElement = targetElement.nextElementSibling;
                  var targetLevel = parseInt(targetElement.tagName.charAt(1));
                  while (currentElement) {
                    if (currentElement.tagName && currentElement.tagName.match(/^H[1-6]$/)) {
                      if (parseInt(currentElement.tagName.charAt(1)) <= targetLevel) break;
                    }
                    elementsToWrap.push(currentElement);
                    currentElement = currentElement.nextElementSibling;
                  }
                }

                var wrapper = doc.createElement('div');
                wrapper.className = 'section-highlight-wrapper';
                targetElement.parentNode.insertBefore(wrapper, targetElement);
                elementsToWrap.forEach(function(element) { wrapper.appendChild(element); });
              });
            })(0);
          ", .open = "<<", .close = ">>")

          shinyjs::runjs(scroll_js)
        }
      }
    }

    # Handle question selection change for scrolling
    shiny::observe({
      scroll_to_question(selected_question())
    }) |>
      shiny::bindEvent(selected_question(), ignoreInit = FALSE)

    # Re-apply the current question's highlight whenever the artifact frame
    # (re)loads. The frame first renders only once the Rubric tab is shown,
    # which can be after the init-time scroll JS has given up retrying; it is
    # also re-inserted on repo switches and when toggling back from the
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
