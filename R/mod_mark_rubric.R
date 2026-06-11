# Mark Rubric Interface Module
#
# Shiny module for displaying and navigating rubric questions during marking

# Mark Rubric UI
#
# id: Character. Module namespace ID

mark_rubric_ui = function(id) {
  ns = shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(7, 5),
    class = "h-100",
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
            shiny::uiOutput(ns("repo_progress"), inline = TRUE),
            bslib::tooltip(
              shiny::icon("keyboard", class = "text-muted"),
              "Keyboard shortcuts: z / x previous or next repo; , / . previous or next question; 1-9 and 0 toggle rubric items; h toggles the html/source view"
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
    ),
    bslib::card(
      class = "h-100",
      bslib::card_header(
        class = "bg-light",
        shiny::div(
          class = "d-flex justify-content-between align-items-center w-100",
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::span("Rubric"),
            shiny::uiOutput(ns("question_progress"), inline = TRUE)
          ),
          shiny::div(
            style = "min-width: 150px; display: flex; align-items: center; gap: 5px;",
            bslib::tooltip(
              shiny::actionButton(
                ns("question_prev_btn"),
                shiny::icon("chevron-left"),
                class = "btn-sm",
                style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;"
              ),
              "Previous question (,)"
            ),
            shiny::div(
              style = "flex: 1;",
              shiny::selectInput(
                ns("question_select"),
                NULL,
                choices = NULL,
                width = "100%",
                selectize = TRUE
              )
            ),
            bslib::tooltip(
              shiny::actionButton(
                ns("question_next_btn"),
                shiny::icon("chevron-right"),
                class = "btn-sm",
                style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;"
              ),
              "Next question (.)"
            )
          )
        )
      ),
      bslib::card_body(
        class = "overflow-auto small",
        id = ns("rubric_body"),
        # Grade display at top
        shiny::uiOutput(ns("grade_ui")),
        # Per-question comment for the current repo. A non-empty comment counts
        # the question as graded, so full-credit / zero-deduction answers can
        # be marked as reviewed without selecting a rubric item.
        shiny::div(
          class = "my-2",
          shiny::textAreaInput(
            ns("question_comment"),
            NULL,
            value = "",
            rows = 2,
            width = "100%",
            placeholder = "Comment for this question (also marks it as graded)"
          )
        ),
        shiny::div(
          id = ns("rubric_items_container"),
          # Dynamic container for items
          shiny::uiOutput(ns("rubric_items_ui"))
        ),
        shiny::div(
          class = "text-center mb-3",
          shiny::actionButton(
            ns("add_item"),
            shiny::icon("plus"),
            class = "btn-primary btn-sm rounded-circle",
            style = "width: 30px; height: 30px; display: inline-flex; align-items: center; justify-content: center; padding: 0;",
            title = "Add Item"
          ),
          shiny::span("Add Item", class = "ms-2 text-dark")
        ),
        shiny::p(
          "Grading saves automatically to the project database.",
          class = "text-center text-muted small mb-0"
        )
      )
    ),
    # JavaScript for keyboard hotkey handling
    shiny::tags$script(shiny::HTML(glue::glue("
      $(document).ready(function() {
        // Global keydown listener for hotkeys and navigation when rubric pane is active
        document.addEventListener('keydown', function(e) {
          // Only act when the rubric pane is actually visible: hidden bslib
          // tab panes stay in the DOM, so an existence check alone would keep
          // the hotkeys live (and silently mutating grades) on other tabs.
          var rubricBody = document.getElementById('<<ns('rubric_body')>>');
          if (!rubricBody || rubricBody.offsetParent === null) return;

          // Suspend hotkeys while a modal dialog is open (e.g. the rubric
          // item delete confirmation): toggling items or moving the question
          // selection behind a modal silently corrupts what gets deleted/saved
          if (document.body.classList.contains('modal-open')) return;
          
          // Check if any input is currently focused or if user is editing
          var activeElement = document.activeElement;
          var isInputFocused = activeElement && (
            activeElement.matches('input, textarea, select, [contenteditable=\"true\"]') ||
            activeElement.isContentEditable ||
            activeElement.tagName === 'INPUT' ||
            activeElement.tagName === 'TEXTAREA'
          );
          
          // Also check if there's a text selection in the page
          var hasSelection = window.getSelection && window.getSelection().toString().length > 0;
          
          // Only proceed if nothing is being edited and rubric pane is visible
          if (!isInputFocused && !hasSelection) {
            
            // Handle numeric keys 0-9 for hotkeys
            if (e.key >= '0' && e.key <= '9') {
              // Map key to hotkey number (0 = 10, 1-9 = 1-9)
              var hotkey = e.key === '0' ? 10 : parseInt(e.key);
              
              // Find button with matching hotkey in rubric pane
              var buttons = document.querySelectorAll('#<<ns('rubric_items_container')>> button');
              for (var i = 0; i < buttons.length; i++) {
                var btn = buttons[i];
                if (btn.textContent.trim() === hotkey.toString() || 
                    (hotkey === 10 && btn.textContent.trim() === '0')) {
                  e.preventDefault();
                  btn.click();
                  break;
                }
              }
            }
            
            // Handle navigation keys by triggering button clicks
            else if (e.key === 'x' || e.key === 'z') {
              // Repository navigation (x = next, z = previous)
              e.preventDefault();
              
              var btnId = e.key === 'x' ? '<<ns(\"repo_next_btn\")>>' : '<<ns(\"repo_prev_btn\")>>';
              var btn = document.getElementById(btnId);
              if (btn) {
                btn.click();
              }
            }
            
            else if (e.key === ',' || e.key === '.') {
              // Question navigation (, = previous, . = next)
              e.preventDefault();
              
              var btnId = e.key === ',' ? '<<ns(\"question_prev_btn\")>>' : '<<ns(\"question_next_btn\")>>';
              var btn = document.getElementById(btnId);
              if (btn) {
                btn.click();
              }
            }
            
            else if (e.key === 'h') {
              // Toggle HTML switch
              e.preventDefault();
              
              var htmlToggle = document.getElementById('<<ns(\"html_toggle\")>>');
              if (htmlToggle) {
                htmlToggle.click();
              }
            }
          }
        });
      });
    ", .open = "<<", .close = ">>")))
  )
}

# Mark Rubric Server
#
# id: Character. Module namespace ID
# template: markermd_template. Static template object containing questions
# artifact_paths: Named character vector mapping each repo to its local HTML
#   report path, or NA when none was found
# artifact_urls: Named character vector mapping each repo to the URL its
#   report is served under (see register_artifact_resources()), or NA
# root: Character string. Project root directory (base for the grading database)
# use_qmd: Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
# collection: Parsed collection data (data frame with path and ast columns)
# database_state: List. Database state loaded from SQLite (optional)
# selected_repo: Reactive returning the repo selected in the Assignments
#   table, so both tabs stay on the same repository (optional)
# on_question_change: Reactive function. Callback when question selection changes

mark_rubric_server = function(id, template, artifact_paths, artifact_urls, root, use_qmd, collection, database_state = NULL, selected_repo = NULL, on_question_change = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Global id index
    id_idx = 0

    question_names = purrr::map_chr(template@questions, "name")

    # Bumped after grading data (selections, comments, deletions) is
    # persisted, so progress displays re-read the database only once the
    # write has actually happened
    grading_data_version = shiny::reactiveVal(0L)
    bump_grading_version = function() {
      grading_data_version(shiny::isolate(grading_data_version()) + 1L)
    }

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
    
    # Helper function to get raw document content
    get_raw_document_content = function(repo_name, collection, use_qmd, highlight_ranges = NULL) {
      # Find rows for the selected repository in collection
      repo_rows = collection$repo == repo_name
      if (!any(repo_rows)) {
        return(NULL)
      }
      
      # Get the file path for this repo
      file_path = collection$path[repo_rows][1]
      if (!file.exists(file_path)) {
        return(NULL)
      }
      
      # Read the raw file content (normalised so knitr chunk headers match the
      # parsed AST and section-based highlighting lines up)
      raw_content_lines = normalize_knitr_chunks(readLines(file_path, warn = FALSE))
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

      formatted_content = paste0(
        '<div class="h-100 p-2"><div id="', editor_id, '" class="h-100 w-100 border rounded"></div></div>',
        '<script>', editor_script, '</script>'
      )

      return(list(
        content = formatted_content,
        lines = raw_content_lines
      ))
    }
    

    question_item_servers = shiny::reactiveValues()
    question_grade_servers = shiny::reactiveValues()
    for(name in question_names) {
      question_item_servers[[name]] = list()
      # Initialize grade server for each question using database state if available
      local({
        question_name = name
        initial_grade_state = if (!is.null(database_state) && 
                                   !is.null(database_state$grade_states[[question_name]])) {
          database_state$grade_states[[question_name]]
        } else {
          markermd_grade_state(current_score = 0, total_score = 10)
        }
        
        question_grade_servers[[question_name]] = mark_grade_server(
          paste0("grade_", question_name),
          initial_grade_state,
          ui_ns = session$ns,
          collection_path = root,
          question_name = question_name
        )
      })
    }
    

    redraw_ui = shiny::reactiveVal(0)

    # Wire the parent-side handlers for one rubric item server: move up/down
    # reordering and deletion. Called for items added in this session and for
    # items recreated from the database, so loaded items respond to their
    # buttons too.
    wire_item_signals = function(server) {
      handles = list()

      # Handle move up signal
      handles$move_up = shiny::observe({
        server_list = question_item_servers[[input$question_select]]
        server_names = names(server_list)
        current_index = which(server_names == server$id)

        if (length(current_index) > 0 && length(server_names) > 1) {
          if (current_index == 1) {
            # Moving up from top - move item to end of list
            new_order = c(2:length(server_list), 1)
          } else {
            # Normal move up - swap with previous item
            new_index = current_index - 1
            new_order = seq_along(server_list)
            new_order[c(current_index, new_index)] = new_order[c(new_index, current_index)]
          }

          # Reorder the server list
          new_server_list = server_list[new_order]
          names(new_server_list) = server_names[new_order]

          # Update the list with new order
          question_item_servers[[input$question_select]] = new_server_list

          # Update all hotkeys to maintain sequence
          for (i in seq_along(new_server_list)) {
            srv = new_server_list[[i]]
            current_item = srv$item()
            new_hotkey = if (i <= 10) as.integer(i) else NA_integer_

            updated_item = markermd_rubric_item(
              hotkey = new_hotkey,
              points = current_item@points,
              description = current_item@description,
              selected = current_item@selected
            )

            srv$update_item(updated_item)
            # Persist the renumbered hotkey; load_rubric_items() orders by
            # hotkey, so the new arrangement survives an app restart
            save_rubric_item(root, input$question_select, names(new_server_list)[i], updated_item)
          }

          redraw_ui(redraw_ui()+1)
        }
      }) |>
        shiny::bindEvent(server$move_up_signal(), ignoreInit = TRUE)

      # Handle move down signal
      handles$move_down = shiny::observe({
        server_list = question_item_servers[[input$question_select]]
        server_names = names(server_list)
        current_index = which(server_names == server$id)

        if (length(current_index) > 0 && length(server_names) > 1) {
          if (current_index == length(server_names)) {
            # Moving down from bottom - move item to beginning of list
            new_order = c(length(server_list), 1:(length(server_list)-1))
          } else {
            # Normal move down - swap with next item
            new_index = current_index + 1
            new_order = seq_along(server_list)
            new_order[c(current_index, new_index)] = new_order[c(new_index, current_index)]
          }

          # Reorder the server list
          new_server_list = server_list[new_order]
          names(new_server_list) = server_names[new_order]

          # Update the list with new order
          question_item_servers[[input$question_select]] = new_server_list

          # Update all hotkeys to maintain sequence
          for (i in seq_along(new_server_list)) {
            srv = new_server_list[[i]]
            current_item = srv$item()
            new_hotkey = if (i <= 10) as.integer(i) else NA_integer_

            updated_item = markermd_rubric_item(
              hotkey = new_hotkey,
              points = current_item@points,
              description = current_item@description,
              selected = current_item@selected
            )

            srv$update_item(updated_item)
            # Persist the renumbered hotkey; load_rubric_items() orders by
            # hotkey, so the new arrangement survives an app restart
            save_rubric_item(root, input$question_select, names(new_server_list)[i], updated_item)
          }

          redraw_ui(redraw_ui()+1)
        }
      }) |>
        shiny::bindEvent(server$move_down_signal(), ignoreInit = TRUE)

      # Handle delete signal
      handles$delete = shiny::observe({
        question_item_servers[[input$question_select]][[server$id]] = NULL

        # Remove the item's row and its grade-selection events; otherwise it
        # reappears on the next app start via load_rubric_items()
        delete_rubric_item(root, input$question_select, server$id)
        bump_grading_version()

        # Reorder hotkeys for remaining items to ensure continuity
        remaining_servers = question_item_servers[[input$question_select]]
        if (length(remaining_servers) > 0) {
          server_list = names(remaining_servers)

          # Reassign continuous hotkeys starting from 1
          for (i in seq_along(server_list)) {
            srv_id = server_list[i]
            current_item = remaining_servers[[srv_id]]$item()

            new_hotkey = if (i <= 10) as.integer(i) else NA_integer_
            updated_item = markermd_rubric_item(
              hotkey = new_hotkey,
              points = current_item@points,
              description = current_item@description,
              selected = current_item@selected
            )

            # Update the server's internal state. The item's hotkey button
            # self-renders from this state, so remaining labels renumber without
            # a parent re-render. Persist the renumbered hotkey so the database
            # stays in sync with what is shown.
            remaining_servers[[srv_id]]$update_item(updated_item)
            save_rubric_item(root, input$question_select, srv_id, updated_item)
          }
        }

        # Remove only the deleted item; existing items keep their DOM (and any
        # in-progress edit) instead of being rebuilt by a full re-render.
        shiny::removeUI(selector = paste0("#", session$ns(server$id), "-container"))

        # The item is gone, so destroy its handlers; repeated add/delete cycles
        # would otherwise accumulate dead observers for the session's lifetime.
        handles$move_up$destroy()
        handles$move_down$destroy()
        handles$delete$destroy()
      }) |>
        shiny::bindEvent(server$delete_signal(), ignoreInit = TRUE)
    }

    # Initialize rubric items from database state inside an observer
    shiny::observe({
      if (!is.null(database_state) && !is.null(database_state$rubric_items)) {
        for (question_name in question_names) {
          if (!is.null(database_state$rubric_items[[question_name]]) &&
              length(database_state$rubric_items[[question_name]]) > 0) {
            saved_items = database_state$rubric_items[[question_name]]
            for (item_id in names(saved_items)) {
              server_id = item_id  # Use the same ID from database
              server = mark_rubric_item_server(
                server_id,
                saved_items[[item_id]],
                collection_path = root,
                question_name = question_name,
                item_id = item_id
              )
              question_item_servers[[question_name]][[server_id]] = server
              wire_item_signals(server)
            }
          }
        }
        # Trigger UI redraw to show loaded items
        redraw_ui(redraw_ui() + 1)
      }
    }) |>
      shiny::bindEvent(TRUE, once = TRUE)  # Run once when module starts

    shiny::observe({
      shiny::updateSelectInput(
        session, "question_select", 
        choices = question_names, selected = question_names[1]
      )
    })
    
    # Render grade UI for current question
    output$grade_ui = shiny::renderUI({
      shiny::req(input$question_select)
      
      current_grade_server = question_grade_servers[[input$question_select]]
      if (!is.null(current_grade_server)) {
        current_grade_state = current_grade_server$grade()
        mark_grade_ui(session$ns(current_grade_server$id), current_grade_state)
      }
    }) |>
      # Only re-render when the question changes. Within a question the score is
      # updated imperatively via update_grade() -> update_score_display(), so
      # rubric-item add/move/delete (which bump redraw_ui) must not rebuild this
      # widget.
      shiny::bindEvent(input$question_select)
    
    output$rubric_items_ui = shiny::renderUI({
      shiny::req(input$question_select)

      uis = lapply(question_item_servers[[input$question_select]], function(server) {
        mark_rubric_item_ui(session$ns(server$id), server$item())
      })
      
      return(shiny::tagList(uis))
    }) |>
      shiny::bindEvent(redraw_ui(), input$question_select)
    
    shiny::observe({
      if (!is.null(on_question_change)) {
        on_question_change(input$question_select)
      }
    }) |> 
      shiny::bindEvent(input$question_select)

    # Handle add item button
    shiny::observe({
      current_servers = question_item_servers[[input$question_select]]

      # The unique server_id must not collide across ANY question: module ids
      # share one namespace, and items recreated from the database keep their
      # original ids, so the fresh per-session counter could otherwise mint an
      # id already used by a loaded item elsewhere (two servers bound to the
      # same inputs, each saving to its own question)
      existing_ids = unlist(lapply(shiny::reactiveValuesToList(question_item_servers), names))

      # Generate unique server_id
      server_id = paste0("item_", id_idx)
      while (server_id %in% existing_ids) {
        id_idx <<- id_idx + 1
        server_id = paste0("item_", id_idx)
      }

      hotkey = max( 0L, purrr::map_int(current_servers, ~ .x$item()@hotkey) )+1L
      hotkey = if (hotkey > 10) NA_integer_ else hotkey

      new_item = markermd_rubric_item(hotkey, 0, "")
      
      server = mark_rubric_item_server(
          server_id,
          new_item,
          collection_path = root,
          question_name = input$question_select,
          item_id = server_id
      )
      question_item_servers[[input$question_select]][[server_id]] = server
      
      # Save new item to database
      save_rubric_item(root, input$question_select, server_id, new_item)

      wire_item_signals(server)

      id_idx <<- id_idx + 1

      # Insert only the new item rather than re-rendering the whole list, so
      # in-progress edits (e.g. a description being typed) on existing items are
      # preserved. The list still re-renders from question_item_servers on a
      # move/reorder or question switch, so the source of truth stays consistent.
      shiny::insertUI(
        selector = paste0("#", session$ns("rubric_items_ui")),
        where = "beforeEnd",
        ui = mark_rubric_item_ui(session$ns(server$id), new_item)
      )
    }) |>
      shiny::bindEvent(input$add_item, ignoreInit = TRUE)
    
    
    # Populate the repo selector, starting on the repo selected in the
    # Assignments table so the two tabs agree from the start
    shiny::observe({
      all_repos = names(artifact_paths)
      sel = if (!is.null(selected_repo)) shiny::isolate(selected_repo()) else NULL
      shiny::updateSelectInput(
        session, "content_repo_select",
        choices = stats::setNames(all_repos, all_repos),
        selected = if (!is.null(sel) && sel %in% all_repos) sel else all_repos[1]
      )
    })

    # Follow later table selections. Selecting the already-current value does
    # not re-fire the client input, so this cannot loop with the reverse sync
    # in the parent app.
    if (!is.null(selected_repo)) {
      shiny::observe({
        repo = selected_repo()
        if (!is.null(repo) && repo %in% names(artifact_paths)) {
          shiny::updateSelectInput(session, "content_repo_select", selected = repo)
        }
      }) |>
        shiny::bindEvent(selected_repo(), ignoreInit = TRUE)
    }

    # Graded (question, repo) pairs re-read from the database after each write
    graded_pairs_r = shiny::reactive({
      grading_data_version()
      graded_question_pairs(root)
    })

    # Which questions count as graded for the current repo
    graded_questions = shiny::reactive({
      shiny::req(input$content_repo_select)
      pairs = graded_pairs_r()
      repo_graded = pairs$question_name[pairs$assignment_repo == input$content_repo_select]
      stats::setNames(question_names %in% repo_graded, question_names)
    })

    # Position badge for the repo selector
    output$repo_progress = shiny::renderUI({
      shiny::req(input$content_repo_select)
      repos = names(artifact_paths)
      idx = match(input$content_repo_select, repos)
      bslib::tooltip(
        shiny::span(
          class = "badge text-bg-light fw-normal",
          glue::glue("{idx}/{length(repos)}")
        ),
        glue::glue("Repository {idx} of {length(repos)}")
      )
    })

    # Position and graded-count badge for the question selector
    output$question_progress = shiny::renderUI({
      shiny::req(input$question_select)
      status = graded_questions()
      idx = match(input$question_select, question_names)
      bslib::tooltip(
        shiny::span(
          class = "badge text-bg-light fw-normal",
          glue::glue("Q {idx}/{length(question_names)} · {sum(status)} graded")
        ),
        glue::glue("Question {idx} of {length(question_names)}; {sum(status)} of {length(question_names)} graded for this repository")
      )
    })

    # Prefix graded questions with a check in the selector; only push an
    # update when the labels actually change so open menus are not rebuilt
    last_question_labels = shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(input$question_select)
      status = graded_questions()
      labels = ifelse(status[question_names], paste0("✓ ", question_names), question_names)
      if (identical(labels, last_question_labels())) return()
      last_question_labels(labels)
      shiny::updateSelectInput(
        session, "question_select",
        choices = stats::setNames(question_names, labels),
        selected = shiny::isolate(input$question_select)
      )
    }) |>
      shiny::bindEvent(graded_questions())

    # Prefix fully graded repos with a check in the repo selector
    last_repo_labels = shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(input$content_repo_select)
      repos = names(artifact_paths)
      counts = calculate_grading_progress(root, question_names, repos, graded_pairs = graded_pairs_r())
      done = length(question_names) > 0 & counts[repos] >= length(question_names)
      labels = ifelse(done, paste0("✓ ", repos), repos)
      if (identical(labels, last_repo_labels())) return()
      last_repo_labels(labels)
      shiny::updateSelectInput(
        session, "content_repo_select",
        choices = stats::setNames(repos, labels),
        selected = shiny::isolate(input$content_repo_select)
      )
    }) |>
      shiny::bindEvent(graded_pairs_r())

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
          onload = "this.dataset.loaded = '1';"
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
        selected_repo, collection, use_qmd,
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
      if (is.null(input$content_repo_select) || is.null(input$question_select)) {
        return(NULL)
      }
      selected_repo = input$content_repo_select
      current_question = input$question_select
      
      highlight_ranges = NULL
      
      if (!is.null(current_question) && !is.null(template)) {
        # Find the selected question from template
        question_obj = NULL
        for (q in template@questions) {
          if (q@name == current_question) {
            question_obj = q
            break
          }
        }
        
        if (!is.null(question_obj) && length(question_obj@selected_nodes@node_ids) > 0) {
          # Get repository AST
          repo_rows = collection$repo == selected_repo
          if (any(repo_rows)) {
            repo_ast = collection$ast[repo_rows][[1]]
            if (!is.null(repo_ast)) {
              # Get raw content lines first to map against
              temp_result = get_raw_document_content(selected_repo, collection, use_qmd)
              if (is.list(temp_result) && !is.null(temp_result$lines)) {
                highlight_ranges = map_content_to_lines(
                  temp_result$lines,
                  question_obj@selected_nodes@node_ids,
                  repo_ast
                )
              }
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
    scroll_to_question = function(selected_question) {
      shiny::req(input$content_repo_select)
      # Handle both HTML and raw content modes
      if (!input$html_toggle) {
        # Raw content mode - highlighting is handled by content display reactive
        # Just trigger a content update by invalidating the reactive
        return()
      }
      
      # Find the selected question from template
      question_obj = NULL
      for (q in template@questions) {
        if (q@name == selected_question) {
          question_obj = q
          break
        }
      }
      
      if (!is.null(question_obj)) {
        # Get the repository AST for the current repo
        selected_repo = input$content_repo_select
        repo_rows = collection$repo == selected_repo
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
      if (!is.null(on_question_change)) {
        on_question_change(input$question_select)
      }
      # Always trigger scrolling when question changes
      scroll_to_question(input$question_select)
    }) |> 
      shiny::bindEvent(input$question_select, ignoreInit = FALSE)
    
    # Observer to trigger initial highlighting when switching repos
    shiny::observe({
      shiny::req(input$content_repo_select, input$question_select)
      # Only trigger on repo changes, not question changes
      shiny::invalidateLater(500, session)
      shiny::isolate({
        selected_question = input$question_select
        scroll_to_question(selected_question)
      })
    }) |> 
      shiny::bindEvent(input$content_repo_select, ignoreInit = TRUE)
    
    # Navigation button observers

    # Compute the next selection in a list of choices, wrapping around at the ends
    #
    # current: Currently selected value
    # choices: Character vector of available choices
    # direction: -1 for previous, 1 for next

    navigate_select = function(current, choices, direction) {
      if (length(choices) <= 1 || is.null(current)) {
        return(NULL)
      }

      current_index = match(current, choices)
      if (is.na(current_index)) {
        return(NULL)
      }

      new_index = (current_index - 1 + direction) %% length(choices) + 1
      choices[new_index]
    }

    # Repository navigation buttons
    shiny::observe({
      new_selection = navigate_select(
        shiny::isolate(input$content_repo_select),
        names(artifact_paths),
        direction = -1
      )
      if (!is.null(new_selection)) {
        shiny::updateSelectInput(session, "content_repo_select", selected = new_selection)
      }
    }) |>
      shiny::bindEvent(input$repo_prev_btn, ignoreInit = TRUE)

    shiny::observe({
      new_selection = navigate_select(
        shiny::isolate(input$content_repo_select),
        names(artifact_paths),
        direction = 1
      )
      if (!is.null(new_selection)) {
        shiny::updateSelectInput(session, "content_repo_select", selected = new_selection)
      }
    }) |>
      shiny::bindEvent(input$repo_next_btn, ignoreInit = TRUE)

    # Question navigation buttons
    shiny::observe({
      new_selection = navigate_select(
        shiny::isolate(input$question_select),
        question_names,
        direction = -1
      )
      if (!is.null(new_selection)) {
        shiny::updateSelectInput(session, "question_select", selected = new_selection)
      }
    }) |>
      shiny::bindEvent(input$question_prev_btn, ignoreInit = TRUE)

    shiny::observe({
      new_selection = navigate_select(
        shiny::isolate(input$question_select),
        question_names,
        direction = 1
      )
      if (!is.null(new_selection)) {
        shiny::updateSelectInput(session, "question_select", selected = new_selection)
      }
    }) |>
      shiny::bindEvent(input$question_next_btn, ignoreInit = TRUE)
    
    # --- Per-question comment persistence -----------------------------------
    # The textarea reflects one (question, repo) pair at a time. comment_pair
    # records which pair that is and current_comment_stored the last value
    # persisted (or loaded) for it. Every edit is captured into
    # pending_comment_edit together with the pair it was typed against, so a
    # save (debounced, or flushed on navigation) can never attribute text to a
    # different question/repo even when navigation outruns the textarea's
    # update echo. Programmatic updateTextAreaInput() echoes are consumed via
    # pending_comment_echoes so they are never mistaken for edits.
    comment_pair = shiny::reactiveVal(NULL)
    current_comment_stored = shiny::reactiveVal("")
    pending_comment_edit = shiny::reactiveVal(NULL)
    pending_comment_echoes = shiny::reactiveVal(character(0))

    save_pending_comment = function() {
      edit = pending_comment_edit()
      if (is.null(edit)) {
        return()
      }
      save_comment(root, edit$question, edit$repo, edit$text)
      pair = comment_pair()
      if (!is.null(pair) && identical(pair$question, edit$question) && identical(pair$repo, edit$repo)) {
        current_comment_stored(edit$text)
      }
      pending_comment_edit(NULL)
      bump_grading_version()
    }

    # Capture typing as a pending edit bound to the pair on screen; consume
    # programmatic echoes, and drop the pending edit when the text is typed
    # back to the stored value
    shiny::observe({
      val = input$question_comment
      echoes = pending_comment_echoes()
      hit = match(val, echoes)
      if (!is.na(hit)) {
        pending_comment_echoes(echoes[-hit])
        return()
      }
      pair = comment_pair()
      if (is.null(pair)) {
        return()
      }
      if (identical(val, current_comment_stored())) {
        pending_comment_edit(NULL)
        return()
      }
      pending_comment_edit(list(question = pair$question, repo = pair$repo, text = val))
    }) |>
      shiny::bindEvent(input$question_comment, ignoreInit = TRUE)

    # On question/repo change: flush the pending edit (to its own pair), then
    # load the new pair's stored comment
    shiny::observe({
      shiny::req(input$question_select, input$content_repo_select)

      save_pending_comment()

      comment_pair(list(question = input$question_select, repo = input$content_repo_select))
      stored = load_comment(root, input$question_select, input$content_repo_select)
      if (is.null(stored)) stored = ""
      current_comment_stored(stored)
      if (!identical(stored, input$question_comment)) {
        # The update only echoes back when it changes the client value; cap
        # the outstanding-echo list so an unmatched entry cannot linger forever
        pending_comment_echoes(utils::tail(c(pending_comment_echoes(), stored), 8))
        shiny::updateTextAreaInput(session, "question_comment", value = stored)
      }
    }) |>
      shiny::bindEvent(input$question_select, input$content_repo_select)

    # Debounced autosave while typing; comments are an event log, so one row
    # per pause rather than per keystroke
    comment_text_debounced = shiny::debounce(shiny::reactive(input$question_comment), 1000)

    shiny::observe({
      save_pending_comment()
    }) |>
      shiny::bindEvent(comment_text_debounced(), ignoreInit = TRUE)

    # Reactive calculation for selected rubric items' points for current question
    selected_rubric_points = shiny::reactive({
      shiny::req(input$question_select)
      
      current_servers = question_item_servers[[input$question_select]]
      if (length(current_servers) == 0) {
        return(0)
      }
      
      # Sum points from all selected items
      selected_points = purrr::map_dbl(current_servers, function(server) {
        item = server$item()
        if (item@selected) {
          item@points
        } else {
          0
        }
      })
      
      sum(selected_points)
    })
    
    # Store previous selection states to detect changes
    previous_selections = shiny::reactiveVal(list())
    
    # Observer to log grade selection changes to database
    shiny::observe({
      shiny::req(input$question_select, 
                 input$content_repo_select,
                 cancelOutput = TRUE)
      
      current_question = input$question_select
      current_repo = input$content_repo_select
      
      # Ensure question_item_servers is available and has the current question
      shiny::req(length(question_item_servers) > 0, 
                 current_question %in% names(question_item_servers),
                 cancelOutput = TRUE)
      
      current_servers = question_item_servers[[current_question]]
      
      if (length(current_servers) == 0) {
        return()
      }
      
      # Get current selection states
      current_selections = purrr::map_lgl(current_servers, function(server) {
        server$item()@selected
      })
      names(current_selections) = names(current_servers)
      
      # Get previous selections for this question/repo combination
      prev_key = paste(current_question, current_repo, sep = ":")
      all_prev = previous_selections()
      prev_selections = if (!is.null(all_prev) && prev_key %in% names(all_prev)) {
        all_prev[[prev_key]]
      } else {
        NULL
      }
      
      if (!is.null(prev_selections) && length(prev_selections) > 0) {
        # Compare with previous state and log changes
        any_saved = FALSE
        for (item_id in names(current_selections)) {
          current_selected = current_selections[[item_id]]
          prev_selected = if (item_id %in% names(prev_selections)) prev_selections[[item_id]] else NULL

          # Log to database if selection state changed
          if (is.null(prev_selected) || current_selected != prev_selected) {
            save_grade_selection(root, current_question, current_repo, item_id, current_selected)
            any_saved = TRUE
          }
        }
        if (any_saved) {
          bump_grading_version()
        }
      }
      
      # Update stored selections
      if (is.null(all_prev)) all_prev = list()
      all_prev[[prev_key]] = current_selections
      previous_selections(all_prev)
    })
    
    # Observer to load saved grade selections when switching repos or questions
    shiny::observe({
      shiny::req(input$question_select, 
                 input$content_repo_select,
                 cancelOutput = TRUE)
      
      current_question = input$question_select
      current_repo = input$content_repo_select
      
      # Ensure question_item_servers is available and has the current question
      shiny::req(length(question_item_servers) > 0, 
                 current_question %in% names(question_item_servers),
                 cancelOutput = TRUE)
      
      current_servers = question_item_servers[[current_question]]
      
      if (length(current_servers) == 0) {
        return()
      }
      
      # Load saved selections from database
      saved_selections = load_grade_selections(root, current_question, current_repo)
      
      # Apply saved selections to current servers (or reset to defaults)
      for (item_id in names(current_servers)) {
        server = current_servers[[item_id]]
        current_item = server$item()
        
        # Get saved selection state, default to FALSE if not found
        saved_selected = if (length(saved_selections) > 0 && item_id %in% names(saved_selections)) {
          saved_selections[[item_id]]
        } else {
          FALSE  # Default state when no saved selection exists
        }
        
        # Update if different from current state
        if (current_item@selected != saved_selected) {
          updated_item = markermd_rubric_item(
            hotkey = current_item@hotkey,
            points = current_item@points,
            description = current_item@description,
            selected = saved_selected
          )
          server$update_item(updated_item)
        }
      }
      
      # Initialize current selections in tracking
      current_selections = purrr::map_lgl(current_servers, function(server) {
        server$item()@selected
      })
      names(current_selections) = names(current_servers)
      
      prev_key = paste(current_question, current_repo, sep = ":")
      all_prev = previous_selections()
      if (is.null(all_prev)) all_prev = list()
      all_prev[[prev_key]] = current_selections
      previous_selections(all_prev)
      
    }) |>
      shiny::bindEvent(input$question_select, input$content_repo_select, ignoreInit = TRUE)
    
    # Observer to update grade when rubric selections change
    shiny::observe({
      shiny::req(input$question_select)
      
      current_grade_server = question_grade_servers[[input$question_select]]
      if (!is.null(current_grade_server)) {
        current_grade_state = current_grade_server$grade()
        selected_points_sum = selected_rubric_points()
        
        # Calculate new current score based on grading mode
        new_current_score = if (current_grade_state@grading_mode == "positive") {
          0 + selected_points_sum
        } else {
          current_grade_state@total_score + selected_points_sum
        }
        
        # Apply bounds if enabled
        if (current_grade_state@bound_above_zero && new_current_score < 0) {
          new_current_score = 0
        }
        if (current_grade_state@bound_below_max && new_current_score > current_grade_state@total_score) {
          new_current_score = current_grade_state@total_score
        }
        
        # Only update if the score actually changed
        if (new_current_score != current_grade_state@current_score) {
          updated_grade_state = markermd_grade_state(
            current_score = new_current_score,
            total_score = current_grade_state@total_score,
            grading_mode = current_grade_state@grading_mode,
            bound_above_zero = current_grade_state@bound_above_zero,
            bound_below_max = current_grade_state@bound_below_max
          )
          
          current_grade_server$update_grade(updated_grade_state)
        }
      }
    })
    
    # Return reactive values for external use
    return(list(
      selected_question = shiny::reactive(input$question_select),
      selected_content_repo = shiny::reactive(input$content_repo_select),
      question_grade_servers = question_grade_servers,
      current_grade = shiny::reactive({
        shiny::req(input$question_select)
        current_server = question_grade_servers[[input$question_select]]
        if (!is.null(current_server)) {
          current_server$grade()
        } else {
          NULL
        }
      })
    ))
  })
}
