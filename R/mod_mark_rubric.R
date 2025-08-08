#' Mark Rubric Interface Module
#'
#' Shiny module for displaying and navigating rubric questions during marking

#' Mark Rubric UI
#'
#' @param id Character. Module namespace ID
#'
mark_rubric_ui = function(id) {
  ns = shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(7, 5),
    class = "h-100",
    bslib::card(
      class = "h-100",
      bslib::card_header(
        class = "bg-light",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; gap: 15px;",
          shiny::span("Content"),
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
              shiny::actionButton(
                ns("repo_prev_btn"),
                shiny::icon("chevron-left"),
                class = "btn-sm",
                style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;",
                title = "Previous repo (z)"
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
              shiny::actionButton(
                ns("repo_next_btn"),
                shiny::icon("chevron-right"),
                class = "btn-sm",
                style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;",
                title = "Next repo (x)"
              )
            )
          )
        )
      ),
      bslib::card_body(
        class = "overflow-auto small",
        shiny::uiOutput(ns("content_display"))
      )
    ),
    bslib::card(
      class = "h-100",
      bslib::card_header(
        class = "bg-light",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          shiny::span("Rubric"),
          shiny::div(
            style = "min-width: 150px; display: flex; align-items: center; gap: 5px;",
            shiny::actionButton(
              ns("question_prev_btn"),
              shiny::icon("chevron-left"),
              class = "btn-sm",
              style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;",
              title = "Previous question (,)"
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
            shiny::actionButton(
              ns("question_next_btn"),
              shiny::icon("chevron-right"),
              class = "btn-sm",
              style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;",
              title = "Next question (.)"
            )
          )
        )
      ),
      bslib::card_body(
        class = "overflow-auto small",
        id = ns("rubric_body"),
        # Grade display at top
        shiny::uiOutput(ns("grade_ui")),
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
            title = "Add Question"
          ),
          shiny::span("Add Item", class = "ms-2 text-dark")
        )
      )
    ),
    # JavaScript for keyboard hotkey handling
    shiny::tags$script(shiny::HTML(glue::glue("
      $(document).ready(function() {
        // Global keydown listener for hotkeys and navigation when rubric pane is active
        document.addEventListener('keydown', function(e) {
          // Check if rubric pane exists (indicates it's active)
          var rubricBody = document.getElementById('<<ns('rubric_body')>>');
          if (!rubricBody) return;
          
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

#' Mark Rubric Server
#'
#' @param id Character. Module namespace ID
#' @param template markermd_template. Static template object containing questions
#' @param artifact_status_reactive Reactive value. Artifact status for repositories
#' @param collection_path Character string. Path to collection directory
#' @param use_qmd Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
#' @param collection Parsed collection data from parsermd
#' @param on_question_change Reactive function. Callback when question selection changes
#'
mark_rubric_server = function(id, template, artifact_status_reactive, collection_path, use_qmd, collection, on_question_change = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Global id index
    id_idx = 0

    question_names = purrr::map_chr(template@questions, "name")
    
    # Helper function to get cached artifact path
    get_cached_artifact_path = function(collection_path, repo_name) {
      cache_dir = file.path(path.expand(collection_path), ".markermd")
      normalizePath(file.path(cache_dir, paste0(repo_name, ".html")), mustWork = FALSE)
    }
    
    # Helper function to find correct AST indices by matching against template
    find_matching_ast_indices = function(template_ast, repo_ast, template_indices) {
      matched_indices = c()
      
      for (template_idx in template_indices) {
        if (template_idx > length(template_ast)) {
          next
        }
        
        template_node = template_ast[[template_idx]]
        if (is.null(template_node)) {
          next
        }
        
        if ("rmd_heading" %in% class(template_node)) {
          # For headings, match by name and level
          template_name = attr(template_node, "name")
          template_level = attr(template_node, "level")
          
          # Find matching heading in repo AST
          for (repo_idx in seq_along(repo_ast)) {
            repo_node = repo_ast[[repo_idx]]
            if (!is.null(repo_node) && "rmd_heading" %in% class(repo_node)) {
              repo_name = attr(repo_node, "name")
              repo_level = attr(repo_node, "level")
              
              if (identical(repo_name, template_name) && identical(repo_level, template_level)) {
                matched_indices = c(matched_indices, repo_idx)
                break
              }
            }
          }
        } else {
          # For non-headings, try to use the template index if it exists and matches type
          if (template_idx <= length(repo_ast)) {
            repo_node = repo_ast[[template_idx]]
            if (!is.null(repo_node) && identical(class(template_node), class(repo_node))) {
              matched_indices = c(matched_indices, template_idx)
            }
          }
        }
      }
      
      return(matched_indices)
    }

    # Helper function to map node content to line numbers using AST structure
    map_content_to_lines = function(raw_content_lines, node_indices, repo_ast, template_ast = NULL) {
      if (length(node_indices) == 0) {
        return(list())
      }
      
      # If we have template AST, find matching indices in repo AST
      if (!is.null(template_ast)) {
        actual_indices = find_matching_ast_indices(template_ast, repo_ast, node_indices)
      } else {
        actual_indices = node_indices
      }
      
      if (length(actual_indices) == 0) {
        return(list())
      }
      
      highlight_ranges = list()
      
      # Process each selected node
      for (node_index in actual_indices) {
          # Get the current node
          current_node = repo_ast[[node_index]]
          if (is.null(current_node)) {
            next
          }
          
          # Get node content
          node_content = parsermd::as_document(current_node) |> as.character() |> trimws()
          
          if (length(node_content) == 0 || nchar(node_content)[1] == 0) {
            next
          }
          
          # Extract first line for matching
          node_lines = strsplit(node_content, "\n")[[1]]
          if (length(node_lines) == 0) {
            next
          }
          node_first_line = trimws(node_lines[1])
          
          # Find start line by exact match
          start_line = NULL
          for (line_idx in seq_along(raw_content_lines)) {
            raw_line = trimws(raw_content_lines[line_idx])
            # Ensure both are single strings before comparison
            if (length(raw_line) == 1 && length(node_first_line) == 1 && 
                nchar(raw_line) > 0 && nchar(node_first_line) > 0 &&
                raw_line == node_first_line) {
              start_line = line_idx
              break
            }
          }
          
          if (is.null(start_line)) {
            next
          }
          
          # Find end line using AST structure
          end_line = length(raw_content_lines)  # Default to end of document
          
          if ("rmd_heading" %in% class(current_node)) {
            current_level = attr(current_node, "level")
            
            # Find the next heading at same or higher level in the AST
            next_heading_index = NULL
            for (i in (node_index + 1):length(repo_ast)) {
              if (i > length(repo_ast)) break
              next_node = repo_ast[[i]]
              
              if (!is.null(next_node) && "rmd_heading" %in% class(next_node)) {
                next_level = attr(next_node, "level")
                
                if (length(current_level) == 1 && length(next_level) == 1 && next_level <= current_level) {
                  next_heading_index = i
                  break
                }
              }
            }
            
            # If we found a next heading, find its line in the document
            if (!is.null(next_heading_index)) {
              next_heading_content = parsermd::as_document(repo_ast[[next_heading_index]]) |> 
                as.character() |> trimws()
              
              if (nchar(next_heading_content) > 0) {
                next_lines = strsplit(next_heading_content, "\n")[[1]]
                if (length(next_lines) > 0) {
                  next_first_line = trimws(next_lines[1])
                  
                  # Find where this next heading appears
                  for (line_idx in (start_line + 1):length(raw_content_lines)) {
                    raw_line = trimws(raw_content_lines[line_idx])
                    # Ensure both are single strings before comparison
                    if (length(raw_line) == 1 && length(next_first_line) == 1 && 
                        nchar(raw_line) > 0 && nchar(next_first_line) > 0 &&
                        raw_line == next_first_line) {
                      end_line = line_idx - 1
                      break
                    }
                  }
                }
              }
            }
          } else {
            # For non-heading nodes, use a reasonable default
            end_line = min(start_line + 10, length(raw_content_lines))
          }
          
          # Check if the last line is blank and adjust end_line if needed
          if (end_line <= length(raw_content_lines)) {
            last_line_content = trimws(raw_content_lines[end_line])
            if (nchar(last_line_content) == 0 && end_line > start_line) {
              end_line = end_line - 1
            }
          }
          
          # Add to highlight ranges
          highlight_ranges[[length(highlight_ranges) + 1]] = list(
            start = start_line,
            end = end_line
          )
      }
      
      # Merge overlapping ranges
      if (length(highlight_ranges) > 1) {
        # Sort ranges by start line
        highlight_ranges = highlight_ranges[order(sapply(highlight_ranges, function(r) r$start))]
        
        merged_ranges = list()
        current_range = highlight_ranges[[1]]
        
        for (i in 2:length(highlight_ranges)) {
          next_range = highlight_ranges[[i]]
          
          # If ranges overlap or are adjacent, merge them
          if (next_range$start <= current_range$end + 1) {
            current_range$end = max(current_range$end, next_range$end)
          } else {
            # No overlap, add current range and start new one
            merged_ranges[[length(merged_ranges) + 1]] = current_range
            current_range = next_range
          }
        }
        
        # Add the final range
        merged_ranges[[length(merged_ranges) + 1]] = current_range
        highlight_ranges = merged_ranges
      }
      
      
      return(highlight_ranges)
    }
    
    # Helper function to get raw document content
    get_raw_document_content = function(repo_name, collection, use_qmd, highlight_ranges = NULL) {
      # Find rows for the selected repository in collection
      repo_rows = collection$path |> dirname() |> basename() == repo_name
      if (!any(repo_rows)) {
        return(NULL)
      }
      
      # Get the file path for this repo
      file_path = collection$path[repo_rows][1]
      if (!file.exists(file_path)) {
        return(NULL)
      }
      
      # Read the raw file content
      tryCatch({
        raw_content_lines = readLines(file_path, warn = FALSE)
        raw_content = paste(raw_content_lines, collapse = "\n")
        
        # Determine file extension for syntax highlighting
        file_ext = if (use_qmd) "qmd" else "rmd"
        
        # Use Monaco Editor for better syntax highlighting with soft wrap
        # Create a unique ID for this editor instance
        editor_id = paste0("monaco-editor-", gsub("[^a-zA-Z0-9]", "", repo_name))
        
        # Determine Monaco language - use markdown for both qmd and rmd files
        monaco_language = "markdown"
        
        # Prepare highlighting decorations JSON
        highlight_decorations = "[]" # Default: no highlights
        if (!is.null(highlight_ranges) && length(highlight_ranges) > 0) {
          decorations = lapply(highlight_ranges, function(range) {
            list(
              range = list(
                startLineNumber = range$start,
                startColumn = 1,
                endLineNumber = range$end,
                endColumn = 1
              ),
              options = list(
                isWholeLine = TRUE,
                className = "highlight-line",
                marginClassName = "highlight-margin"
              )
            )
          })
          highlight_decorations = jsonlite::toJSON(decorations, auto_unbox = TRUE)
        }
        
        formatted_content = paste0(
          '<style>
            .highlight-line {
              background-color: rgba(255, 235, 59, 0.15) !important;
            }
            .highlight-margin {
              background-color: rgba(255, 235, 59, 0.15) !important;
              border-left: 3px solid rgba(255, 193, 7, 0.8) !important;
              width: 100% !important;
            }
          </style>',
          '<div style="margin-top: 35px; height: calc(100vh - 255px);"><div id="', editor_id, '" style="height: 100%; width: 100%; border: 1px solid #e1e5e9;"></div></div>',
          '<script>
            (function() {
              // Load Monaco Editor if not already loaded
              if (typeof monaco === "undefined") {
                var script = document.createElement("script");
                script.src = "https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs/loader.js";
                script.onload = function() {
                  require.config({ paths: { vs: "https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs" } });
                  require(["vs/editor/editor.main"], function() {
                    createEditor();
                  });
                };
                document.head.appendChild(script);
              } else {
                createEditor();
              }
              
              function createEditor() {
                // Clean up any existing editor
                var existingContainer = document.getElementById("', editor_id, '");
                if (existingContainer && existingContainer.editor) {
                  existingContainer.editor.dispose();
                }
                
                // Create the editor
                var editor = monaco.editor.create(document.getElementById("', editor_id, '"), {
                  value: ', jsonlite::toJSON(raw_content, auto_unbox = TRUE), ',
                  language: "', monaco_language, '",
                  theme: "vs",
                  readOnly: true,
                  wordWrap: "on",
                  wrappingIndent: "indent",
                  fontSize: 11,
                  lineNumbers: "on",
                  minimap: { enabled: false },
                  scrollBeyondLastLine: false,
                  automaticLayout: true,
                  contextmenu: false,
                  selectOnLineNumbers: false
                });
                
                // Apply highlighting decorations
                var decorations = ', highlight_decorations, ';
                if (decorations.length > 0) {
                  editor.deltaDecorations([], decorations);
                  
                  
                  // Scroll to first highlighted line near top using Monaco built-in method
                  setTimeout(function() {
                    var firstLine = decorations[0].range.startLineNumber;
                    editor.revealLineNearTop(firstLine, monaco.editor.ScrollType.Immediate);
                  }, 200);
                } else {
                  // Ensure editor starts at the top when no highlights
                  setTimeout(function() {
                    editor.setScrollTop(0);
                    editor.revealLine(1);
                  }, 100);
                }
                
                // Store reference for cleanup and global access
                document.getElementById("', editor_id, '").editor = editor;
                document.getElementById("', editor_id, '").decorations = decorations;
                
                // Store globally for dynamic highlighting
                if (!window.monacoEditors) {
                  window.monacoEditors = {};
                }
                window.monacoEditors["', editor_id, '"] = editor;
              }
            })();
          </script>'
        )
        
        return(list(
          content = formatted_content,
          lines = raw_content_lines
        ))
      }, error = function(e) {
        return(paste("Error reading file:", e$message))
      })
    }
    

    question_item_servers = shiny::reactiveValues()
    question_grade_servers = shiny::reactiveValues()
    for(name in question_names) {
      question_item_servers[[name]] = list()
      # Initialize grade server for each question 
      local({
        question_name = name
        question_grade_servers[[question_name]] = mark_grade_server(
          paste0("grade_", question_name),
          markermd_grade_state(current_score = 0, total_score = 10),
          ui_ns = session$ns
        )
      })
    }
    

    redraw_ui = shiny::reactiveVal(0)

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
      shiny::bindEvent(input$question_select, redraw_ui())
    
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
      server_id = paste0("item_", id_idx)

      hotkey = max( 0L, purrr::map_int(question_item_servers[[input$question_select]], ~ .x$item()@hotkey) )+1L
      hotkey = if (hotkey > 10) NA_integer_ else hotkey

      server = mark_rubric_item_server(
          server_id,
          markermd_rubric_item(hotkey, 0, "")
      )
      question_item_servers[[input$question_select]][[server_id]] = server

      # Handle move up signal
      shiny::observe({
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
          }
          
          redraw_ui(redraw_ui()+1)
        }
      }) |>
        shiny::bindEvent(server$move_up_signal(), ignoreInit = TRUE)

      # Handle move down signal
      shiny::observe({
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
          }
          
          redraw_ui(redraw_ui()+1)
        }
      }) |>
        shiny::bindEvent(server$move_down_signal(), ignoreInit = TRUE)

      # Handle delete signal
      shiny::observe({
        question_item_servers[[input$question_select]][[server$id]] = NULL
        
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
            
            # Update the server's internal state
            remaining_servers[[srv_id]]$update_item(updated_item)
          }
        }
        
        redraw_ui(redraw_ui()+1)
      }) |>
        shiny::bindEvent(server$delete_signal(), ignoreInit = TRUE)

      id_idx <<- id_idx + 1
      redraw_ui(redraw_ui()+1)
    }) |>
      shiny::bindEvent(input$add_item, ignoreInit = TRUE)
    
    
    # Content tab functionality - populate select input with all repos
    shiny::observe({
      artifact_status_data = artifact_status_reactive()
      
      # Include all repos regardless of artifact status
      all_repos = names(artifact_status_data)
      choices = stats::setNames(all_repos, all_repos)
      shiny::updateSelectInput(session, "content_repo_select", choices = choices)
    })
    
    # HTML content reactive (only depends on repo and toggle, not question)
    html_content_reactive = shiny::reactive({
      shiny::req(input$content_repo_select)
      selected_repo = input$content_repo_select
      
      # HTML mode - show artifact HTML
      artifact_status_data = artifact_status_reactive()
      status_val = artifact_status_data[[selected_repo]]
      
      # Determine HTML path based on status structure
      html_path = NULL
      if (is.logical(status_val) && !is.na(status_val) && status_val) {
        # Boolean structure - get path using the helper function
        html_path = get_cached_artifact_path(collection_path, selected_repo)
      } else if (is.list(status_val) && !is.null(status_val$path)) {
        # List structure with path
        html_path = status_val$path
      }
      
      if (!is.null(html_path) && file.exists(html_path)) {
        html_content = readLines(html_path, warn = FALSE)
        html_content = paste(html_content, collapse = "\n")
        
        # Note: Using existing Quarto anchor IDs for highlighting
        
        # Wrap in a div with a specific ID for scrolling context
        html_content = paste0('<div id="html-content-container">', html_content, '</div>')
        
        shiny::HTML(html_content)
      } else {
        # Check if repo has no artifact or artifact is not available
        has_artifact = FALSE
        if (is.logical(status_val) && !is.na(status_val) && status_val) {
          has_artifact = TRUE
        } else if (is.list(status_val) && !is.null(status_val$status) && status_val$status == "available") {
          has_artifact = TRUE
        }
        
        if (!has_artifact) {
          shiny::div(
            class = "text-center p-4",
            shiny::div(
              class = "d-flex align-items-center justify-content-center mb-3",
              shiny::icon("exclamation-triangle", class = "fa-2x text-warning me-2"),
              shiny::h5("No Artifact Available", class = "text-muted mb-0")
            ),
            shiny::p(glue::glue("Repository '{selected_repo}' does not have an associated artifact."), class = "text-muted"),
            shiny::p("Use the sync button to download artifacts for GitHub repositories.", class = "small text-muted")
          )
        } else {
          shiny::p("Artifact file not found for selected repository.", class = "text-muted") 
        }
      }
    }) |> 
      shiny::bindEvent(input$content_repo_select, ignoreNULL = FALSE)
    
    # Raw content reactive (only depends on repo, no highlighting)
    raw_content_reactive = shiny::reactive({
      shiny::req(input$content_repo_select)
      selected_repo = input$content_repo_select
      
      # Get raw content without highlighting
      raw_content = get_raw_document_content(selected_repo, collection, use_qmd)
      
      if (!is.null(raw_content)) {
        content_to_display = if (is.list(raw_content)) {
          raw_content$content
        } else {
          raw_content
        }
        
        # Simple container without embedded highlights
        content_with_container = paste0(
          '<div id="raw-content-container">',
          content_to_display,
          '</div>'
        )
        shiny::HTML(content_with_container)
      } else {
        file_ext = if (use_qmd) ".qmd" else ".Rmd"
        shiny::p(paste("No", file_ext, "content available for selected repository."), class = "text-muted")
      }
    }) |> 
      shiny::bindEvent(input$content_repo_select, ignoreNULL = FALSE)
    
    # Separate reactive for highlight ranges (depends on both repo and question)
    highlight_ranges_reactive = shiny::reactive({
      shiny::req(input$content_repo_select, input$question_select)
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
        
        if (!is.null(question_obj) && length(question_obj@selected_nodes@indices) > 0) {
          # Get repository AST
          repo_rows = collection$path |> dirname() |> basename() == selected_repo
          if (any(repo_rows)) {
            repo_ast = collection$ast[repo_rows][[1]]
            if (!is.null(repo_ast)) {
              # Get raw content lines first to map against
              temp_result = get_raw_document_content(selected_repo, collection, use_qmd)
              if (is.list(temp_result) && !is.null(temp_result$lines)) {
                highlight_ranges = map_content_to_lines(
                  temp_result$lines, 
                  question_obj@selected_nodes@indices, 
                  repo_ast,
                  template@original_ast
                )
              }
            }
          }
        }
      }
      
      return(highlight_ranges)
    })
    
    # Observer to update Monaco Editor highlights when question changes
    shiny::observe({
      shiny::req(input$content_repo_select, input$question_select)
      highlight_ranges = highlight_ranges_reactive()
      
      if (!is.null(highlight_ranges) && length(highlight_ranges) > 0) {
        # Convert highlight ranges to Monaco decorations format
        decorations = lapply(highlight_ranges, function(range) {
          list(
            range = list(
              startLineNumber = range$start,
              startColumn = 1,
              endLineNumber = range$end,
              endColumn = 1
            ),
            options = list(
              isWholeLine = TRUE,
              className = "highlight-line",
              marginClassName = "highlight-margin"
            )
          )
        })
        
        highlight_decorations_json = jsonlite::toJSON(decorations, auto_unbox = TRUE)
        
        # Send decorations to Monaco Editor
        shinyjs::runjs(glue::glue("
          // Update Monaco Editor decorations with retry mechanism
          function updateHighlights(retries) {{
            retries = retries || 0;
            var editorElements = document.querySelectorAll('[id^=\"monaco-editor-\"]');
            var updated = false;
            
            editorElements.forEach(function(editorEl) {{
              if (window.monacoEditors && window.monacoEditors[editorEl.id]) {{
                var editor = window.monacoEditors[editorEl.id];
                var newDecorations = {highlight_decorations_json};
                
                // Use deltaDecorations to replace all existing decorations
                var oldDecorations = editor._currentDecorationIds || [];
                editor._currentDecorationIds = editor.deltaDecorations(oldDecorations, newDecorations);
                
                // Scroll to first highlighted line if there are decorations
                if (newDecorations.length > 0) {{
                  setTimeout(function() {{
                    var firstLine = newDecorations[0].range.startLineNumber;
                    editor.revealLineNearTop(firstLine, monaco.editor.ScrollType.Smooth);
                  }}, 100);
                }}
                
                updated = true;
              }}
            }});
            
            // Retry if no editor found and we haven't exceeded max retries
            if (!updated && retries < 10) {{
              setTimeout(function() {{ updateHighlights(retries + 1); }}, 200);
            }}
          }}
          
          updateHighlights();
        ", .open = "{", .close = "}"))
      } else {
        # Clear all highlights
        shinyjs::runjs("
          function clearHighlights(retries) {
            retries = retries || 0;
            var editorElements = document.querySelectorAll('[id^=\"monaco-editor-\"]');
            var updated = false;
            
            editorElements.forEach(function(editorEl) {
              if (window.monacoEditors && window.monacoEditors[editorEl.id]) {
                var editor = window.monacoEditors[editorEl.id];
                var oldDecorations = editor._currentDecorationIds || [];
                editor._currentDecorationIds = editor.deltaDecorations(oldDecorations, []);
                updated = true;
              }
            });
            
            // Retry if no editor found and we haven't exceeded max retries
            if (!updated && retries < 10) {
              setTimeout(function() { clearHighlights(retries + 1); }, 200);
            }
          }
          
          clearHighlights();
        ")
      }
    }) |> 
      shiny::bindEvent(highlight_ranges_reactive(), ignoreNULL = FALSE)
    
    # Observer to apply initial highlighting when switching to raw mode
    shiny::observe({
      # Only trigger when switching to raw mode (html_toggle = FALSE)
      if (!input$html_toggle && !is.null(input$content_repo_select) && !is.null(input$question_select)) {
        # Delay to ensure Monaco Editor is created
        shinyjs::delay(500, {
          highlight_ranges = highlight_ranges_reactive()
          
          if (!is.null(highlight_ranges) && length(highlight_ranges) > 0) {
            # Convert highlight ranges to Monaco decorations format
            decorations = lapply(highlight_ranges, function(range) {
              list(
                range = list(
                  startLineNumber = range$start,
                  startColumn = 1,
                  endLineNumber = range$end,
                  endColumn = 1
                ),
                options = list(
                  isWholeLine = TRUE,
                  className = "highlight-line",
                  marginClassName = "highlight-margin"
                )
              )
            })
            
            highlight_decorations_json = jsonlite::toJSON(decorations, auto_unbox = TRUE)
            
            # Apply initial highlighting
            shinyjs::runjs(glue::glue("
              function applyInitialHighlights(retries) {{
                retries = retries || 0;
                var editorElements = document.querySelectorAll('[id^=\"monaco-editor-\"]');
                var updated = false;
                
                editorElements.forEach(function(editorEl) {{
                  if (window.monacoEditors && window.monacoEditors[editorEl.id]) {{
                    var editor = window.monacoEditors[editorEl.id];
                    var newDecorations = {highlight_decorations_json};
                    
                    // Apply initial decorations
                    var oldDecorations = editor._currentDecorationIds || [];
                    editor._currentDecorationIds = editor.deltaDecorations(oldDecorations, newDecorations);
                    
                    // Scroll to first highlighted line if there are decorations
                    if (newDecorations.length > 0) {{
                      setTimeout(function() {{
                        var firstLine = newDecorations[0].range.startLineNumber;
                        editor.revealLineNearTop(firstLine, monaco.editor.ScrollType.Smooth);
                      }}, 100);
                    }}
                    
                    updated = true;
                  }}
                }});
                
                // Retry if no editor found and we haven't exceeded max retries
                if (!updated && retries < 15) {{
                  setTimeout(function() {{ applyInitialHighlights(retries + 1); }}, 300);
                }}
              }}
              
              applyInitialHighlights();
            ", .open = "{", .close = "}"))
          }
        })
      }
    }) |> 
      shiny::bindEvent(input$html_toggle, input$content_repo_select, input$question_select, ignoreInit = TRUE)
    
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
        repo_rows = collection$path |> dirname() |> basename() == selected_repo
        repo_ast = if (any(repo_rows)) collection$ast[repo_rows][[1]] else NULL
        
        if (!is.null(repo_ast)) {
          # Map template indices to actual repo AST indices
          actual_indices = find_matching_ast_indices(template@original_ast, repo_ast, question_obj@selected_nodes@indices)
          
          # Get all hierarchies for highlighting using actual repo AST and mapped indices
          all_hierarchies = get_heading_selector(repo_ast, actual_indices)
        } else {
          # Fallback to template AST if repo AST not found
          all_hierarchies = get_heading_selector(template@original_ast, question_obj@selected_nodes@indices)
        }
        if (length(all_hierarchies) > 0) {
          # Use first hierarchy for scrolling
          first_hierarchy = all_hierarchies[[1]]
          if (length(first_hierarchy) > 0) {
            # Get the deepest heading from first hierarchy for scrolling
            target_heading = first_hierarchy[length(first_hierarchy)]
            
            # Clean heading text to match Quarto's data-anchor-id format
            # Quarto converts "Write up" -> "write-up", spaces become hyphens
            clean_heading = gsub("[^a-zA-Z0-9 ]", "", target_heading)     # Remove punctuation but keep spaces
            clean_heading = gsub(" +", "-", trimws(clean_heading))        # Convert spaces to hyphens
            clean_heading = tolower(clean_heading)                       # Make lowercase
            target_id = clean_heading  # Use Quarto's data-anchor-id format
            
            # Create JavaScript array of all target headings and their expected IDs
            all_target_data = lapply(all_hierarchies, function(hierarchy) {
              if (length(hierarchy) > 0) {
                deepest_heading = hierarchy[length(hierarchy)]
                # Match Quarto's data-anchor-id format exactly
                clean_id = gsub("[^a-zA-Z0-9 ]", "", deepest_heading)    # Remove punctuation but keep spaces
                clean_id = gsub(" +", "-", trimws(clean_id))             # Convert spaces to hyphens  
                clean_id = tolower(clean_id)                            # Make lowercase
                list(text = deepest_heading, id = clean_id)
              } else {
                NULL
              }
            })
            all_target_data = all_target_data[!sapply(all_target_data, is.null)]
            
            # Create JavaScript array of the target data
            js_target_data = paste0("[", paste(sapply(all_target_data, function(d) {
              paste0("{text: '", d$text, "', id: '", d$id, "'}")
            }), collapse = ", "), "]")
            
            
            # JavaScript with scrolling to first and highlighting all
            scroll_js = paste0("
              setTimeout(function() {
                // Check if HTML content container exists (ensures content is loaded)
                var contentContainer = document.getElementById('html-content-container');
                if (!contentContainer) {
                  // Retry after a shorter delay if content not loaded
                  setTimeout(arguments.callee, 200);
                  return;
                }
                
                // Clear any existing highlights by removing wrapper divs
                var previousWrappers = document.querySelectorAll('.section-highlight-wrapper');
                previousWrappers.forEach(function(wrapper) {
                  // Move children back to original parent and remove wrapper
                  var parent = wrapper.parentNode;
                  while (wrapper.firstChild) {
                    parent.insertBefore(wrapper.firstChild, wrapper);
                  }
                  parent.removeChild(wrapper);
                });
                
                // Try to find target by data-anchor-id first, then by text content if not found
                var firstTarget = document.querySelector('[data-anchor-id=\"", target_id, "\"]');
                
                if (!firstTarget) {
                  // Try to find by heading text content
                  var allHeadings = document.querySelectorAll('h1, h2, h3, h4, h5, h6');
                  for (var i = 0; i < allHeadings.length; i++) {
                    var heading = allHeadings[i];
                    if (heading.textContent.trim().toLowerCase() === '", tolower(target_heading), "') {
                      firstTarget = heading;
                      break;
                    }
                  }
                }
                
                if (firstTarget) {
                  var targetRect = firstTarget.getBoundingClientRect();
                  var scrollContainer = firstTarget.closest('.overflow-auto') || window;
                  var headroom = 20; // pixels of space above the target
                  
                  if (scrollContainer === window) {
                    window.scrollTo({
                      top: window.scrollY + targetRect.top - headroom,
                      behavior: 'smooth'
                    });
                  } else {
                    var containerRect = scrollContainer.getBoundingClientRect();
                    scrollContainer.scrollTo({
                      top: scrollContainer.scrollTop + (targetRect.top - containerRect.top) - headroom,
                      behavior: 'smooth'
                    });
                  }
                }
                
                // Highlight all target sections
                var targetData = ", js_target_data, ";
                targetData.forEach(function(target) {
                  var targetElement = document.querySelector('[data-anchor-id=\"' + target.id + '\"]');
                  
                  if (!targetElement) {
                    // Try to find by heading text content
                    var allHeadings = document.querySelectorAll('h1, h2, h3, h4, h5, h6');
                    for (var i = 0; i < allHeadings.length; i++) {
                      var heading = allHeadings[i];
                      if (heading.textContent.trim().toLowerCase() === target.text.toLowerCase()) {
                        targetElement = heading;
                        break;
                      }
                    }
                  }
                  
                  if (targetElement) {
                    // Collect all elements to wrap
                    var elementsToWrap = [targetElement];
                    
                    // Find all content until the next heading of same or higher level
                    var currentElement = targetElement.nextElementSibling;
                    var targetLevel = parseInt(targetElement.tagName.charAt(1));
                    
                    while (currentElement) {
                      // If we hit another heading, check its level
                      if (currentElement.tagName && currentElement.tagName.match(/^H[1-6]$/)) {
                        var currentLevel = parseInt(currentElement.tagName.charAt(1));
                        if (currentLevel <= targetLevel) {
                          break;
                        }
                      }
                      
                      // Add to elements to wrap
                      elementsToWrap.push(currentElement);
                      currentElement = currentElement.nextElementSibling;
                    }
                    
                    // Create wrapper div
                    var wrapper = document.createElement('div');
                    wrapper.className = 'section-highlight-wrapper';
                    
                    // Insert wrapper before the first element
                    var parent = targetElement.parentNode;
                    parent.insertBefore(wrapper, targetElement);
                    
                    // Move all elements into the wrapper
                    elementsToWrap.forEach(function(element) {
                      wrapper.appendChild(element);
                    });
                  }
                });
              }, 100);
            ")
            
            shinyjs::runjs(scroll_js)
          }
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
    
    # Repository navigation buttons
    shiny::observeEvent(input$repo_prev_btn, {
      current_choices = names(shiny::isolate(artifact_status_reactive()))
      current_selected = shiny::isolate(input$content_repo_select)
      
      if (length(current_choices) > 1 && !is.null(current_selected)) {
        current_index = match(current_selected, current_choices)
        if (!is.na(current_index)) {
          # Previous repo (wrap around)
          new_index = if (current_index <= 1) length(current_choices) else current_index - 1
          new_selection = current_choices[new_index]
          shiny::updateSelectInput(session, "content_repo_select", selected = new_selection)
        }
      }
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$repo_next_btn, {
      current_choices = names(shiny::isolate(artifact_status_reactive()))
      current_selected = shiny::isolate(input$content_repo_select)
      
      if (length(current_choices) > 1 && !is.null(current_selected)) {
        current_index = match(current_selected, current_choices)
        if (!is.na(current_index)) {
          # Next repo (wrap around)
          new_index = if (current_index >= length(current_choices)) 1 else current_index + 1
          new_selection = current_choices[new_index]
          shiny::updateSelectInput(session, "content_repo_select", selected = new_selection)
        }
      }
    }, ignoreInit = TRUE)
    
    # Question navigation buttons  
    shiny::observeEvent(input$question_prev_btn, {
      current_choices = question_names
      current_selected = shiny::isolate(input$question_select)
      
      if (length(current_choices) > 1 && !is.null(current_selected)) {
        current_index = match(current_selected, current_choices)
        if (!is.na(current_index)) {
          # Previous question (wrap around)
          new_index = if (current_index <= 1) length(current_choices) else current_index - 1
          new_selection = current_choices[new_index]
          shiny::updateSelectInput(session, "question_select", selected = new_selection)
        }
      }
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$question_next_btn, {
      current_choices = question_names
      current_selected = shiny::isolate(input$question_select)
      
      if (length(current_choices) > 1 && !is.null(current_selected)) {
        current_index = match(current_selected, current_choices)
        if (!is.na(current_index)) {
          # Next question (wrap around)
          new_index = if (current_index >= length(current_choices)) 1 else current_index + 1
          new_selection = current_choices[new_index]
          shiny::updateSelectInput(session, "question_select", selected = new_selection)
        }
      }
    }, ignoreInit = TRUE)
    
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