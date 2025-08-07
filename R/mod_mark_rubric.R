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
              style = "min-width: 200px;",
              shiny::selectInput(
                ns("content_repo_select"),
                NULL,
                choices = NULL,
                width = "100%",
                selectize = TRUE
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
            style = "min-width: 150px;",
            shiny::selectInput(
              ns("question_select"),
              NULL,
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
          )
        )
      ),
      bslib::card_body(
        class = "overflow-auto small",
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
    )
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
                
                // Store reference for cleanup
                document.getElementById("', editor_id, '").editor = editor;
                document.getElementById("', editor_id, '").decorations = decorations;
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
    
    # Helper function to add IDs to headings in HTML content
    add_heading_ids = function(html_content) {
      # Find all headings and add IDs
      # Use a more robust approach without nested gsub functions
      
      # Pattern to match headings
      heading_pattern = "<h([1-6])([^>]*)>([^<]+)</h[1-6]>"
      
      # Find all matches
      matches = gregexpr(heading_pattern, html_content, ignore.case = TRUE)
      match_data = regmatches(html_content, matches)[[1]]
      
      if (length(match_data) > 0) {
        for (i in seq_along(match_data)) {
          original_heading = match_data[i]
          
          # Extract the heading text
          heading_text = gsub("<h[1-6][^>]*>([^<]+)</h[1-6]>", "\\1", original_heading, ignore.case = TRUE)
          
          # Create clean ID
          clean_id = gsub("[^a-zA-Z0-9\\s-]", "", heading_text)
          clean_id = gsub("\\s+", "-", trimws(clean_id))
          clean_id = tolower(clean_id)
          
          # Create new heading with ID
          new_heading = gsub(
            "<h([1-6])([^>]*)>([^<]+)</h([1-6])>",
            paste0("<h\\1\\2 id='heading-", clean_id, "'>\\3</h\\4>"),
            original_heading,
            ignore.case = TRUE
          )
          
          # Replace in content
          html_content = gsub(original_heading, new_heading, html_content, fixed = TRUE)
        }
      }
      
      return(html_content)
    }

    question_item_servers = shiny::reactiveValues()
    for(name in question_names) {
      question_item_servers[[name]] = list()
    }

    redraw_ui = shiny::reactiveVal(0)

    shiny::observe({
      shiny::updateSelectInput(
        session, "question_select", 
        choices = question_names, selected = question_names[1]
      )
    })
    
    output$rubric_items_ui = shiny::renderUI({
      req(input$question_select)

      uis = lapply(question_item_servers[[input$question_select]], function(server) {
        mark_rubric_item_ui(session$ns(server$id), server$item())
      })
      
      return(shiny::tagList(uis))
    }) |>
      bindEvent(redraw_ui(), input$question_select)
    
    shiny::observe({
      if (!is.null(on_question_change)) {
        on_question_change(input$question_select)
      }
    }) |> bindEvent(input$question_select)

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
        print(paste("Move up triggered for server:", server$id))
        server_list = question_item_servers[[input$question_select]]
        server_names = names(server_list)
        print(paste("Server names before move:", paste(server_names, collapse = ", ")))
        current_index = which(server_names == server$id)
        print(paste("Current index:", current_index))
        
        if (length(current_index) > 0 && length(server_names) > 1) {
          if (current_index == 1) {
            # Moving up from top - move item to end of list
            print("Moving item from top to end")
            new_order = c(2:length(server_list), 1)
          } else {
            # Normal move up - swap with previous item
            new_index = current_index - 1
            print(paste("New index:", new_index))
            new_order = seq_along(server_list)
            new_order[c(current_index, new_index)] = new_order[c(new_index, current_index)]
          }
          
          # Reorder the server list
          new_server_list = server_list[new_order]
          names(new_server_list) = server_names[new_order]
          
          # Update the list with new order
          question_item_servers[[input$question_select]] = new_server_list
          print(paste("Server names after move:", paste(names(new_server_list), collapse = ", ")))
          
          # Update all hotkeys to maintain sequence
          for (i in seq_along(new_server_list)) {
            srv = new_server_list[[i]]
            current_item = srv$item()
            new_hotkey = if (i <= 10) as.integer(i) else NA_integer_
            print(paste("Updating server", names(new_server_list)[i], "with hotkey", new_hotkey))
            
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
        print(paste("Move down triggered for server:", server$id))
        server_list = question_item_servers[[input$question_select]]
        server_names = names(server_list)
        print(paste("Server names before move:", paste(server_names, collapse = ", ")))
        current_index = which(server_names == server$id)
        print(paste("Current index:", current_index))
        
        if (length(current_index) > 0 && length(server_names) > 1) {
          if (current_index == length(server_names)) {
            # Moving down from bottom - move item to beginning of list
            print("Moving item from bottom to beginning")
            new_order = c(length(server_list), 1:(length(server_list)-1))
          } else {
            # Normal move down - swap with next item
            new_index = current_index + 1
            print(paste("New index:", new_index))
            new_order = seq_along(server_list)
            new_order[c(current_index, new_index)] = new_order[c(new_index, current_index)]
          }
          
          # Reorder the server list
          new_server_list = server_list[new_order]
          names(new_server_list) = server_names[new_order]
          
          # Update the list with new order
          question_item_servers[[input$question_select]] = new_server_list
          print(paste("Server names after move:", paste(names(new_server_list), collapse = ", ")))
          
          # Update all hotkeys to maintain sequence
          for (i in seq_along(new_server_list)) {
            srv = new_server_list[[i]]
            current_item = srv$item()
            new_hotkey = if (i <= 10) as.integer(i) else NA_integer_
            print(paste("Updating server", names(new_server_list)[i], "with hotkey", new_hotkey))
            
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
        bindEvent(server$delete_signal(), ignoreInit = TRUE)

      id_idx <<- id_idx + 1
      redraw_ui(redraw_ui()+1)
    }) |>
      bindEvent(input$add_item, ignoreInit = TRUE)
    
    # Content tab functionality - populate select input with all repos
    shiny::observe({
      artifact_status_data = artifact_status_reactive()
      
      # Include all repos regardless of artifact status
      all_repos = names(artifact_status_data)
      choices = setNames(all_repos, all_repos)
      shiny::updateSelectInput(session, "content_repo_select", choices = choices)
    })
    
    # Display content based on HTML toggle state
    output$content_display = shiny::renderUI({
      req(input$content_repo_select)
      
      selected_repo = input$content_repo_select
      show_html = input$html_toggle
      
      if (show_html) {
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
          
          # Add IDs to headings for scrolling functionality
          html_content = add_heading_ids(html_content)
          
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
      } else {
        # Raw document mode - show source content with syntax highlighting
        # Get current question highlighting if available
        highlight_ranges = NULL
        current_question = input$question_select
        
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
        
        raw_content = get_raw_document_content(selected_repo, collection, use_qmd, highlight_ranges)
        
        if (!is.null(raw_content)) {
          content_to_display = if (is.list(raw_content)) {
            raw_content$content
          } else {
            raw_content
          }
          
          # Monaco Editor provides its own highlighting
          content_with_highlight = paste0(
            '<div id="raw-content-container">',
            content_to_display,
            '</div>'
          )
          shiny::HTML(content_with_highlight)
        } else {
          file_ext = if (use_qmd) ".qmd" else ".Rmd"
          shiny::p(paste("No", file_ext, "content available for selected repository."), class = "text-muted")
        }
      }
    }) |>
      bindEvent(input$content_repo_select, input$html_toggle, input$question_select, ignoreNULL = FALSE)
    
    # Create scrolling callback function
    scroll_to_question = function(selected_question) {
      req(input$content_repo_select)
      
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
            
            # Clean heading text to match our ID generation
            clean_heading = gsub("[^a-zA-Z0-9\\s-]", "", target_heading)
            clean_heading = gsub("\\s+", "-", trimws(clean_heading))
            clean_heading = tolower(clean_heading)
            target_id = paste0("heading-", clean_heading)
            
            # Create JavaScript array of all target IDs for highlighting
            all_target_ids = sapply(all_hierarchies, function(hierarchy) {
              if (length(hierarchy) > 0) {
                deepest_heading = hierarchy[length(hierarchy)]
                clean_id = gsub("[^a-zA-Z0-9\\s-]", "", deepest_heading)
                clean_id = gsub("\\s+", "-", trimws(clean_id))
                clean_id = tolower(clean_id)
                paste0("heading-", clean_id)
              } else {
                NULL
              }
            })
            all_target_ids = all_target_ids[!is.null(all_target_ids)]
            js_target_ids = paste0("['", paste(all_target_ids, collapse = "', '"), "']")
            
            # JavaScript with scrolling to first and highlighting all
            scroll_js = paste0("
              setTimeout(function() {
                // Check if HTML content container exists (ensures content is loaded)
                var contentContainer = document.getElementById('html-content-container');
                if (!contentContainer) {
                  // Retry after a longer delay if content not loaded
                  setTimeout(arguments.callee, 1000);
                  return;
                }
                
                // Clear any existing highlights
                var previousHighlights = document.querySelectorAll('.section-highlight');
                previousHighlights.forEach(function(el) {
                  el.classList.remove('section-highlight');
                });
                
                // Scroll to first target with headroom
                var firstTarget = document.getElementById('", target_id, "');
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
                var targetIds = ", js_target_ids, ";
                targetIds.forEach(function(targetId) {
                  var targetElement = document.getElementById(targetId);
                  if (targetElement) {
                    // Add highlighting to the heading
                    targetElement.classList.add('section-highlight');
                    
                    // Find and highlight all content until the next heading of same or higher level
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
                      
                      // Add highlight to this element
                      currentElement.classList.add('section-highlight');
                      currentElement = currentElement.nextElementSibling;
                    }
                  }
                });
              }, 800);
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
    }) |> bindEvent(input$question_select)
    
    # Observer to trigger highlighting when content is loaded and question is selected
    shiny::observe({
      req(input$content_repo_select, input$question_select)
      
      # Small delay to ensure HTML content has rendered
      shiny::invalidateLater(1000, session)
      shiny::isolate({
        selected_question = input$question_select
        scroll_to_question(selected_question)
      })
    }) |> 
      shiny::bindEvent(input$content_repo_select, ignoreInit = FALSE)
    
    # Return reactive values for external use
    return(list(
      selected_question = shiny::reactive(input$question_select),
      selected_content_repo = shiny::reactive(input$content_repo_select)
    ))
  })
}