#' Question Module
#'
#' Shiny module for managing a single question with its rules

#' Question UI
#'
#' @param id Character. Module namespace ID
#' @param name_id Integer. The question ID used for default naming
#'
question_ui = function(id, name_id) {
  ns = shiny::NS(id)
  
  bslib::card(
    style = "margin: 0;",
    bslib::card_header(
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        
        # Question name input
        shiny::div(
          style = "flex-grow: 1; margin-right: 10px;",
          shiny::textInput(
            ns("question_name"),
            NULL,
            value = paste("Question", name_id),
            width = "100%"
          )
        ),
        
        # Delete question button
        shiny::div(
          style = "flex-shrink: 0;",
          shiny::actionButton(
            ns("delete_question"),
            shiny::icon("times"),
            class = "btn-danger btn-sm",
            style = "font-size: 12px; padding: 2px 6px; border-radius: 20%; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; line-height: 1;",
            title = "Delete Question"
          )
        )
      )
    ),
    
    # Question content
    bslib::card_body(
      style = "padding: 12px 12px 8px 12px;",
      
      # Selected nodes display
      shiny::div(
        style = "margin-bottom: 2px;",
        shiny::strong("Selected nodes: "),
        shiny::uiOutput(ns("selected_nodes_display"))
      ),
      
      # Rules section
      shiny::div(
        # Rules header with add button
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: -2px; margin-bottom: 8px;",
          shiny::div(
            shiny::strong("Validation rules: "),
            shiny::uiOutput(ns("rules_status"))
          ),
          shiny::div(
            style = "display: flex; align-items: center;",
            shiny::actionButton(
              ns("add_rule"),
              shiny::icon("plus"),
              class = "btn-outline-primary btn-sm",
              style = "font-size: 10px; padding: 2px 6px;",
              title = "Add Rule"
            ),
            shiny::span("Add Rule", style = "margin-left: 6px; font-size: 12px;")
          )
        ),
        
        # Rules container
        shiny::div(
          id = ns("rules_container"),
          shiny::uiOutput(ns("rules_ui"))
        )
      )
    )
  )
}

#' Question Server
#'
#' @param id Character. Module namespace ID
#' @param question_id Integer. The question ID
#' @param ast Reactive. The parsed AST object for building tree structure
#'
question_server = function(id, ast) {
  shiny::moduleServer(id, function(input, output, session) {
    
    stopifnot(S7::S7_inherits(ast(), parsermd::rmd_ast))

    # Question state
    state = shiny::reactiveVal({
      markermd_question(1L, "default", markermd_node_selection(), list())
    })
    
    # Render selected nodes display
    output$selected_nodes_display = shiny::renderUI({
      nodes = state()@selected_nodes@indices
      if (length(nodes) == 0) {
        shiny::span("None", style = "color: #6c757d;")
      } else {
        tree_items = build_ast_tree_structure(ast())
        
        node_displays = sapply(nodes, function(node_index) {
          children = find_all_descendants(tree_items, node_index)
          if (length(children) > 0) {
            paste0(node_index, " [", paste(children, collapse = ","), "]")
          } else {
            as.character(node_index)
          }
        })
        
        shiny::span(
          paste(node_displays, collapse = ", "), 
          style = "color: #28a745;"
        )
      }
    })
        
    observe({
      req(input$question_name)
      cur_state = state()
      cur_state@name = input$question_name
      state(cur_state)
    }) |>
      bindEvent(input$question_name)
    
    
    
    # Return reactive question data and methods
    return(list(
      # Reactive data
      question = shiny::reactive({
        state()
      }),
      
      # Node management methods
      add_node = function(node_index) {
        cur_state = state()
        cur_nodes = cur_state@selected_nodes@indices
        if (!node_index %in% cur_nodes) {
          cur_state@selected_nodes = markermd_node_selection(indices = sort(c(cur_nodes, node_index)))
          state(cur_state)
        }
      },

      remove_node = function(node_index) {
        cur_state = state()
        cur_state@selected_nodes = markermd_node_selection(indices = setdiff(cur_state@selected_nodes@indices, node_index))
        state(cur_state)
      },
    
      clear_nodes = function() {
        cur_state = state()
        cur_state@selected_nodes = markermd_node_selection(indices = integer())
        state(cur_state)
      },
        
      get_selected_nodes = shiny::reactive({
        state()@selected_nodes@indices
      }),
      
      delete_clicked = shiny::reactive({
        input$delete_question
      })
    ))
    })
}
