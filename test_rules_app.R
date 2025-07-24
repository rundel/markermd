#!/usr/bin/env Rscript

# Minimal test app for debugging rule functionality
library(shiny)
library(bslib)
library(shinyjs)

# Load markermd package
devtools::load_all()

# Create a simple AST for testing (validation disabled in question module)
test_ast = structure(list(), class = "rmd_ast")

# Simple test UI
ui = bslib::page_fluid(
  shinyjs::useShinyjs(),
  
  # Add CSS for rule styling
  tags$style(HTML("
    .rule-item select {
      font-size: 12px !important;
      height: 32px !important;
      padding: 4px 8px !important;
    }
    
    .rule-item .form-control {
      font-size: 12px !important;
      height: 32px !important;
      padding: 4px 8px !important;
    }
    
    .rule-item .form-group {
      margin-bottom: 0 !important;
    }
    
    .rule-item .input-group-addon {
      font-size: 12px !important;
      padding: 4px 8px !important;
      height: 32px !important;
      line-height: 1.2 !important;
    }
    
    .rule-item input[type='number'] {
      font-size: 12px !important;
      height: 32px !important;
      padding: 4px 8px !important;
    }
  ")),
  
  titlePanel("Rule Debugging Test App"),
  
  bslib::layout_columns(
    col_widths = c(12),
    
    # Single question for testing
    div(
      style = "border: 1px solid #ddd; padding: 15px; margin: 10px;",
      h4("Test Question"),
      
      # Use the question module directly
      question_ui("test_question", 1)
    )
  )
)

# Simple test server
server = function(input, output, session) {
  
  # Create the question module server
  question_result = question_server("test_question", reactive(test_ast))
  
  # Debug output
  observe({
    cat("Question state changed:\n")
    q_state = question_result$question()
    cat("- Name:", q_state@name, "\n")
    cat("- Selected nodes:", paste(q_state@selected_nodes@indices, collapse = ", "), "\n") 
    cat("- Rules count:", length(q_state@rules), "\n")
    if (length(q_state@rules) > 0) {
      for (i in seq_along(q_state@rules)) {
        rule = q_state@rules[[i]]
        cat("  Rule", i, ":", rule@node_type, rule@verb, toString(rule@values), "\n")
      }
    }
    cat("---\n")
  })
  
  # Debug button clicks
  observe({
    cat("Add rule button clicks detected:", input$`test_question-add_rule`, "\n")
  })
  
  # Debug delete button clicks  
  observe({
    cat("All inputs:", names(input), "\n")
    
    # Check for delete button inputs specifically
    delete_inputs = names(input)[grepl("delete", names(input))]
    if (length(delete_inputs) > 0) {
      cat("Delete button inputs found:", paste(delete_inputs, collapse = ", "), "\n")
      for (delete_input in delete_inputs) {
        cat(" ", delete_input, "=", input[[delete_input]], "\n")
      }
    } else {
      cat("No delete button inputs found\n")
    }
    cat("---DELETE DEBUG---\n")
  })
  
  # Debug verb changes
  observe({
    verb_inputs = names(input)[grepl("verb", names(input)) & !grepl("verb_inputs", names(input))]
    if (length(verb_inputs) > 0) {
      cat("VERB CHANGES:\n")
      for (verb_input in verb_inputs) {
        cat(" ", verb_input, "=", input[[verb_input]], "\n")
      }
      cat("---VERB DEBUG---\n")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)