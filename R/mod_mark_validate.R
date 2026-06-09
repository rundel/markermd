# Shiny module for displaying validation results during marking

# Creates the content display for a question including tree structure
#
# repo_ast: The current document AST
# question: The question object with selected headings
# session: Shiny session object

get_question_content = function(repo_ast, question, session) {

  question_ast = get_question_ast(repo_ast, question)
  tree_items = build_ast_tree_structure(question_ast)
  question_id = paste0("q_", gsub("[^A-Za-z0-9]", "", question@name))

  shiny::div(
    class = "mb-1 p-2 bg-light border rounded overflow-auto small",
    style = "max-height: 120px;",
    render_ast_tree(
      tree_items,
      session$ns,
      ast_render_opts(
        mode = "readonly",
        id_prefix = question_id,
        start_depth = 1,
        drop_root = TRUE
      )
    )
  )
}

# Creates the rule details section for a question card
#
# question: The question object with rules
# question_result: The validation result for this question

create_rule_details = function(question, question_result) {

  # Parse rule messages to understand individual rule results
  rule_items = lapply(seq_along(question_result$messages), function(i) {
    message = question_result$messages[i]
    rule = question@rules[[i]]
    
    # Determine if this rule passed based on message content
    rule_passed = question_result$passed[i]
    rule_color = if (rule_passed) "#28a745" else "#dc3545"
    rule_icon = if (rule_passed) "check" else "times"
    
    shiny::div(
      style = paste0("margin: 6px 0; padding: 8px; border-left: 3px solid ", rule_color, "; background-color: #f8f9fa; border-radius: 3px;"),
      shiny::div(
        style = "display: flex; align-items: center;",
        shiny::icon(rule_icon, style = paste0("color: ", rule_color, "; margin-right: 6px; font-size: 14px;")),
        if (!is.null(rule)) {
          lapply(rule@node_type, function(nt) {
            shiny::span(
              nt,
              class = "text-muted fw-medium me-2",
              style = "font-size: 11px; padding: 2px 6px; border: 1px solid #dee2e6; border-radius: 6px;"
            )
          })
        },
        shiny::span(
          message,
          style = "font-size: 13px;"
        )
      )
    )
  })
  
  shiny::div(rule_items)
}

# Creates a single question validation card
#
# question: The question object
# question_result: The validation result for this question
# current_ast: The current document AST
# session: Shiny session object

create_question_card = function(question, question_result, current_ast, session) {
  question_name = question@name
  
  # Overall status styling - only pass/fail states
  status_color = switch(question_result$status,
    "pass" = "#28a745",
    "fail" = "#dc3545",
    "#6c757d"  # Default for unknown status
  )
  
  # Create HTML for solid circle icons with white symbols - only pass/fail states
  status_icon_html = switch(question_result$status,
    "pass" = '<i class="fas fa-circle text-success"></i><i class="fas fa-check text-white position-absolute top-50 start-50 translate-middle" style="font-size: 10px;"></i>',
    "fail" = '<i class="fas fa-circle text-danger"></i><i class="fas fa-times text-white position-absolute top-50 start-50 translate-middle" style="font-size: 10px;"></i>',
    '<i class="fas fa-question-circle text-muted"></i>'  # Default for unknown status
  )
  
  # Get the document nodes for this question using section selection
  question_nodes_content = get_question_content(current_ast, question, session)
  
  # Create rule details
  rule_details = create_rule_details(question, question_result)
  
  # Create question card
  bslib::card(
    bslib::card_header(
      class = "bg-light",
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        shiny::span(question_name, class = "my-0 text-dark fw-semibold"),
        shiny::div(
          style = "position: relative; font-size: 18px;",
          shiny::HTML(status_icon_html)
        )
      )
    ),
    bslib::card_body(
      class = "pt-2 pb-1",
      question_nodes_content
    ),
    bslib::card_body(
      class = "pt-0 pb-1",
      rule_details
    )
  )
}

# Mark validation UI
#
# id: Character. Module namespace ID

mark_validate_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::div(
    id = ns("template_validation"),
    style = "max-height: calc(100vh - 200px); overflow-y: auto;",
    shiny::uiOutput(ns("template_validation_ui"))
  )
}

# Mark validation server
#
# id: Character. Module namespace ID
# ast: Reactive. The parsed AST object
# current_repo_name: Reactive. Current repository name
# validation_results: Reactive. Validation results for current repository
# selected_question_name: Reactive. Currently selected question name
# template: Reactive. Template object for reference

mark_validate_server = function(id, ast, current_repo_name = shiny::reactiveVal(NULL), validation_results = shiny::reactiveVal(NULL), selected_question_name = shiny::reactiveVal(NULL), template = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    
    
    output$template_validation_ui = shiny::renderUI({
      shiny::req(template(), validation_results(), ast())
      
      current_validation = validation_results()
      current_template = template()
      current_ast = ast()
      
      question_cards = lapply(current_template@questions, function(question) {
        create_question_card(
          question, current_validation[[question@name]],
          current_ast, session
        )
      })
      
      if (length(question_cards) == 0) {
        shiny::p("No questions available.", class = "text-muted fst-italic")
      } else {
        shiny::div(
          style = "display: flex; flex-direction: column; gap: 3px;",
          question_cards
        )
      }
    })
    
    # Wire preview-modal observers once per question, guarded against duplicates.
    # The button ids (preview_<question_id>_<index>) match those drawn by
    # render_ast_tree() / node_preview_button() in get_question_content().
    created_observers = shiny::reactiveValues()

    shiny::observe({
      shiny::req(template(), ast())

      for (question in template()@questions) {
        question_id = paste0("q_", gsub("[^A-Za-z0-9]", "", question@name))
        if (!is.null(created_observers[[question_id]])) {
          next
        }

        question_ast = get_question_ast(ast(), question)
        question_nodes = if (is.null(question_ast)) list() else lapply(q2r_flatten(question_ast), function(record) record$node)

        if (length(question_nodes) > 0) {
          ast_preview_observers(input, question_nodes, id_prefix = question_id)
          created_observers[[question_id]] = TRUE
        }
      }
    })
    
    # Return empty list for now
    return(list())
  })
}