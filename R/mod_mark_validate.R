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
    style = "max-height: 160px;",
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

  # Parse rule messages to understand individual rule results. A question can
  # have more messages than rules (the no-rules pass message), so guard the
  # rules index rather than dereferencing an out-of-range element.
  rule_items = lapply(seq_along(question_result$messages), function(i) {
    message = question_result$messages[i]
    rule = if (i <= length(question@rules)) question@rules[[i]] else NULL

    # Determine if this rule passed based on message content
    rule_passed = question_result$passed[i]
    rule_color = if (rule_passed) "#28a745" else "#dc3545"
    rule_icon = if (rule_passed) "check" else "times"
    
    shiny::div(
      style = paste0("margin: 6px 0; padding: 8px; border-left: 3px solid ", rule_color, "; background-color: #f8f9fa; border-radius: 3px;"),
      shiny::div(
        style = "display: flex; align-items: center;",
        shiny::icon(rule_icon, class = "fa-fw", style = paste0("color: ", rule_color, "; margin-right: 8px; font-size: 14px;")),
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
  
  # Solid circle status icons - only pass/fail states
  status_icon = switch(question_result$status,
    "pass" = shiny::icon("circle-check", class = "fa-fw text-success"),
    "fail" = shiny::icon("circle-xmark", class = "fa-fw text-danger"),
    shiny::icon("circle-question", class = "fa-fw text-muted")  # Default for unknown status
  )
  
  # Get the document nodes for this question using section selection
  question_nodes_content = get_question_content(current_ast, question, session)
  
  # Create rule details
  rule_details = create_rule_details(question, question_result)
  
  # Create question card. mb-0 drops bslib's default 1rem card margin so the
  # column gap set in the container controls the spacing between cards.
  bslib::card(
    class = "mb-0",
    bslib::card_header(
      class = "bg-light d-flex align-items-center",
      shiny::span(status_icon, class = "me-2"),
      shiny::span(question_name, class = "my-0 text-dark fw-semibold")
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
  
  # Scrolling is handled by the enclosing card_body, so no height math here
  shiny::div(
    id = ns("template_validation"),
    shiny::uiOutput(ns("template_validation_ui"))
  )
}

# Mark validation server
#
# id: Character. Module namespace ID
# ast: Reactive. The parsed AST object
# validation_results: Reactive. Validation results for current repository
# template: Reactive. Template object for reference

mark_validate_server = function(id, ast, validation_results = shiny::reactiveVal(NULL), template = shiny::reactiveVal(NULL)) {
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
          style = "display: flex; flex-direction: column; gap: 8px;",
          question_cards
        )
      }
    })
    
    # Wire preview-modal observers per question. Each observer resolves its node
    # from the CURRENT repo's AST at click time (not the repo that happened to be
    # active when it was wired), so navigating between repos previews the right
    # content. We track how many buttons have been wired per question and only
    # wire newly appearing indices, so a repo with more nodes extends the set
    # without ever creating duplicate observers. The button ids
    # (preview_<question_id>_<index>) match those drawn by render_ast_tree() /
    # node_preview_button() in get_question_content().
    wired_count = shiny::reactiveValues()

    question_node_at = function(question) {
      function(index) {
        question_ast = get_question_ast(ast(), question)
        records = if (is.null(question_ast)) list() else q2r_flatten(question_ast)
        if (index <= length(records)) records[[index]]$node else NULL
      }
    }

    shiny::observe({
      shiny::req(template(), ast())

      for (question in template()@questions) {
        question_id = paste0("q_", gsub("[^A-Za-z0-9]", "", question@name))

        question_ast = get_question_ast(ast(), question)
        n = if (is.null(question_ast)) 0L else length(q2r_flatten(question_ast))

        already = wired_count[[question_id]]
        already = if (is.null(already)) 0L else already

        if (n > already) {
          local({
            ast_preview_observers(
              input, nodes = NULL, id_prefix = question_id,
              node_at = question_node_at(question),
              indices = seq.int(already + 1L, n)
            )
          })
          wired_count[[question_id]] = n
        }
      }
    })
    
    return(list())
  })
}