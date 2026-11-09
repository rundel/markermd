# Shiny module for displaying validation results during marking

# DOM id (before namespacing) of a question's validation card, the target of
# the question shortcut select; question_dom_id() keeps two questions whose
# names differ only in punctuation apart
#
# question_name: Character. Question name

question_card_id = function(question_name) {
  paste0("qcard_", question_dom_id(question_name))
}

# Choices for the question shortcut select: the question names as values,
# labeled with a check or cross for the repository's validation status
# (plain names when the repository has no results, e.g. a parse error)
#
# question_names: Character vector of question names in template order
# results: The repository's per-question validation results, or NULL
# Returns: Named character vector (labels as names) for selectInput choices

validation_question_choices = function(question_names, results = NULL) {
  labels = vapply(question_names, function(question_name) {
    status = results[[question_name]]$status
    if (is.null(status)) {
      question_name
    } else if (identical(status, "pass")) {
      paste0("\u2713 ", question_name)
    } else {
      paste0("\u2717 ", question_name)
    }
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(question_names, labels)
}

# Selectize render functions for the question shortcut select: the leading
# check or cross of a label is colored green or red, in the dropdown and in
# the selected item alike. A custom render must supply selectize's own
# option / item classes itself (its default templates do), or the entries
# lose their padding and hover styling. Evaluated by shiny's selectize
# binding (the I() wrapper marks it as JavaScript).

validation_select_render_js = function() {
  I("(function() {
    function render(cls) {
      return function(item, escape) {
        var label = item.label, mark = label.charAt(0), color = null;
        if (mark === '\\u2713') color = 'text-success';
        else if (mark === '\\u2717') color = 'text-danger';
        if (color === null) return '<div class=\"' + cls + '\">' + escape(label) + '</div>';
        return '<div class=\"' + cls + '\"><span class=\"' + color + ' fw-semibold\">' + escape(mark) + '</span>' + escape(label.slice(1)) + '</div>';
      };
    }
    return {option: render('option'), item: render('item')};
  })()")
}

# The question shortcut select, placed by the app in the validation modal's
# header but namespaced to this module, whose server keeps its labels current
# and scrolls to the chosen question's card. It starts empty (a placeholder)
# on every opening, so only an actual choice scrolls and expands a card.
#
# id: Character. Module namespace ID
# choices: Initial choices from validation_question_choices()

mark_validate_question_select_ui = function(id, choices) {
  ns = shiny::NS(id)
  shiny::selectizeInput(
    ns("question_select"), NULL, choices = choices, width = "100%",
    options = list(
      render = validation_select_render_js(),
      placeholder = "Jump to question",
      onInitialize = I("function() { this.setValue(''); }")
    )
  )
}

# Creates the content display for a question including tree structure
#
# repo_ast: The current document AST
# question: The question object with selected headings
# session: Shiny session object

get_question_content = function(repo_ast, question, session) {

  question_ast = get_question_ast(repo_ast, question)
  tree_items = build_ast_tree_structure(question_ast)
  question_id = paste0("q_", question_dom_id(question@name))

  # The card collapses as a whole, so the tree is shown in full rather than
  # capped in height
  shiny::div(
    class = "mb-1 p-2 bg-light border rounded overflow-auto small",
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
        style = "display: flex; align-items: center; flex-wrap: wrap; row-gap: 2px;",
        fa_icon(rule_icon, class = "fa-fw", style = paste0("color: ", rule_color, "; margin-right: 8px; font-size: 14px;")),
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

# Creates a single question validation card. Its header holds the question's
# status and its rule results, so the summary shows while the card is
# collapsed, plus a chevron that collapses or expands the body, the matched
# document content (a Bootstrap collapse, so no server round trip); the
# body's data-question attribute lets the collapse state be reported back
# per question.
#
# question: The question object
# question_result: The validation result for this question
# current_ast: The current document AST
# session: Shiny session object
# collapsed: Logical. Whether the card starts collapsed

create_question_card = function(question, question_result, current_ast, session, collapsed = TRUE) {
  question_name = question@name
  card_id = session$ns(question_card_id(question_name))
  body_id = paste0(card_id, "_body")
  
  # Overall status styling - only pass/fail states
  status_color = switch(question_result$status,
    "pass" = "#28a745",
    "fail" = "#dc3545",
    "#6c757d"  # Default for unknown status
  )
  
  # Solid circle status icons - only pass/fail states
  status_icon = switch(question_result$status,
    "pass" = fa_icon("circle-check", class = "fa-fw text-success"),
    "fail" = fa_icon("circle-xmark", class = "fa-fw text-danger"),
    fa_icon("circle-question", class = "fa-fw text-muted")  # Default for unknown status
  )
  
  header = bslib::card_header(
    class = "bg-light d-flex align-items-center",
    shiny::span(status_icon, class = "me-2"),
    shiny::span(question_name, class = "my-0 text-dark fw-semibold"),
    if (is_file_question(question)) shiny::tags$code(class = "ms-2 small", question@file)
  )
  rule_details = create_rule_details(question, question_result)

  # A question on another file has no document content: its body is the
  # result alone (whether the repository has the file)
  if (is_file_question(question)) {
    return(bslib::card(
      id = card_id,
      class = "mb-0",
      header,
      bslib::card_body(class = "pt-1 pb-1", fillable = FALSE, rule_details)
    ))
  }

  # Get the document nodes for this question using section selection
  question_nodes_content = get_question_content(current_ast, question, session)

  # Create question card: the rule results at the top of the body, and below
  # them the matched document content, collapsed behind a toggle. mb-0 drops
  # bslib's default 1rem card margin so the column gap set in the container
  # controls the spacing between cards. The id is the scroll target of the
  # question shortcut select.
  bslib::card(
    id = card_id,
    class = "mb-0",
    header,
    bslib::card_body(
      class = "pt-1 pb-2",
      fillable = FALSE,
      rule_details,
      shiny::tags$button(
        type = "button",
        class = "btn btn-link btn-sm p-0 text-decoration-none validation-collapse-toggle align-self-start",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", body_id),
        `aria-expanded` = if (collapsed) "false" else "true",
        `aria-controls` = body_id,
        fa_icon("chevron-down", class = "fa-fw"),
        "Matched content"
      ),
      htmltools::div(
        id = body_id,
        class = if (collapsed) "collapse" else "collapse show",
        `data-question` = question_name,
        htmltools::div(class = "pt-2", question_nodes_content)
      )
    )
  )
}

# The validation display's script and styles, placed once in the page head
# (the display itself lives in a modal that is re-created on every opening,
# which would otherwise register the listeners again each time). The script
# serves the question shortcut select (it scrolls a question's card into
# view, expanding it first if collapsed, and retries briefly in case the
# results are still being rendered when the message lands) and reports which
# cards are expanded after every toggle, so the render can keep a question
# expanded across repositories (cards start collapsed).
#
# id: Character. Module namespace ID

mark_validate_script = function(id) {
  ns = shiny::NS(id)
  script = glue::glue("
    Shiny.addCustomMessageHandler('markermd_scroll_to', function(msg) {
      var attempts = 20;
      (function tryScroll() {
        var el = document.getElementById(msg.id);
        if (el) {
          var body = el.querySelector('.collapse');
          if (body && !body.classList.contains('show')) {
            bootstrap.Collapse.getOrCreateInstance(body).show();
          }
          el.scrollIntoView({behavior: 'smooth', block: 'start'});
        } else if (attempts-- > 0) {
          setTimeout(tryScroll, 50);
        }
      })();
    });
    ['shown.bs.collapse', 'hidden.bs.collapse'].forEach(function(type) {
      document.addEventListener(type, function(event) {
        var container = document.getElementById('<<ns(\"template_validation\")>>');
        if (!container || !container.contains(event.target)) return;
        var expanded = [];
        container.querySelectorAll('.collapse.show').forEach(function(el) {
          expanded.push(el.dataset.question);
        });
        Shiny.setInputValue('<<ns(\"expanded_questions\")>>', expanded);
      });
    });
  ", .open = "<<", .close = ">>")

  htmltools::tagList(
    shiny::tags$style(shiny::HTML("
      .validation-collapse-toggle .fa-chevron-down { transition: transform 0.2s ease; }
      .validation-collapse-toggle[aria-expanded='false'] .fa-chevron-down { transform: rotate(-90deg); }
    ")),
    shiny::tags$script(shiny::HTML(script))
  )
}

# Mark validation UI: the per-question results (see mark_validate_script()
# for the script it relies on)
#
# id: Character. Module namespace ID

mark_validate_ui = function(id) {
  ns = shiny::NS(id)
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
# on_back: Optional zero-argument function behind a Back button in the node
#   preview modal (the app re-opens the validation modal the preview replaced)

mark_validate_server = function(id, ast, validation_results = shiny::reactiveVal(NULL), template = shiny::reactiveVal(NULL),
                                on_back = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    back_id = if (is.null(on_back)) NULL else session$ns("preview_back")
    if (!is.null(on_back)) {
      shiny::observe({
        on_back()
      }) |> shiny::bindEvent(input$preview_back)
    }

    
    # Keep the question shortcut select's labels in step with the current
    # repository's results, preserving the chosen question. Re-sending the
    # current value does not re-fire the input, so this never scrolls.
    shiny::observe({
      shiny::req(template())
      shiny::updateSelectInput(
        session, "question_select",
        choices = validation_question_choices(template_question_names(template()), validation_results()),
        selected = shiny::isolate(input$question_select)
      )
    })

    # Choosing a question scrolls its card into view
    shiny::observe({
      shiny::req(nzchar(input$question_select))
      session$sendCustomMessage(
        "markermd_scroll_to",
        list(id = session$ns(question_card_id(input$question_select)))
      )
    }) |> shiny::bindEvent(input$question_select, ignoreInit = TRUE)

    output$template_validation_ui = shiny::renderUI({
      shiny::req(template(), validation_results(), ast())
      
      current_validation = validation_results()
      current_template = template()
      current_ast = ast()

      # Cards start collapsed (the header carries the rule results); those
      # the user expanded stay expanded when the cards are rebuilt for
      # another repository. Isolated so a toggle alone never re-renders.
      expanded = as.character(unlist(shiny::isolate(input$expanded_questions)))
      question_cards = lapply(current_template@questions, function(question) {
        create_question_card(
          question, current_validation[[question@name]],
          current_ast, session,
          collapsed = !question@name %in% expanded
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
        question_id = paste0("q_", question_dom_id(question@name))

        question_ast = get_question_ast(ast(), question)
        n = if (is.null(question_ast)) 0L else length(q2r_flatten(question_ast))

        already = wired_count[[question_id]]
        already = if (is.null(already)) 0L else already

        if (n > already) {
          local({
            ast_preview_observers(
              input, nodes = NULL, id_prefix = question_id,
              node_at = question_node_at(question),
              indices = seq.int(already + 1L, n),
              back_id = back_id
            )
          })
          wired_count[[question_id]] = n
        }
      }
    })
    
    return(list())
  })
}