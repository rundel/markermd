# Question names of a template, in template order
#
# template: markermd_template S7 object

template_question_names = function(template) {
  vapply(template@questions, function(q) q@name, character(1))
}

# A template's question by name, or NULL when no question has that name
#
# template: markermd_template S7 object
# name: Character. Question name

template_question = function(template, name) {
  for (q in template@questions) {
    if (q@name == name) {
      return(q)
    }
  }
  NULL
}

# The node ids the template's other questions select, at which this
# question's heading sections stop (see heading_section_indices()): a section
# ends at the next heading another question claims, so a student who promotes
# a heading cannot pull a later question's answer into an earlier question. A
# question's own ids never stop its own sections, and document-scope and file
# questions select nothing. Questions are told apart by id.
#
# questions: A markermd_template, or a list of markermd_question objects
# question: The markermd_question whose stop ids are wanted

other_question_heading_ids = function(questions, question) {
  if (S7::S7_inherits(questions, markermd_template)) {
    questions = questions@questions
  }
  if (is_document_question(question) || is_file_question(question)) {
    return(character(0))
  }
  ids = unlist(lapply(questions, function(q) {
    if (identical(q@id, question@id) || is_document_question(q) || is_file_question(q)) {
      character(0)
    } else {
      q@selected_nodes@node_ids
    }
  }))
  sort(setdiff(unique(as.character(ids)), question@selected_nodes@node_ids))
}

# The q2r/Pandoc id of the selectable node at a flattened index
#
# Headings and id'd divs are selectable; everything else (and a div without an
# explicit id) yields "".
#
# ast: q2r pandoc AST object
# index: Integer. A 1-based flattened node index

node_id_for_index = function(ast, index) {
  node = q2r_flatten(ast)[[index]]$node
  if (S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) {
    return(node@attr@id)
  }
  ""
}

# Flattened node indices of the headings and id'd divs carrying the given ids
#
# ast: q2r pandoc AST object
# ids: Character vector of node ids (header or div ids)

node_ids_to_indices = function(ast, ids) {
  if (length(ids) == 0) {
    return(integer(0))
  }
  records = q2r_flatten(ast)
  hits = vapply(records, function(record) {
    node = record$node
    (S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) &&
      nzchar(node@attr@id) && node@attr@id %in% ids
  }, logical(1))
  sort(which(hits))
}

# Selected ids that name no heading or div in the document: a section the
# student renamed or removed. Validation fails a question's rules when any of
# its sections is missing (see evaluate_question_rules()), so a rule such as
# "lacks content" cannot pass merely because nothing was found to check.
#
# ast: q2r pandoc AST object
# ids: Character vector of selected node ids

unresolved_node_ids = function(ast, ids) {
  if (length(ids) == 0) {
    return(character(0))
  }
  present = vapply(q2r_flatten(ast), function(record) {
    node = record$node
    if (S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) node@attr@id else ""
  }, character(1))
  setdiff(ids, present)
}

# Ids carried by more than one heading or div in the document. Selections are
# matched by id, so such an id would select (and grade) every node that shares
# it; the template editor reports these and refuses to select them.
#
# ast: q2r pandoc AST object

duplicate_node_ids = function(ast) {
  if (is.null(ast)) {
    return(character(0))
  }
  ids = vapply(q2r_flatten(ast), function(record) {
    node = record$node
    if (S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) node@attr@id else ""
  }, character(1))
  ids = ids[nzchar(ids)]
  unique(ids[duplicated(ids)])
}

# A plain-text note per id that more than one heading or div carries (see
# duplicate_node_ids()), naming the nodes, for the template editor's document
# notes
#
# ast: q2r pandoc AST object

duplicate_node_id_notes = function(ast) {
  records = q2r_flatten(ast)
  vapply(duplicate_node_ids(ast), function(id) {
    labels = character(0)
    for (record in records) {
      node = record$node
      if ((S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) &&
          identical(node@attr@id, id)) {
        labels = c(labels, record$label)
      }
    }
    paste0(
      "The id #", id, " is used by ", length(labels), " nodes: ", paste(labels, collapse = "; "), ". ",
      "Questions select sections by id, so this id cannot be selected; give each heading or div a unique {#id}."
    )
  }, character(1), USE.NAMES = FALSE)
}

# Record indices of the blocks a sections question counts, in document order:
# the selected headings' sections (heading_section_indices()) plus each
# selected div's child blocks. A selected div already counted whole, because
# it (or a div enclosing it) is one of the section's blocks or a child of an
# outer selected div, is drilled into in place: the enclosing chain down to
# the selected div is replaced by its children. A heading plus a div in its
# section therefore counts the div's content once and keeps it visible to the
# rules, rather than counting the div whole and its children again.
#
# records: q2r_flatten() output for the AST
# heading_ids: Character vector of selected heading ids
# div_ids: Character vector of selected div ids
# stop_ids: Optional ids of headings other questions select, at which the
#   heading sections stop (see other_question_heading_ids())

question_block_indices = function(records, heading_ids, div_ids, stop_ids = NULL) {
  selected = heading_section_indices(records, heading_ids, stop_ids)
  if (length(div_ids) == 0) {
    return(selected)
  }

  container_of = vapply(records, function(r) as.integer(r$container), integer(1))
  children = function(i) which(container_of == i)
  div_index = vapply(div_ids, function(did) {
    hit = which(vapply(records, function(r) {
      S7::S7_inherits(r$node, q2r::pandoc_div) && identical(r$node@attr@id, did)
    }, logical(1)))
    if (length(hit) == 0) NA_integer_ else hit[1]
  }, integer(1))

  for (d in sort(unique(div_index[!is.na(div_index)]))) {
    path = d
    parent = container_of[d]
    while (parent != 0L) {
      path = c(parent, path)
      parent = container_of[parent]
    }
    hit = which(path %in% selected)
    if (length(hit) == 0) {
      selected = c(selected, children(d))
    } else {
      for (p in path[hit[1]:length(path)]) {
        selected = c(setdiff(selected, p), children(p))
      }
    }
  }

  sort(unique(selected))
}

# Problems with loaded questions' node selections against the editor's
# document: ids that match no heading or div (a template written against
# another key), and ids nested under another selected id of the same
# question, which the tree's click handling never produces and which count
# their content twice. Returns one message per question with a problem.
#
# ast: q2r pandoc AST object of the editor's document
# questions: List of markermd_question objects

question_node_id_problems = function(ast, questions) {
  if (!any(vapply(questions, function(q) length(q@selected_nodes@node_ids) > 0, logical(1)))) {
    return(character(0))
  }
  records = q2r_flatten(ast)
  record_ids = vapply(records, function(record) {
    node = record$node
    if (S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) node@attr@id else ""
  }, character(1))
  tree_items = NULL
  out = character(0)
  for (q in questions) {
    ids = q@selected_nodes@node_ids
    if (length(ids) == 0) next
    found = ids %in% record_ids[nzchar(record_ids)]
    unknown = ids[!found]
    nested = character(0)
    if (any(found)) {
      if (is.null(tree_items)) tree_items = build_ast_tree_structure(ast)
      indices = which(record_ids %in% ids[found])
      for (idx in indices) {
        if (has_selected_ancestor(tree_items, idx, setdiff(indices, idx))) {
          nested = c(nested, record_ids[idx])
        }
      }
    }
    parts = c(
      if (length(unknown) > 0) paste0("not in the document: ", paste0("#", unknown, collapse = ", ")),
      if (length(nested) > 0) paste0("inside another selected section: ", paste0("#", unique(nested), collapse = ", "))
    )
    if (length(parts) > 0) {
      out = c(out, paste0("\"", q@name, "\" selects nodes ", paste(parts, collapse = "; and nodes ")))
    }
  }
  out
}

# Partition selected ids into heading ids and div ids using the AST
#
# An id carried by a pandoc_div in the document is a div id; everything else is
# treated as a heading id (an id that resolves to nothing matches no blocks
# during extraction; validation reports it, see unresolved_node_ids()).
#
# ast: q2r pandoc AST object
# ids: Character vector of selected node ids

classify_selected_ids = function(ast, ids) {
  records = q2r_flatten(ast)
  div_ids = character(0)
  for (record in records) {
    node = record$node
    if (S7::S7_inherits(node, q2r::pandoc_div) && nzchar(node@attr@id) && node@attr@id %in% ids) {
      div_ids = c(div_ids, node@attr@id)
    }
  }
  list(heading_ids = setdiff(ids, div_ids), div_ids = div_ids)
}

# Flattened indices of the blocks belonging to the selected heading sections
#
# A selected heading contributes itself and its following siblings (within the
# same container) up to the next heading of the same or higher level - its
# section - or, when stop_ids are given, up to the first heading another
# question selects, whatever its level. Blocks after such a stop heading, even
# ones a level below the selected heading, are no longer part of the section:
# a section ends at the stop, it does not resume after the other question's
# part. Sections are resolved at every container level, so a heading nested
# inside a fenced div resolves to that div's sibling blocks rather than silently
# matching nothing. Indices are returned at the granularity of the matched
# section's container: a top-level section yields its top-level block indices (a
# div block counted whole), while an in-div section yields the in-div sibling
# block indices. A div that is itself part of a captured section is included
# whole and not descended into here; a separately selected div inside a
# captured section is drilled into by question_block_indices(), so a heading
# plus a div in its section counts each block once.
#
# records: q2r_flatten() output for the AST
# heading_ids: Character vector of selected heading ids
# stop_ids: Optional character vector of heading ids that end a section (the
#   other questions' selections); the selected ids themselves never stop

heading_section_indices = function(records, heading_ids, stop_ids = NULL) {
  if (length(heading_ids) == 0) {
    return(integer(0))
  }

  is_header = function(node) S7::S7_inherits(node, q2r::pandoc_header)
  stops = setdiff(as.character(stop_ids), heading_ids)
  ends_section = function(sib, level) {
    is_header(sib) && (sib@level <= level || (length(stops) > 0 && nzchar(sib@attr@id) && sib@attr@id %in% stops))
  }

  children = list()
  for (record in records) {
    key = as.character(record$container)
    children[[key]] = c(children[[key]], list(record))
  }

  collect = function(container_key) {
    kids = children[[container_key]]
    out = integer(0)
    k = 1L
    while (k <= length(kids)) {
      record = kids[[k]]
      node = record$node
      if (is_header(node) && nzchar(node@attr@id) && node@attr@id %in% heading_ids) {
        level = node@level
        out = c(out, record$index)
        j = k + 1L
        while (j <= length(kids)) {
          if (ends_section(kids[[j]]$node, level)) {
            break
          }
          out = c(out, kids[[j]]$index)
          j = j + 1L
        }
        k = j
      } else {
        if (S7::S7_inherits(node, q2r::pandoc_div)) {
          out = c(out, collect(as.character(record$index)))
        }
        k = k + 1L
      }
    }
    out
  }

  sort(unique(collect("0")))
}

# Evaluate "has between" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_between = function(nodes, rule) {
  actual_count = length(nodes)
  min_count = rule@values[1]
  max_count = rule@values[2]
  
  passed = actual_count >= min_count && actual_count <= max_count
  message = if (passed) {
    cli::pluralize("count check passed: {actual_count} node{?s} (expected {min_count}-{max_count})")
  } else {
    cli::pluralize("count check failed: {actual_count} node{?s} (expected {min_count}-{max_count})")
  }
  
  list(passed = passed, message = message)
}

# Evaluate "has at least" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_at_least = function(nodes, rule) {
  actual_count = length(nodes)
  min_count = rule@values
  
  passed = actual_count >= min_count
  message = if (passed) {
    cli::pluralize("count check passed: {actual_count} node{?s} (expected \u2265 {min_count})")
  } else {
    cli::pluralize("count check failed: {actual_count} node{?s} (expected \u2265 {min_count})")
  }
  
  list(passed = passed, message = message)
}

# Evaluate "has at most" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_at_most = function(nodes, rule) {
  actual_count = length(nodes)
  max_count = rule@values
  
  passed = actual_count <= max_count
  message = if (passed) {
    cli::pluralize("count check passed: {actual_count} node{?s} (expected \u2264 {max_count})")
  } else {
    cli::pluralize("count check failed: {actual_count} node{?s} (expected \u2264 {max_count})")
  }
  
  list(passed = passed, message = message)
}

# Check whether any node's extracted text matches a regular expression
#
# Shared node-loop used by the content and name rule evaluators (the rule UI
# labels these fields "regex pattern" / "name (regex)"). Each node's text is
# obtained via extract_fn and tested against pattern as a case-insensitive
# regular expression. Callers check the pattern compiles first (see
# invalid_pattern_result()), so an uncompilable pattern never reaches grepl().
#
# nodes: List of AST nodes
# pattern: Regular expression to match against each node's extracted text
# extract_fn: Function taking a single node and returning its text to match

evaluate_pattern_in_nodes = function(nodes, pattern, extract_fn) {
  for (node in nodes) {
    node_text = extract_fn(node)

    if (nchar(node_text) > 0 && grepl(pattern, node_text, ignore.case = TRUE)) {
      return(TRUE)
    }
  }

  FALSE
}

# Extract a node's rendered document content as a single string

extract_node_content = function(node) {
  node_to_qmd(node) |>
    as.character() |>
    paste(collapse = " ")
}

# Extract a node's name (heading title or chunk label), or "" when it has none

extract_node_name = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc_header)) {
    q2r::ast_text(node)
  } else if (S7::S7_inherits(node, q2r::pandoc_code_block) && q2r::is_code_cell(node)) {
    label = q2r::cell_label(node)
    if (is.na(label)) "" else label
  } else {
    ""
  }
}

# The failing result for a pattern that does not compile as a regular
# expression. Every pattern verb reports it the same way, and as a failure, so
# that "lacks content" cannot pass because its pattern could not be tried.
#
# pattern: Character scalar

invalid_pattern_result = function(pattern) {
  list(passed = FALSE, message = paste0("invalid regular expression: '", pattern, "'"))
}

# Evaluate "has content" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_content = function(nodes, rule) {
  pattern = rule@values[1]
  if (is.na(pattern) || nchar(pattern) == 0) {
    return(list(passed = TRUE, message = "empty pattern always matches"))
  }
  if (!is_valid_regex(pattern)) {
    return(invalid_pattern_result(pattern))
  }

  content_found = evaluate_pattern_in_nodes(nodes, pattern, extract_node_content)

  message = if (content_found) {
    paste0("content check passed: '", pattern, "' found")
  } else {
    paste0("content check failed: '", pattern, "' not found")
  }

  list(passed = content_found, message = message)
}

# Evaluate "lacks content" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_lacks_content = function(nodes, rule) {
  pattern = rule@values[1]
  if (is.na(pattern) || nchar(pattern) == 0) {
    return(list(passed = TRUE, message = "empty pattern never matches"))
  }
  if (!is_valid_regex(pattern)) {
    return(invalid_pattern_result(pattern))
  }

  content_found = evaluate_pattern_in_nodes(nodes, pattern, extract_node_content)

  passed = !content_found
  message = if (passed) {
    paste0("negative content check passed: '", pattern, "' not found")
  } else {
    paste0("negative content check failed: '", pattern, "' found")
  }

  list(passed = passed, message = message)
}

# Evaluate "has name" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_name = function(nodes, rule) {
  pattern = rule@values[1]
  if (is.na(pattern) || nchar(pattern) == 0) {
    return(list(passed = TRUE, message = "empty pattern always matches"))
  }
  if (!is_valid_regex(pattern)) {
    return(invalid_pattern_result(pattern))
  }

  name_found = evaluate_pattern_in_nodes(nodes, pattern, extract_node_name)

  message = if (name_found) {
    paste0("name check passed: '", pattern, "' found")
  } else {
    paste0("name check failed: '", pattern, "' not found")
  }

  list(passed = name_found, message = message)
}

# Apply individual rule to an AST subset and determine if the rule condition is met
#
# ast: q2r pandoc AST or list of nodes to evaluate
# rule: markermd_rule S7 object containing the rule definition

evaluate_rule = function(ast, rule) {

  stopifnot(S7::S7_inherits(rule, markermd_rule))

  # Convert ast to nodes if needed
  nodes = if (S7::S7_inherits(ast, q2r::pandoc)) {
    ast@blocks@content
  } else {
    ast
  }

  stopifnot(is.list(nodes))

  # Apply node type filtering unless "Any node" is among the selected types.
  # The selected types are combined as a logical OR (kinds match the tree labels).
  if (!("Any node" %in% rule@node_type)) {
    nodes = nodes[vapply(nodes, function(n) q2r_node_kind(n) %in% rule@node_type, logical(1))]
  }

  # Delegate to specific rule evaluation functions
  switch(rule@verb,
    "has between" = evaluate_rule_has_between(nodes, rule),
    "has at least" = evaluate_rule_has_at_least(nodes, rule),
    "has at most" = evaluate_rule_has_at_most(nodes, rule),
    "has content" = evaluate_rule_has_content(nodes, rule),
    "lacks content" = evaluate_rule_lacks_content(nodes, rule),
    "has name" = evaluate_rule_has_name(nodes, rule),
    # Default case for unknown verbs
    list(passed = FALSE, message = paste0("Unknown rule verb: ", rule@verb))
  )
}

# Whether a sections question has rules that target nothing: it selects no
# nodes, so its rules cannot pass. A document-scope question never does (it
# covers the whole document) and a question without rules has nothing to
# target.
#
# question: markermd_question S7 object

rules_target_nothing = function(question) {
  length(question@rules) > 0 &&
    length(question@selected_nodes@node_ids) == 0 &&
    !is_document_question(question)
}

# Evaluate a question's rules against a document: the one implementation
# behind the template editor's live badges and validate_question_rules(), so
# the two can never disagree on what a rule targets. Returns one
# evaluate_rule() result (passed / message) per rule, in rule order; a
# sections question whose rules target nothing (rules_target_nothing()) gets a
# failing "No nodes selected" result for each rule instead of being evaluated
# against the whole document, which is only meant for a document-scope
# question, and a question with a selected section the document lacks (a
# renamed or removed heading) fails every rule with a "not found" message, so
# a negative rule cannot pass on an empty node set.
#
# current_ast: q2r pandoc AST object from the document to validate
# question: markermd_question S7 object containing rules and node selections
# stop_ids: Optional ids the template's other questions select, where this
#   question's heading sections end (see other_question_heading_ids())

evaluate_question_rules = function(current_ast, question, stop_ids = NULL) {
  rules = question@rules
  if (length(rules) == 0) {
    return(list())
  }
  if (rules_target_nothing(question)) {
    return(lapply(rules, function(rule) list(passed = FALSE, message = "No nodes selected")))
  }
  missing = if (is_document_question(question)) character(0) else unresolved_node_ids(current_ast, question@selected_nodes@node_ids)
  if (length(missing) > 0) {
    message = paste0(
      "Selected section", if (length(missing) > 1) "s" else "", " ",
      paste0("#", missing, collapse = ", "), " not found in this document"
    )
    return(lapply(rules, function(rule) list(passed = FALSE, message = message)))
  }
  question_ast = get_question_ast(current_ast, question, stop_ids)
  lapply(rules, function(rule) evaluate_rule(question_ast, rule))
}

# Validate all rules for a single question against a new AST using section-based matching
#
# repo_ast: q2r pandoc AST object from the document to validate
# question: markermd_question S7 object containing rules and node selections
# stop_ids: Optional ids where the question's heading sections end (see
#   other_question_heading_ids())

validate_question_rules = function(repo_ast, question, stop_ids = NULL) {

  # Selected nodes (by id) for details display; a document-scope question
  # covers the whole document
  node_ids = question@selected_nodes@node_ids
  formatted_hierarchies = if (is_document_question(question)) {
    "(whole document)"
  } else if (length(node_ids) > 0) {
    paste0("#", node_ids)
  } else {
    "(none)"
  }
  missing = if (is_document_question(question)) character(0) else unresolved_node_ids(repo_ast, node_ids)
  section_line = paste0("Selected section(s): ", paste(formatted_hierarchies, collapse = ", "))
  if (length(missing) > 0) {
    section_line = c(section_line, paste0("Not found in this document: ", paste0("#", missing, collapse = ", ")))
  }

  # If no rules, consider it a pass. messages/passed stay length-aligned with
  # each other (length 1) even though question@rules is empty, so the mark-side
  # create_rule_details() loop never indexes the empty rules list.
  if (length(question@rules) == 0) {
    return(list(
      question_name = question@name,
      status = "pass",
      messages = "No rules defined - validation passed",
      passed = TRUE,
      details = paste(section_line, collapse = "\n")
    ))
  }

  rule_results = evaluate_question_rules(repo_ast, question, stop_ids)
  passed = vapply(rule_results, function(r) isTRUE(r$passed), logical(1))
  messages = vapply(rule_results, function(r) r$message, character(1))

  list(
    question_name = question@name,
    status = if (all(passed)) "pass" else "fail",
    messages = messages,
    passed = passed,
    details = paste(c(section_line, messages), collapse = "\n")
  )
}

# Stop with a clear message if a loaded template predates the q2r migration
#
# Templates created with earlier versions of markermd store a parsermd rmd_ast
# as their original_ast; the current code needs a q2r pandoc AST.
#
# template: markermd_template S7 object

assert_template_compatible = function(template) {
  if (!S7::S7_inherits(template@original_ast, q2r::pandoc)) {
    cli::cli_abort("This template was created with a previous version of markermd and is no longer compatible: its stored document AST predates the move to the q2r parser. Please recreate the template with template().")
  }

  version = template@metadata@version
  if (is.na(version) || utils::compareVersion(version, markermd_template_min_version()) < 0) {
    cli::cli_abort("This template was created with an older version of markermd and is no longer compatible. Please recreate the template with template().")
  }
  if (utils::compareVersion(version, markermd_template_version()) > 0) {
    cli::cli_abort("This template's format_version '{version}' is newer than this version of markermd understands ({markermd_template_version()}). Update markermd to load it.")
  }
}

# Validates a parsed repository AST against template rules using section-based
# matching (each question's heading sections end at the headings the other
# questions select); a question on one of the template's other files passes
# when the repository has the file (see validate_file_question())
#
# ast: q2r pandoc AST object from parsing a repository document
# template: markermd_template S7 object with questions and rules
# repo_dir: The repository directory, where file questions look for their
#   file (NULL fails every file question: there is nowhere to look)

validate_repo_against_rules = function(ast, template, repo_dir = NULL) {

  stopifnot(S7::S7_inherits(template, markermd_template))

  results = list()

  for (question in template@questions) {
    result = if (is_file_question(question)) {
      validate_file_question(question, repo_dir)
    } else {
      validate_question_rules(ast, question, other_question_heading_ids(template, question))
    }
    results[[question@name]] = result
  }

  return(results)
}

# All flatten-record indices nested (at any depth) beneath the given record,
# following real AST containment (the records' container field)
#
# container_of: Integer vector of immediate-container record indices (0 = none)
# index: Record index whose nested descendants to collect

record_descendant_indices = function(container_of, index) {
  out = integer(0)
  frontier = which(container_of == index)
  while (length(frontier) > 0) {
    out = c(out, frontier)
    frontier = which(container_of %in% frontier)
  }
  out
}

# Tree indices (q2r_flatten index space) of the nodes a question's filters
# exclude, for drawing them red in the template tree
#
# Pairs each countable block of the question's node set (the same blocks
# get_question_ast() collects: section blocks for selected headings, child
# blocks for selected divs) with its flatten index via the records' block_pos
# and container fields, applies the filter expression once via
# select_children, and returns the indices of the dropped blocks. A dropped
# block also drops its real nested children (they leave the question AST with
# it); a dropped heading does not color its section's content, which is
# tested independently (containment never crosses sections). A document-scope
# question selects every top-level block, matching get_question_ast(), which
# applies its filters to the whole document. Returns integer(0) when the
# question has no effective filters or no selection.
#
# current_ast: q2r pandoc AST object from the document to analyze
# question: markermd_question S7 object containing selected nodes and filters
# stop_ids: Optional ids where the question's heading sections end (see
#   other_question_heading_ids())

question_filtered_indices = function(current_ast, question, stop_ids = NULL) {

  expr = filters_expr(question@filters)
  ids = question@selected_nodes@node_ids
  whole_document = is_document_question(question)
  if (is.null(current_ast) || is.null(expr) || (length(ids) == 0 && !whole_document)) {
    return(integer(0))
  }

  records = q2r_flatten(current_ast)
  container_of = vapply(records, function(r) as.integer(r$container), integer(1))

  node_indices = if (whole_document) {
    which(container_of == 0L)
  } else {
    split = classify_selected_ids(current_ast, ids)
    question_block_indices(records, split$heading_ids, split$div_ids, stop_ids)
  }
  nodes = lapply(node_indices, function(i) records[[i]]$node)

  if (length(nodes) == 0) {
    return(integer(0))
  }

  kept = rlang::inject(q2r::select_children(
    q2r::pandoc(blocks = q2r::pandoc_blocks(nodes)), !!expr
  ))

  # kept is an ordered subsequence of nodes, and the predicate is a pure
  # function of node content (identical nodes share a result), so greedy
  # matching recovers the per-block keep flags
  keep = logical(length(nodes))
  j = 1L
  for (i in seq_along(nodes)) {
    if (j <= length(kept) && identical(nodes[[i]], kept[[j]])) {
      keep[i] = TRUE
      j = j + 1L
    }
  }

  out = integer(0)
  for (i in which(!keep)) {
    out = c(out, node_indices[i], record_descendant_indices(container_of, node_indices[i]))
  }

  # A node countable through two routes (e.g. a div child also in a selected
  # section) stays green when its own keep flag says so
  out = setdiff(out, node_indices[keep])

  sort(unique(out))
}

# Narrow a question's node set by its filters
#
# Applies the question's filter expression (see filters_expr) to the top-level
# blocks only, via q2r::select_children - the same node set rules count - so
# filtering never surfaces nested or inline nodes as countable. Returns the
# AST unchanged when the question has no effective filters. Invalid predicate
# values (e.g. a bad regex) surface as q2r_predicate_error warnings from q2r
# and the condition evaluates as no-match; they are deliberately not caught.
#
# question_ast: q2r pandoc AST object holding the question's node set
# question: markermd_question S7 object containing filter groups

apply_question_filters = function(question_ast, question) {

  expr = filters_expr(question@filters)
  if (is.null(expr)) {
    return(question_ast)
  }

  kept = rlang::inject(q2r::select_children(question_ast, !!expr))
  q2r::pandoc(blocks = q2r::pandoc_blocks(kept))
}

# Extracts the AST subset for a specific question by node-id matching
#
# Heading ids match by section (heading_section_indices()): the heading plus
# its following sibling blocks up to the next heading of the same or higher
# level or the first heading another question selects (stop_ids), nested
# subsections included, resolved in whatever container the
# heading sits (top level or inside a div). Div ids drill in: the selected
# div's own child blocks are contributed, making nested content visible to
# rule evaluation; a div already counted whole by a selected section or an
# enclosing selected div is drilled into in place (question_block_indices()),
# so the two are unioned without double counting. An empty selection keeps the whole
# document. The question's filters (if any) then narrow the resulting node set,
# so every consumer - live rule badges, headless validation, and the mark-side
# question content display - sees the filtered set.
#
# current_ast: q2r pandoc AST object from the document to analyze
# question: markermd_question S7 object containing selected nodes and filters
# stop_ids: Optional ids of the headings the template's other questions
#   select, where this question's heading sections end (see
#   other_question_heading_ids()); NULL ends them only at the next heading of
#   the same or higher level

get_question_ast = function(current_ast, question, stop_ids = NULL) {

  stopifnot(S7::S7_inherits(question, markermd_question))

  # A question on another file has no part of the document
  if (is_file_question(question)) {
    return(empty_pandoc_ast())
  }

  ids = question@selected_nodes@node_ids

  if (length(ids) == 0) {
    return(apply_question_filters(current_ast, question))
  }

  records = q2r_flatten(current_ast)
  split = classify_selected_ids(current_ast, ids)

  indices = question_block_indices(records, split$heading_ids, split$div_ids, stop_ids)
  result_blocks = lapply(indices, function(i) records[[i]]$node)

  apply_question_filters(q2r::pandoc(blocks = q2r::pandoc_blocks(result_blocks)), question)
}

# Tree indices (q2r_flatten index space) a sections question covers: the
# blocks question_block_indices() counts plus everything nested in them, and
# the selected nodes themselves. The template tree highlights this set when the
# question's sections stop at other questions' headings, so the editor shows
# exactly what grading takes; without stop ids the tree's own parent-child
# walk (compute_all_selected_nodes()) gives the same set.
#
# current_ast: q2r pandoc AST object
# ids: Character vector of selected node ids
# stop_ids: Ids where heading sections end (see other_question_heading_ids())

question_covered_indices = function(current_ast, ids, stop_ids = NULL) {
  if (is.null(current_ast) || length(ids) == 0) {
    return(integer(0))
  }
  records = q2r_flatten(current_ast)
  container_of = vapply(records, function(r) as.integer(r$container), integer(1))
  split = classify_selected_ids(current_ast, ids)
  blocks = question_block_indices(records, split$heading_ids, split$div_ids, stop_ids)
  direct = node_ids_to_indices(current_ast, ids)
  roots = sort(unique(c(direct, blocks)))
  nested = unlist(lapply(roots, function(i) record_descendant_indices(container_of, i)))
  sort(unique(as.integer(c(roots, nested))))
}
