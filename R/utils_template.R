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

# Locate a div node anywhere in a block tree by its explicit id, or NULL
#
# Recurses into nested divs so a div inside another div is reachable.
#
# blocks: List of q2r pandoc block nodes
# id: Character. The div id to find

find_div_by_id = function(blocks, id) {
  for (block in blocks) {
    if (S7::S7_inherits(block, q2r::pandoc_div)) {
      if (nzchar(block@attr@id) && block@attr@id == id) {
        return(block)
      }
      hit = find_div_by_id(block@content@content, id)
      if (!is.null(hit)) {
        return(hit)
      }
    }
  }
  NULL
}

# Partition selected ids into heading ids and div ids using the AST
#
# An id carried by a pandoc_div in the document is a div id; everything else is
# treated as a heading id (an id that resolves to nothing simply matches no
# blocks during extraction).
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

# Enclosing-heading id chain for each top-level block of a document
#
# Walks the top-level blocks tracking a stack of open headings (level, id). A
# heading belongs to the section it introduces, so a heading block's own chain
# includes its id; every following block until the next same-or-higher heading
# carries it too. Returns one character vector of ids per top-level block.
#
# ast: q2r pandoc AST object

block_id_chains = function(ast) {
  blocks = ast@blocks@content
  chains = vector("list", length(blocks))
  stack = list()

  for (k in seq_along(blocks)) {
    block = blocks[[k]]
    if (S7::S7_inherits(block, q2r::pandoc_header)) {
      level = block@level
      while (length(stack) > 0 && stack[[length(stack)]]$level >= level) {
        stack[[length(stack)]] = NULL
      }
      stack[[length(stack) + 1]] = list(level = level, id = block@attr@id)
    }
    chains[[k]] = vapply(stack, function(s) s$id, character(1))
  }

  chains
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

# Check whether any node's extracted text matches a glob pattern
#
# Shared node-loop used by the content and name rule evaluators. Each node's
# text is obtained via extract_fn and tested against the glob-converted pattern.
#
# nodes: List of AST nodes
# pattern: Glob pattern to match against each node's extracted text
# extract_fn: Function taking a single node and returning its text to match

evaluate_pattern_in_nodes = function(nodes, pattern, extract_fn) {
  for (node in nodes) {
    node_text = extract_fn(node)

    if (nchar(node_text) > 0 && length(grep(utils::glob2rx(pattern), node_text, ignore.case = TRUE)) > 0) {
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

# Evaluate "has content" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_content = function(nodes, rule) {
  pattern = rule@values[1]
  if (is.na(pattern) || nchar(pattern) == 0) {
    return(list(passed = TRUE, message = "empty pattern always matches"))
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

# Validate all rules for a single question against a new AST using section-based matching
#
# repo_ast: q2r pandoc AST object from the document to validate
# question: markermd_question S7 object containing rules and node selections

validate_question_rules = function(repo_ast, question) {

  # Get question AST subset using the shared helper function
  question_ast = get_question_ast(repo_ast, question)

  if (is.null(question_ast)) {
    stop("No sections could be resolved from the selected nodes")
  }

  # Selected nodes (by id) for details display
  node_ids = question@selected_nodes@node_ids
  formatted_hierarchies = if (length(node_ids) > 0) paste0("#", node_ids) else character(0)

  # If no rules, consider it a pass
  if (length(question@rules) == 0) {
    return(list(
      question_name = question@name,
      status = "pass",
      messages = "No rules defined - validation passed",
      details = paste0("Selected node(s): ", paste(formatted_hierarchies, collapse = ", "))
    ))
  }

  # A question with rules but no selected nodes targets nothing; its rules cannot
  # pass. Fail them rather than letting get_question_ast()'s empty-selection
  # fallback evaluate the rules against the whole document.
  if (length(node_ids) == 0) {
    messages = rep("No nodes selected", length(question@rules))
    return(list(
      question_name = question@name,
      status = "fail",
      messages = messages,
      passed = rep(FALSE, length(question@rules)),
      details = paste(c("Selected section(s): (none)", messages), collapse = "\n")
    ))
  }

  # Evaluate each rule - any errors will propagate up
  rule_results = list()
  all_passed = TRUE
  
  for (i in seq_along(question@rules)) {
    rule = question@rules[[i]]
    rule_result = evaluate_rule(question_ast, rule)

    rule_results[[i]] = rule_result
    if (!rule_result$passed) {
      all_passed = FALSE
    }
  }
  
  # Return results - only pass or fail status
  list(
    question_name = question@name,
    status = if (all_passed) "pass" else "fail",
    messages = sapply(rule_results, function(r) r$message),
    passed = sapply(rule_results, function(r) r$passed),
    details = paste(c(
      paste0("Selected section(s): ", paste(formatted_hierarchies, collapse = ", ")),
      sapply(rule_results, function(r) r$message)
    ), collapse = "\n")
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
    stop(
      "This template was created with a previous version of markermd and is no longer ",
      "compatible: its stored document AST predates the move to the q2r parser. ",
      "Please recreate the template with template().",
      call. = FALSE
    )
  }

  version = template@metadata@version
  if (is.na(version) || utils::compareVersion(version, markermd_template_version()) < 0) {
    stop(
      "This template was created with an older version of markermd and is no longer ",
      "compatible. Please recreate the template with template().",
      call. = FALSE
    )
  }
}

# Validates a parsed repository AST against template rules using section-based matching
#
# ast: q2r pandoc AST object from parsing a repository document
# template: markermd_template S7 object with questions and rules

validate_repo_against_rules = function(ast, template) {

  stopifnot(S7::S7_inherits(template, markermd_template))
    
  results = list()

  for (question in template@questions) {
    result = validate_question_rules(ast, question)
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
# tested independently (containment never crosses sections). Returns
# integer(0) when the question has no effective filters or no selection.
#
# current_ast: q2r pandoc AST object from the document to analyze
# question: markermd_question S7 object containing selected nodes and filters

question_filtered_indices = function(current_ast, question) {

  expr = filters_expr(question@filters)
  ids = question@selected_nodes@node_ids
  if (is.null(current_ast) || is.null(expr) || length(ids) == 0) {
    return(integer(0))
  }

  records = q2r_flatten(current_ast)
  container_of = vapply(records, function(r) as.integer(r$container), integer(1))

  # Flatten index of each top-level block, by its position in blocks
  blocks = current_ast@blocks@content
  index_by_block_pos = integer(length(blocks))
  for (r in records) {
    if (!is.na(r$block_pos)) index_by_block_pos[r$block_pos] = r$index
  }

  split = classify_selected_ids(current_ast, ids)

  nodes = list()
  node_indices = integer(0)

  if (length(split$heading_ids) > 0) {
    chains = block_id_chains(current_ast)
    matched = vapply(chains, function(chain) any(split$heading_ids %in% chain), logical(1))
    nodes = c(nodes, blocks[matched])
    node_indices = c(node_indices, index_by_block_pos[matched])
  }

  for (did in split$div_ids) {
    div_index = which(vapply(records, function(r) {
      S7::S7_inherits(r$node, q2r::pandoc_div) && identical(r$node@attr@id, did)
    }, logical(1)))[1]
    if (is.na(div_index)) next
    child_indices = which(container_of == div_index)
    nodes = c(nodes, lapply(child_indices, function(i) records[[i]]$node))
    node_indices = c(node_indices, child_indices)
  }

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
# Heading ids match by section: a top-level block is kept when a selected
# heading id appears in its enclosing-heading id chain (the heading plus every
# block in its section, nested subsections included). Div ids drill in: the
# selected div's own child blocks are contributed, making nested content visible
# to rule evaluation. The two are unioned. An empty selection keeps the whole
# document. The question's filters (if any) then narrow the resulting node set,
# so every consumer - live rule badges, headless validation, and the mark-side
# question content display - sees the filtered set.
#
# current_ast: q2r pandoc AST object from the document to analyze
# question: markermd_question S7 object containing selected nodes and filters

get_question_ast = function(current_ast, question) {

  stopifnot(S7::S7_inherits(question, markermd_question))

  ids = question@selected_nodes@node_ids

  if (length(ids) == 0) {
    return(apply_question_filters(current_ast, question))
  }

  blocks = current_ast@blocks@content
  split = classify_selected_ids(current_ast, ids)

  result_blocks = list()

  if (length(split$heading_ids) > 0) {
    chains = block_id_chains(current_ast)
    matched = vapply(chains, function(chain) any(split$heading_ids %in% chain), logical(1))
    result_blocks = c(result_blocks, blocks[matched])
  }

  for (did in split$div_ids) {
    div = find_div_by_id(blocks, did)
    if (!is.null(div)) {
      result_blocks = c(result_blocks, div@content@content)
    }
  }

  apply_question_filters(q2r::pandoc(blocks = q2r::pandoc_blocks(result_blocks)), question)
}