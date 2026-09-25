# A template with the given question names and a trivial AST, for tests that
# need a markermd_template but not a real document (the group tests' initial
# database state and Assignments table checks)
mt_template_for_groups = function(names) {
  qs = lapply(seq_along(names), function(i) markermd_question(id = as.integer(i), name = names[i]))
  markermd_template(original_ast = q2r::parse_qmd("# x", quiet = TRUE), questions = qs)
}
