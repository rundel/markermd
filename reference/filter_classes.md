# S7 Classes for Question Filters

S7 classes representing per-question node filters. A question holds a
list of filter groups; conditions within a group are combined with
logical AND, and groups are combined with logical OR (disjunctive normal
form). Filters narrow the question's node set before its rules evaluate,
using q2r predicate filtering (e.g. `has_class()`, `has_text()`).
