# Markermd Filter Condition

S7 class representing a single filter condition. Each condition maps to
one q2r predicate: a node-type test (`is(<pandoc class>)`) or one of the
attribute/content helpers (`has_class()`, `has_id()`, `has_text()`,
`has_label()`, `has_option()`, `has_engine()`).

## Usage

``` r
markermd_filter_condition(
  type = character(0),
  value = character(0),
  negate = FALSE
)
```

## Arguments

- type:

  Character. The condition type. Must be one of the values returned by
  filter_condition_types().

- value:

  Character. The condition value. For "node type" this must be a node
  kind from rule_node_types() (excluding "Any node"); for "has option"
  it is a Quarto cell option as written on a `#|` line ("eval" for key
  presence, "eval: false" for a key/value test); for "has engine" it is
  a cell engine name (e.g. "r"); for the other types it is the class
  name, id, regex pattern, or glob pattern.

- negate:

  Logical. When TRUE the predicate is negated (logical NOT).

## Value

A `markermd_filter_condition` S7 object.

## Details

"has text" matches a node's rendered text
([`q2r::ast_text()`](https://rundel.github.io/q2r/reference/ast_text.html),
without markup) against a case-sensitive regular expression; "has label"
matches a heading or div id, or a chunk's label option, against a glob
pattern (`fig-*`). Rules
([`markermd_rule()`](https://rundel.github.io/markermd/reference/markermd_rule.md))
differ: their content verbs match the node's qmd source
case-insensitively and "has name" matches a heading title or chunk label
by regular expression.

## Examples

``` r
# Match divs
type_condition = markermd_filter_condition(
  type = "node type",
  value = "Div"
)

# Match nodes carrying a class
class_condition = markermd_filter_condition(
  type = "has class",
  value = "hint"
)
```
