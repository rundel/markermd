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
  get_allowed_filter_condition_types().

- value:

  Character. The condition value. For "node type" this must be a node
  kind from get_allowed_node_types() (excluding "Any node"); for "has
  option" it is a Quarto cell option as written on a `#|` line ("eval"
  for key presence, "eval: false" for a key/value test); for "has
  engine" it is a cell engine name (e.g. "r"); for the other types it is
  the class name, id, regex pattern, or glob pattern.

- negate:

  Logical. When TRUE the predicate is negated (logical NOT).

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
