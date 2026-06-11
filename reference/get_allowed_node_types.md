# Get allowed node types for rules

Returns the list of valid node types that can be used in validation
rules. These are the same friendly category names shown in the AST tree
(see `q2r_node_kind()`), plus a catch-all "Any node" option.

## Usage

``` r
get_allowed_node_types()
```

## Value

Character vector of allowed node types
