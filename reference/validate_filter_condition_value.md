# Validate filter condition value based on condition type

Validates a filter condition value according to the requirements of its
condition type. "node type" values are one or more node kinds from
get_allowed_node_types() (excluding "Any node"), combined as a logical
OR; the other types take a single character string (empty allowed).

## Usage

``` r
validate_filter_condition_value(type, value)
```

## Arguments

- type:

  Character. The condition type that determines validation requirements

- value:

  Character. The value to validate

## Value

Character error message if invalid, NULL if valid
