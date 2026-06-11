# Create a new markermd filter condition with default values

Convenience function to create a new filter condition with an
appropriate default value for the specified condition type.

## Usage

``` r
new_markermd_filter_condition(type = "node type", value = NULL, negate = FALSE)
```

## Arguments

- type:

  Character. The condition type (defaults to "node type")

- value:

  Character. Custom value (if NULL, uses the default for the type)

- negate:

  Logical. Whether the condition is negated (defaults to FALSE)

## Value

markermd_filter_condition object
