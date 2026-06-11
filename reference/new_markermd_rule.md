# Create a new markermd rule with default values

Convenience function to create a new rule with appropriate default
values based on the specified verb type.

## Usage

``` r
new_markermd_rule(node_type = "Any node", verb = "has at least", values = NULL)
```

## Arguments

- node_type:

  Character. The node type (defaults to "Any node")

- verb:

  Character. The validation verb (defaults to "has at least")

- values:

  Vector. Custom values (if NULL, uses defaults for the verb)

## Value

markermd_rule object
