# Validate node type value

Checks if a rule's node types are valid. A rule targets one or more node
types combined as a logical OR, so this accepts a non-empty character
vector of unique allowed types.

## Usage

``` r
validate_node_type(node_type)
```

## Arguments

- node_type:

  Character vector. The node type(s) to validate

## Value

Character error message if invalid, NULL if valid
