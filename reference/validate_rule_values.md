# Validate rule values based on verb type

Validates rule values according to the specific requirements of each
verb type. Different verbs have different value requirements (numeric
ranges, text patterns, etc.).

## Usage

``` r
validate_rule_values(verb, values)
```

## Arguments

- verb:

  Character. The rule verb that determines validation requirements

- values:

  List or vector. The values to validate

## Value

Character error message if invalid, NULL if valid
