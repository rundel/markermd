# Markermd Validation Rule

S7 class representing a single validation rule for template questions.
This class provides built-in validation for all rule properties and
ensures consistency across the application.

## Usage

``` r
markermd_rule(node_type = character(0), verb = character(0), values = NULL)
```

## Arguments

- node_type:

  Character. The type of AST node this rule applies to. Must be one of
  the values returned by get_allowed_node_types().

- verb:

  Character. The validation verb/action to perform. Must be one of the
  values returned by get_allowed_rule_verbs().

- values:

  Vector. The values/parameters for the validation. Type and format
  depend on the verb:

  - "has between": numeric vector of length 2 (min, max)

  - "has at least": integer (minimum count)

  - "has at most": integer (maximum count)

  - "has content": character string (pattern)

  - "lacks content": character string (pattern)

  - "has name": character string (pattern)

## Examples

``` r
# Create a count rule
count_rule = markermd_rule(
  node_type = "Heading",
  verb = "has between",
  values = c(1, 5)
)

# Create a content rule
content_rule = markermd_rule(
  node_type = "Chunk",
  verb = "has content",
  values = "*plot*"
)

# Validate a rule
validation = validate_markermd_rule(count_rule)
if (!validation$valid) {
  print(validation$errors)
}
```
