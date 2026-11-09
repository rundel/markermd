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

  Character vector. One or more AST node types this rule applies to,
  combined as a logical OR. Each must be one of the values returned by
  rule_node_types().

- verb:

  Character. The validation verb/action to perform. Must be one of the
  values returned by rule_verbs().

- values:

  Vector. The values/parameters for the validation. Type and format
  depend on the verb:

  - "has between": numeric vector of length 2 (min, max)

  - "has at least": integer (minimum count)

  - "has at most": integer (maximum count)

  - "has content": character string (pattern)

  - "lacks content": character string (pattern)

  - "has name": character string (pattern)

## Value

A `markermd_rule` S7 object.

## Details

Rule patterns are regular expressions (R's default engine), matched
case-insensitively and unanchored, and an invalid pattern fails the
rule. "has content" and "lacks content" match a node's qmd source as
written back by q2r, so a chunk's fences and `#|` option lines and a
heading's `#` marks and `{#id}` are part of the text. "has name" matches
a heading's title or a chunk's label, never an attribute id.

Filters
([`markermd_filter_condition()`](https://rundel.github.io/markermd/reference/markermd_filter_condition.md))
differ: "has text" matches a node's rendered text
([`q2r::ast_text()`](https://rundel.github.io/q2r/reference/ast_text.html),
no markup) case-sensitively, and "has label" matches a node's id or a
chunk's label option with a glob pattern (`fig-*`). The same words can
therefore match in a rule and not in a filter, or the reverse.

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
  values = "ggplot\\("
)
```
