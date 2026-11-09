# Markermd Template

S7 class representing a complete markermd template with questions and
metadata

## Usage

``` r
markermd_template(
  original_ast = NULL,
  questions = list(),
  files = character(0),
  metadata = markermd_metadata()
)
```

## Arguments

- original_ast:

  pandoc. The original parsed AST from q2r

- questions:

  List of question objects

- files:

  Character vector. The assignment's files besides its Quarto document,
  as paths relative to the repository root, that questions can target
  (see the `file` property of
  [`markermd_question()`](https://rundel.github.io/markermd/reference/markermd_question.md)).

- metadata:

  template_metadata. Template metadata

## Value

A `markermd_template` S7 object.
