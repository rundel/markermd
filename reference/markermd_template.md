# Markermd Template

S7 class representing a complete markermd template with questions and
metadata

## Usage

``` r
markermd_template(
  original_ast = NULL,
  questions = list(),
  metadata = markermd_metadata()
)
```

## Arguments

- original_ast:

  pandoc. The original parsed AST from q2r

- questions:

  List of question objects

- metadata:

  template_metadata. Template metadata

## Value

A `markermd_template` S7 object.
