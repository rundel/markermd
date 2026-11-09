# Parse an assignment document into a Pandoc AST

Reads a qmd document, normalizes any knitr-style chunk headers so the
Quarto parser accepts them, and parses it into a q2r `pandoc` AST.

## Usage

``` r
parse_assignment_document(path)
```

## Arguments

- path:

  Character. Path to the assignment file

## Value

A q2r `pandoc` AST object for the parsed document.

## Details

Parse diagnostics are surfaced rather than stripped: error-kind
diagnostics from q2r are raised as R errors. Warning-kind diagnostics
are not signaled (they would otherwise print to the console from inside
the apps); they stay attached to the returned AST's `@diagnostics` slot,
labeled with the document's file name, and
[`mark()`](https://rundel.github.io/markermd/reference/mark.md),
[`template()`](https://rundel.github.io/markermd/reference/template.md)
and
[`parse_project()`](https://rundel.github.io/markermd/reference/parse_project.md)
report them. Rewriting a knitr-style chunk header adds lines, so a
diagnostic whose line number moved carries a hint giving the line of the
file as written. If parsing yields an empty AST (no blocks) an error is
reported.
