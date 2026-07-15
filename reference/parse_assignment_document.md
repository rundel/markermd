# Parse an assignment document into a Pandoc AST

Reads a qmd document, normalises any knitr-style chunk headers so the
Quarto parser accepts them, and parses it into a q2r `pandoc` AST.

## Usage

``` r
parse_assignment_document(file_path)
```

## Arguments

- file_path:

  Character. Path to the assignment file

## Value

A q2r `pandoc` AST object for the parsed document.

## Details

Parse diagnostics are surfaced rather than stripped: error-kind
diagnostics from q2r are raised as R errors and warnings as R warnings.
If parsing yields an empty AST (no blocks) an error is reported.
