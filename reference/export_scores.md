# Export per-repository scores to a CSV file

Computes every student repository's per-question scores from the
project's grading database and writes them to `scores.csv` in the
project root, one row per repository with one column per question plus a
`total` column. Scores are recomputed the same way
[`mark()`](https://rundel.github.io/markermd/reference/mark.md) displays
them: the points of the selected rubric items are summed and passed
through the question's grading mode and score bounds. A
repository/question pair that is not yet graded (no selected rubric item
and no public comment) is written as `NA`, and a repository's `total`
stays `NA` until all of its questions are graded.

## Usage

``` r
export_scores(project = ".")
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

## Value

The path of the written CSV file, invisibly.

## See also

[`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md),
[`export_marks()`](https://rundel.github.io/markermd/reference/export_marks.md),
[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)
