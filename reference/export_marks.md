# Export scores and feedback for a graded project

Runs
[`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md)
and
[`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)
in one call: the final hand-off step of a grading project, writing
`scores.csv` and the per-repository feedback files from the project's
grading database.

## Usage

``` r
export_marks(project = ".")
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

## Value

Invisibly, a list with elements `scores` (the CSV path) and `comments`
(the feedback file paths).

## See also

[`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md),
[`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)
