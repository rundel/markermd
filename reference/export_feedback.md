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
export_feedback(project = ".", group_headings = FALSE)
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

- group_headings:

  Passed to
  [`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md):
  whether to write each item group's selected items under a subheading
  carrying the group's label. Defaults to `FALSE`.

## Value

Invisibly, a list with elements `scores` (the CSV path) and `comments`
(the feedback file paths).

## See also

[`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md),
[`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
export_feedback("hw01")
} # }
```
