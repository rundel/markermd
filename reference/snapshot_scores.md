# Scores recorded in a snapshot, or the live scores

Returns per-repository, per-question scores as
[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md)
and the History tab compare them: for a snapshot, the scores cached when
it was taken; for `"current"`, the live database scored with the same
code as
[`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md)
and the Assignments table.

## Usage

``` r
snapshot_scores(id = "current", project = ".")
```

## Arguments

- id:

  A snapshot reference (see
  [`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md)),
  or `"current"` for the live state.

- project:

  Path to the project directory. Defaults to the working directory.

## Value

A data frame with `assignment_repo`, `question_name` and `score`,
including a `"(total)"` row per repository (`NA` until every question is
graded).

## See also

[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md),
[`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_scores("latest", project = "hw01")
snapshot_scores(project = "hw01")
} # }
```
