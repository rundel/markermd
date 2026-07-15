# Export student-facing feedback to per-repository markdown files

Writes each student repository's public feedback to
`<comments>/<repo>.md` under the project root, using the project's
configured comments directory (`comments/` when none is configured; it
is created when missing). Each graded question appears as a heading, in
template order, followed by a bulleted markdown list of its selected
rubric item descriptions and its public comment. Private comments are
never exported, and a repository with no public feedback gets no file.

## Usage

``` r
export_comments(project = ".")
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

## Value

The paths of the written markdown files, invisibly.

## See also

[`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md),
[`export_marks()`](https://rundel.github.io/markermd/reference/export_marks.md),
[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
export_comments("hw01")
} # }
```
