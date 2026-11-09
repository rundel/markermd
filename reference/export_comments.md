# Export student-facing feedback to per-repository markdown files

Writes each student repository's public feedback to
`<comments>/<repo>.md` under the project root, using the project's
configured comments directory (`comments/` when none is configured; it
is created when missing). Each graded question appears as a heading, in
template order, followed by a bulleted markdown list of its selected
rubric item descriptions and its public comment. Document-scope
questions (feedback about the document as a whole) come first, ahead of
the per-question sections. Private comments are never exported, and a
repository with no public feedback gets no file.

## Usage

``` r
export_comments(project = ".", group_headings = FALSE)
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

- group_headings:

  Whether to write each item group's selected items under a subheading
  carrying the group's label. Defaults to `FALSE`.

## Value

The paths of the written markdown files, invisibly.

## Details

Item groups never change the default output: selected items are listed
as they are and a group's floor or ceiling is never mentioned, in
keeping with the rule that student-facing text does not describe grading
mechanics. With `group_headings = TRUE` the selected items that belong
to a group are written under a `### <label>` subheading beneath the
question's heading, in display order; the question's ungrouped items and
its public comment come first, without a subheading, and a group with no
selected items gets none. Group labels then reach students, so choose
them accordingly.

## See also

[`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md),
[`export_feedback()`](https://rundel.github.io/markermd/reference/export_feedback.md),
[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
export_comments("hw01")

# Group the feedback under the rubric's item group labels
export_comments("hw01", group_headings = TRUE)
} # }
```
