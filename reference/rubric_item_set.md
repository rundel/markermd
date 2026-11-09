# Reword or re-point one rubric item

Changes the description and/or points of a single stored rubric item in
place. The item keeps its id, position, hotkey and group, so every
recorded selection of it (and so every repository's marks) carries over
to the new wording: unlike replacing the question's rubric with
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
nothing has to be re-recorded.

## Usage

``` r
rubric_item_set(
  question,
  item,
  description = NULL,
  points = NULL,
  project = ".",
  snapshot = TRUE,
  summary = NULL
)
```

## Arguments

- question:

  Question name from the project's template.

- item:

  Current description of the item, matched verbatim.

- description:

  New description, or `NULL` to keep the current one.

- points:

  New point value, or `NULL` to keep the current one.

- project:

  Path to the project directory. Defaults to the working directory.

- snapshot:

  Whether to take an automatic snapshot before the change.

- summary:

  Optional text recorded on the automatic snapshot and in the activity
  log: what the change is and why.

## Value

Invisibly, the updated
[markermd_rubric_item](https://rundel.github.io/markermd/reference/markermd_rubric_item.md).

## Details

The item is identified by its current description, matched verbatim as
in
[`marks_set()`](https://rundel.github.io/markermd/reference/marks_set.md)
and
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md).
A new description must be non-empty and must not equal another item's
description in the same question, since descriptions identify items on
import. Changing the points changes the score of every repository that
has the item selected; use
[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md)
afterwards to review the effect.

An automatic snapshot of the state being changed is taken in the same
transaction (see
[`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md)),
and the change is recorded in the activity log (see
[`project_activity()`](https://rundel.github.io/markermd/reference/project_activity.md)).
A running
[`mark()`](https://rundel.github.io/markermd/reference/mark.md) session
picks the change up and offers to reload its rubric.

## See also

[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md),
[`marks_set()`](https://rundel.github.io/markermd/reference/marks_set.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rubric_item_set("Document", "Repository check failed",
  description = "The repository check did not complete successfully on the final submission",
  project = "hw03",
  summary = "Feedback review: clearer wording"
)
} # }
```
