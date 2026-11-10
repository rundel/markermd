# Delete one rubric item

Removes a single stored rubric item, leaving the rest of its question's
rubric and every other recorded selection untouched: unlike a
`mode = "replace"`
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
only this item's selections stop counting.

## Usage

``` r
rubric_item_delete(
  question,
  item,
  project = ".",
  force = FALSE,
  snapshot = TRUE,
  summary = NULL
)
```

## Arguments

- question:

  Question name from the project's template.

- item:

  Description of the item, matched verbatim.

- project:

  Path to the project directory. Defaults to the working directory.

- force:

  Whether to delete an item that repositories have selected without
  asking for confirmation.

- snapshot:

  Whether to take an automatic snapshot before the change.

- summary:

  Optional text recorded on the automatic snapshot and in the activity
  log: what the change is and why.

## Value

Invisibly, the deleted
[markermd_rubric_item](https://rundel.github.io/markermd/reference/markermd_rubric_item.md),
or `NULL` when the deletion was declined.

## Details

The item is identified by its description, matched verbatim as in
[`rubric_item_set()`](https://rundel.github.io/markermd/reference/rubric_item_set.md).
When some repository currently has the item selected, deleting it
changes that repository's score and can leave the question ungraded for
it, so the deletion has to be confirmed: an interactive session is
asked, and a non-interactive one is refused unless `force = TRUE`. An
item nobody has selected is deleted without asking.

The item is soft-deleted: its past selection events stay in the History
tab, and restoring the automatic snapshot taken before the change (see
[`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md))
brings the item back with its selections. The remaining items keep their
order and their hotkeys are renumbered. The change is recorded in the
activity log (see
[`project_activity()`](https://rundel.github.io/markermd/reference/project_activity.md)).

## See also

[`rubric_item_set()`](https://rundel.github.io/markermd/reference/rubric_item_set.md),
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
[`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rubric_item_delete("Q3", "Distance ordering is not explained",
  project = "hw03",
  summary = "Wording review: never selected, meaning unclear"
)
} # }
```
