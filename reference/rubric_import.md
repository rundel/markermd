# Import a grading rubric from YAML into a project's database

Reads a rubric YAML file (see
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md)
for the format) and applies it to the project's grading database. Item
order in the file determines display order and keyboard hotkeys (the
first ten items of a question get the keys 1-9 and 0; any further items
are toggled by clicking, with no limit on their number). This is the
inverse of
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md).

## Usage

``` r
rubric_import(
  path,
  project = ".",
  mode = c("append", "replace"),
  question = NULL,
  snapshot = TRUE,
  summary = NULL
)
```

## Arguments

- path:

  Path to a rubric `.yaml`/`.yml` file. Absolute or relative to the
  current directory; a bare relative path is also resolved against the
  project root.

- project:

  Path to the project directory. Defaults to the working directory.

- mode:

  Either `"append"` or `"replace"`; see Details.

- question:

  Optional character vector restricting the import to those question
  names within the file.

- snapshot:

  Whether to take an automatic snapshot before the import. `FALSE` is
  for scripts that take their own snapshot once before several imports.

- summary:

  Optional text recorded on the automatic snapshot: what the import
  changes and why.

## Value

The parsed rubric list, invisibly.

## Details

With `mode = "append"` (the default) the file's items are added after
each question's existing items; since descriptions identify items in
every exchange format, an append is refused when the file repeats a
description the question already stores (reword stored items with
[`rubric_item_set()`](https://rundel.github.io/markermd/reference/rubric_item_set.md),
or import only the new ones). With `mode = "replace"` each affected
question's existing items and item groups are deleted first, so their
recorded selections stop counting for every repository (the selection
events stay in the activity log, and restoring the automatic snapshot,
see below, brings the items back with their selections). A question's
scoring setup is only updated when the file provides a `scoring` block.

Item groups (format `"1.1"`; see
[markermd_rubric_group](https://rundel.github.io/markermd/reference/markermd_rubric_group.md))
are matched to the question's existing groups by label. Items for an
existing group are appended to the end of that group and the file's
`floor`, `ceiling` and `exclusive` overwrite the stored ones; an unknown
label creates a new group at the end of the question. Making an existing
group exclusive while some repository has more than one of its items
selected aborts before anything is written, listing those repositories.

Question names in the file must exactly match question names in the
project's stored template; mismatches abort before anything is written.

An automatic snapshot of the state being changed is taken in the same
transaction as the import (see
[`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md)),
so a replace import can be undone with
[`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md).

## See also

[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md),
[`read_rubric_yaml()`](https://rundel.github.io/markermd/reference/read_rubric_yaml.md),
[`validate_rubric_file()`](https://rundel.github.io/markermd/reference/validate_rubric_file.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rubric_import("hw01-rubric.yaml", project = "hw01")

# Replace each affected question's existing items, recording why
rubric_import("hw01-rubric.yaml", project = "hw01", mode = "replace",
  summary = "Feedback review: reworded the Q1 items and capped the examples group")
} # }
```
