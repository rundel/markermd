# Take a snapshot of a project's grading state

Records the project's current grading state (its template, rubric and
marks) as a named, timestamped snapshot inside the project database. The
snapshot can be listed
([`snapshot_list()`](https://rundel.github.io/markermd/reference/snapshot_list.md)),
inspected
([`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md)),
compared with another or with the live state
([`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md)),
exported to YAML
([`snapshot_export()`](https://rundel.github.io/markermd/reference/snapshot_export.md))
and restored
([`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md)).

## Usage

``` r
snapshot_create(project = ".", label = NULL, summary = NULL, metadata = list())
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

- label:

  Optional short name, e.g. `"Before feedback review"`. A unique label
  can be used in place of the snapshot id in the other `snapshot_*()`
  functions.

- summary:

  Optional free text: what is about to change, or why the snapshot was
  taken.

- metadata:

  Optional named list of extra key/value metadata, recorded as keyed
  notes (e.g. `list(review_doc = "notes/review.md")`).

## Value

The new snapshot's id, invisibly.

## Details

Snapshots are also taken automatically before every bulk change to the
database:
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
[`template_import()`](https://rundel.github.io/markermd/reference/template_import.md),
[`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md),
the imports and deletions in
[`mark()`](https://rundel.github.io/markermd/reference/mark.md) and
saves in
[`template()`](https://rundel.github.io/markermd/reference/template.md),
and schema upgrades. Those functions take a `summary` argument so the
reason for a change is recorded with it. An automatic snapshot of a
state identical to the latest snapshot's is not created; the operation
is recorded as a note on that snapshot instead. A manual snapshot is
always created, since its label and summary carry information, and
identical parts are stored once.

## See also

[`snapshot_list()`](https://rundel.github.io/markermd/reference/snapshot_list.md),
[`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md),
[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md),
[`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md),
[`snapshot_annotate()`](https://rundel.github.io/markermd/reference/snapshot_annotate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_create("hw01", label = "Before review",
  summary = "First-pass machine grading reviewed item by item next.")
} # }
```
