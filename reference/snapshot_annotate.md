# Annotate a snapshot

Adds a note or keyed metadata to a snapshot's append-only note log, or
changes its label or summary. Nothing is edited in place: a new label or
summary is written to the snapshot and also recorded as a note holding
the new value, so the record of who changed what, and when, is kept.

## Usage

``` r
snapshot_annotate(
  id,
  note = NULL,
  metadata = list(),
  label = NULL,
  summary = NULL,
  project = "."
)
```

## Arguments

- id:

  The snapshot: its id, a unique prefix of it, a unique label,
  `"latest"`, or a non-positive integer counting back from the latest
  (`0` is the latest, `-1` the one before it).

- note:

  Optional free-text note to append.

- metadata:

  Optional named list of key/value metadata to append as keyed notes.

- label:

  Optional new label.

- summary:

  Optional new summary.

- project:

  Path to the project directory. Defaults to the working directory.

## Value

The snapshot's id, invisibly.

## See also

[`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_annotate("latest", note = "Reviewed with the TA on Friday.", project = "hw01")
snapshot_annotate("latest", metadata = list(review_doc = "notes/review.md"), project = "hw01")
} # }
```
