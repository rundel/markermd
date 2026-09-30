# Compare two snapshots, or a snapshot with the live state

Reports what changed between two snapshots (or between a snapshot and
the project's current grading state): rubric items added, removed,
reworded, re-pointed or regrouped, group bound and scoring changes;
per-repository selection, public comment and private note changes;
per-pair score deltas and every repository's total before and after; and
template questions added, removed, renamed or changed.

## Usage

``` r
snapshot_diff(
  from,
  to = "current",
  project = ".",
  part = c("rubric", "marks", "template")
)
```

## Arguments

- from:

  A snapshot reference (see
  [`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md)).

- to:

  A snapshot reference, or `"current"` (the default) for the live state.

- project:

  Path to the project directory. Defaults to the working directory.

- part:

  Which parts to compare: any of `"rubric"`, `"marks"`, `"template"`.
  Scores are always compared.

## Value

A
[markermd_snapshot_diff](https://rundel.github.io/markermd/reference/markermd_snapshot_diff.md)
object. Its `rubric`, `marks`, `scores`, `totals` and `template`
properties are data frames.

## Details

Rubric items are matched across the two sides by the database ids a
snapshot records, so an item reworded in
[`mark()`](https://rundel.github.io/markermd/reference/mark.md) is
reported as a rename. A replace import mints new ids; items it
re-created are matched as a likely rename when they keep their points
and are selected by the same repositories on both sides. Renamed items
are not reported as selection changes in the marks.

The comparison is only meaningful when the snapshot's part is present on
both sides; a `part` left out is reported as unchanged.

## See also

[`snapshot_scores()`](https://rundel.github.io/markermd/reference/snapshot_scores.md),
[`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# What did everything since the latest snapshot change?
snapshot_diff("latest", project = "hw01")

# What did the last import change?
snapshot_diff(-1, "latest", project = "hw01")
} # }
```
