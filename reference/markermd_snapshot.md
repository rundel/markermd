# Snapshot of a project's grading state

S7 class returned by
[`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md):
every stored field of one snapshot, its notes in order, its parts'
format versions and sizes, and its cached per-repository scores.

## Usage

``` r
markermd_snapshot(
  snapshot_id = character(0),
  created_at = .POSIXct(numeric(0)),
  created_tz = character(0),
  username = character(0),
  kind = character(0),
  trigger = character(0),
  label = character(0),
  summary = character(0),
  parent_snapshot_id = character(0),
  schema_version = character(0),
  markermd_version = character(0),
  content_hash = character(0),
  pinned = logical(0),
  notes = data.frame(),
  parts = data.frame(),
  stats = data.frame()
)
```

## Arguments

- snapshot_id:

  Character. The snapshot's id (`snap-<UTC time>-<suffix>`).

- created_at:

  POSIXct. Creation time, in the session's time zone.

- created_tz:

  Character. The creator's time zone name.

- username:

  Character. Who took it.

- kind:

  Character. `"manual"` or `"auto"`.

- trigger:

  Character. What took an automatic snapshot (`NA` for a manual one).

- label:

  Character. Short name, or `NA`.

- summary:

  Character. What changed or why, or `NA`.

- parent_snapshot_id:

  Character. The previous snapshot's id, or `NA` for the first.

- schema_version:

  Character. Database schema version at creation.

- markermd_version:

  Character. Package version at creation.

- content_hash:

  Character. Hash of the stored parts.

- pinned:

  Logical. Whether the snapshot is protected from pruning.

- notes:

  Data frame of notes and keyed metadata (`timestamp`, `username`,
  `key`, `value`), oldest first.

- parts:

  Data frame with one row per stored part (`part`, `format_version`,
  `size` in bytes, `hash`).

- stats:

  Data frame of cached scores (`assignment_repo`, `question_name`,
  `score`), with `"(total)"` rows for each repository's total.

## Value

A `markermd_snapshot` S7 object.

## See also

[`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md),
[`snapshot_list()`](https://rundel.github.io/markermd/reference/snapshot_list.md)
