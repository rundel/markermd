# Pin or unpin a snapshot

A pinned snapshot is never removed by
[`snapshot_prune()`](https://rundel.github.io/markermd/reference/snapshot_prune.md)
and needs `force = TRUE` in
[`snapshot_delete()`](https://rundel.github.io/markermd/reference/snapshot_delete.md).

## Usage

``` r
snapshot_pin(id, pinned = TRUE, project = ".")
```

## Arguments

- id:

  The snapshot: its id, a unique prefix of it, a unique label,
  `"latest"`, or a non-positive integer counting back from the latest
  (`0` is the latest, `-1` the one before it).

- pinned:

  `TRUE` to pin, `FALSE` to unpin.

- project:

  Path to the project directory. Defaults to the working directory.

## Value

The snapshot's id, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_pin("Scores sent to students", project = "hw01")
} # }
```
