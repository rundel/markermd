# Prune old automatic snapshots

Deletes unpinned automatic snapshots that are both older than
`keep_days` and outside the `keep_last` most recent ones. Manual
snapshots, pinned snapshots, the latest snapshot and the oldest
(baseline) snapshot are never pruned. Stored content no remaining
snapshot shares is removed.

## Usage

``` r
snapshot_prune(project = ".", keep_last = 20, keep_days = 30, kind = "auto")
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

- keep_last:

  Keep at least this many of the most recent automatic snapshots.

- keep_days:

  Keep automatic snapshots younger than this many days.

- kind:

  Which snapshots are candidates; only `"auto"` is supported.

## Value

The deleted snapshot ids, invisibly.

## See also

[`snapshot_delete()`](https://rundel.github.io/markermd/reference/snapshot_delete.md),
[`snapshot_pin()`](https://rundel.github.io/markermd/reference/snapshot_pin.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_prune("hw01", keep_last = 10, keep_days = 14)
} # }
```
