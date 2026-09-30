# Inspect one snapshot

Returns every stored field of a snapshot, its notes, its parts' format
versions and sizes, and its cached per-repository scores.

## Usage

``` r
snapshot_info(id, project = ".")
```

## Arguments

- id:

  The snapshot: its id, a unique prefix of it, a unique label,
  `"latest"`, or a non-positive integer counting back from the latest
  (`0` is the latest, `-1` the one before it).

- project:

  Path to the project directory. Defaults to the working directory.

## Value

A
[markermd_snapshot](https://rundel.github.io/markermd/reference/markermd_snapshot.md)
object.

## See also

[`snapshot_list()`](https://rundel.github.io/markermd/reference/snapshot_list.md),
[`snapshot_scores()`](https://rundel.github.io/markermd/reference/snapshot_scores.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_info("latest", project = "hw01")
snapshot_info("Before review", project = "hw01")
} # }
```
