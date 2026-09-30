# Delete a snapshot

Permanently removes a snapshot, its notes and its cached scores. A
manual or pinned snapshot is only deleted with `force = TRUE`. Stored
content no other snapshot shares is removed with it.

## Usage

``` r
snapshot_delete(id, project = ".", force = FALSE)
```

## Arguments

- id:

  The snapshot: its id, a unique prefix of it, a unique label,
  `"latest"`, or a non-positive integer counting back from the latest
  (`0` is the latest, `-1` the one before it).

- project:

  Path to the project directory. Defaults to the working directory.

- force:

  Delete a manual or pinned snapshot.

## Value

The deleted snapshot's id, invisibly.

## See also

[`snapshot_prune()`](https://rundel.github.io/markermd/reference/snapshot_prune.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_delete("snap-20260929T235540Z-3f9a", project = "hw01")
} # }
```
