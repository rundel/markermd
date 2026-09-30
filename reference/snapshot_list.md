# List a project's snapshots

Returns the project's snapshots, newest first, with their metadata and a
summary of the cached totals (how many repositories had a complete
total, and its mean and range).

## Usage

``` r
snapshot_list(project = ".", kind = NULL, since = NULL, n = NULL)
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

- kind:

  Optional filter: `"manual"` or `"auto"`.

- since:

  Optional lower bound on creation time (a `POSIXct`, `Date`, or a
  string [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html)
  understands).

- n:

  Optional maximum number of snapshots to return.

## Value

A data frame (class `markermd_snapshot_list`) with one row per snapshot:
`snapshot_id`, `created_at` (in the session's time zone), `created_tz`,
`username`, `kind`, `trigger`, `label`, `summary`, `n_notes`, `pinned`,
`n_complete`, `mean_total`, `min_total` and `max_total`. Its print
method shows one line per snapshot.

## See also

[`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md),
[`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_list("hw01")
snapshot_list("hw01", kind = "manual")
} # }
```
