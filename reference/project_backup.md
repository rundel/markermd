# Back up the project database file

Writes a consistent copy of the grading database (with SQLite's
`VACUUM INTO`, safe while
[`mark()`](https://rundel.github.io/markermd/reference/mark.md) is open)
and of the project config to a backup directory, and records the
backup's path as a note on the latest snapshot. Snapshots live inside
the database, so this is the protection against losing or corrupting the
file itself.

## Usage

``` r
project_backup(project = ".", dir = "backups")
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

- dir:

  Backup directory, relative to the project root unless absolute.
  Created when missing.

## Value

The path of the database copy, invisibly.

## See also

[`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md),
[`snapshot_export()`](https://rundel.github.io/markermd/reference/snapshot_export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project_backup("hw01")
} # }
```
