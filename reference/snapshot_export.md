# Export a snapshot to YAML files

Writes a snapshot's parts as the project's exchange files, validated by
the same readers the imports use: `markermd-template.yaml` (when the
snapshot holds a template), `markermd-rubric.yaml` and
`markermd-marks.yaml`, plus a `snapshot.yaml` sidecar with the
snapshot's fields and notes, so the directory stands alone as an
archive. The rubric file drops the item and group ids a snapshot carries
internally, so it validates against the published schema.

## Usage

``` r
snapshot_export(id, dir, project = ".")
```

## Arguments

- id:

  The snapshot: its id, a unique prefix of it, a unique label,
  `"latest"`, or a non-positive integer counting back from the latest
  (`0` is the latest, `-1` the one before it).

- dir:

  Output directory, created when missing.

- project:

  Path to the project directory. Defaults to the working directory.

## Value

The written file paths, invisibly.

## See also

[`template_export()`](https://rundel.github.io/markermd/reference/template_export.md),
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md),
[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_export("Before review", "backups/before-review", project = "hw01")
} # }
```
