# Export a project's grading template to YAML

Writes the template stored in the project's grading database to a YAML
file (the optional import/export format). This is the inverse of
[`template_import()`](https://rundel.github.io/markermd/reference/template_import.md).

## Usage

``` r
template_export(path, project = ".", snapshot = NULL)
```

## Arguments

- path:

  Output path for the template `.yaml` file.

- project:

  Path to the project directory. Defaults to the working directory.

- snapshot:

  Optional snapshot reference (see
  [`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md)):
  export the template that snapshot recorded instead of the stored one.

## Value

The output `path`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
template_export("hw01-template.yaml", project = "hw01")
} # }
```
