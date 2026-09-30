# Import a grading template from YAML into a project's database

Reads a template YAML file and stores it in the project's grading
database, which is the canonical store for templates. This is the
inverse of
[`template_export()`](https://rundel.github.io/markermd/reference/template_export.md).
Equivalent to `project_set(project, template = path)`. An automatic
snapshot of the state being replaced is taken first (see
[`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md)).

## Usage

``` r
template_import(path, project = ".", snapshot = TRUE, summary = NULL)
```

## Arguments

- path:

  Path to a template `.yaml`/`.yml` file. Absolute or relative to the
  current directory; a bare relative path is also resolved against the
  project root.

- project:

  Path to the project directory. Defaults to the working directory.

- snapshot:

  Whether to take an automatic snapshot before the import. `FALSE` is
  for scripts that take their own snapshot once before several imports.

- summary:

  Optional text recorded on the automatic snapshot: what the import
  changes and why.

## Value

The imported `markermd_template` object, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
template_import("hw01-template.yaml", project = "hw01")
} # }
```
