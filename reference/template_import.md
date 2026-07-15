# Import a grading template from YAML into a project's database

Reads a template YAML file and stores it in the project's grading
database, which is the canonical store for templates. This is the
inverse of
[`template_export()`](https://rundel.github.io/markermd/reference/template_export.md).
Equivalent to `project_set(project, template = path)`.

## Usage

``` r
template_import(path, project = ".")
```

## Arguments

- path:

  Path to a template `.yaml`/`.yml` file. Absolute or relative to the
  current directory; a bare relative path is also resolved against the
  project root.

- project:

  Path to the project directory. Defaults to the working directory.

## Value

The imported `markermd_template` object, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
template_import("hw01-template.yaml", project = "hw01")
} # }
```
