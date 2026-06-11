# Export a project's grading rubric to YAML

Writes the rubric stored in the project's grading database (each
question's rubric items, in display order, plus its scoring setup when
one has been configured) to a YAML file. The file contains no
per-repository grading data, so it can be shared, edited by hand or by
an LLM tool, and brought back with
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md).
The file format is described by the JSON Schema at
`system.file("schema/markermd-rubric.json", package = "markermd")`.

## Usage

``` r
rubric_export(path, project = ".", question = NULL)
```

## Arguments

- path:

  Output path for the rubric `.yaml` file.

- project:

  Path to the project directory. Defaults to the working directory.

- question:

  Optional character vector of question names to export. Defaults to
  every question in the project's template.

## Value

The output `path`, invisibly.

## See also

[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
[`template_export()`](https://rundel.github.io/markermd/reference/template_export.md)
