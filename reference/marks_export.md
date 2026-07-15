# Export a project's grading marks to YAML

Writes the per-repository grading state stored in the project's grading
database (each repository/question pair's selected rubric items,
identified by description, plus public and private comments) to a YAML
file. Only pairs with grading activity are written; a pair whose items
were all deselected exports as `items: []`. The file can be edited and
brought back with
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
and its format is described by the JSON Schema at
`system.file("schema/markermd-marks.json", package = "markermd")`.

## Usage

``` r
marks_export(path, project = ".", repo = NULL, question = NULL)
```

## Arguments

- path:

  Output path for the marks `.yaml` file.

- project:

  Path to the project directory. Defaults to the working directory.

- repo:

  Optional character vector of repository names to export.

- question:

  Optional character vector of question names to export.

## Value

The output `path`, invisibly.

## See also

[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
marks_export("hw01-marks.yaml", project = "hw01")
} # }
```
