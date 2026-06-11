# Validate a marks file against the markermd JSON Schema

Structurally validates a marks YAML (or JSON) file against the bundled
JSON Schema (`inst/schema/markermd-marks.json`). This is an optional
check aimed at tooling and LLM-generated files; the authoritative
validation (including matching item descriptions against the project's
rubric) happens in
[`read_marks_yaml()`](https://rundel.github.io/markermd/reference/read_marks_yaml.md)
and
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md).
Requires the suggested `jsonvalidate` package.

## Usage

``` r
validate_marks_file(path)
```

## Arguments

- path:

  Path to a marks `.yaml`/`.yml`/`.json` file.

## Value

`TRUE` when the file conforms to the schema, otherwise `FALSE` with the
validation errors attached as attributes (see
[`jsonvalidate::json_validate()`](https://docs.ropensci.org/jsonvalidate/reference/json_validate.html)).

## See also

[`validate_rubric_file()`](https://rundel.github.io/markermd/reference/validate_rubric_file.md),
[`validate_template_file()`](https://rundel.github.io/markermd/reference/validate_template_file.md)
