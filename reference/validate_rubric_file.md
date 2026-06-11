# Validate a rubric file against the markermd JSON Schema

Structurally validates a rubric YAML (or JSON) file against the bundled
JSON Schema (`inst/schema/markermd-rubric.json`). This is an optional
check aimed at tooling and LLM-generated files; the authoritative
validation happens via the S7 constructors in
[`read_rubric_yaml()`](https://rundel.github.io/markermd/reference/read_rubric_yaml.md).
Requires the suggested `jsonvalidate` package.

## Usage

``` r
validate_rubric_file(path)
```

## Arguments

- path:

  Path to a rubric `.yaml`/`.yml`/`.json` file.

## Value

`TRUE` when the file conforms to the schema, otherwise `FALSE` with the
validation errors attached as attributes (see
[`jsonvalidate::json_validate()`](https://docs.ropensci.org/jsonvalidate/reference/json_validate.html)).

## See also

[`validate_template_file()`](https://rundel.github.io/markermd/reference/validate_template_file.md)
