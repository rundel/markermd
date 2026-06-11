# Validate a template file against the markermd JSON Schema

Structurally validates a template YAML (or JSON) file against the
bundled JSON Schema (`inst/schema/markermd-template.json`). This is an
optional check aimed at tooling and LLM-generated files; the
authoritative validation happens via the S7 constructors in
[`read_template_yaml()`](https://rundel.github.io/markermd/reference/read_template_yaml.md).
Requires the suggested `jsonvalidate` package.

## Usage

``` r
validate_template_file(path)
```

## Arguments

- path:

  Path to a template `.yaml`/`.yml`/`.json` file.

## Value

`TRUE` when the file conforms to the schema, otherwise `FALSE` with the
validation errors attached as attributes (see
[`jsonvalidate::json_validate()`](https://docs.ropensci.org/jsonvalidate/reference/json_validate.html)).
