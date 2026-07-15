# Write a markermd template to a YAML file

Serializes a `markermd_template` to a human-readable, schema-validatable
YAML file. The assignment document is referenced by `source_path`
(recorded under `source.path`) rather than embedded; the parsed AST is
regenerated on load by re-parsing that document.

## Usage

``` r
write_template_yaml(template, path, source_path = NULL)
```

## Arguments

- template:

  A `markermd_template` object.

- path:

  Output file path (`.yaml`).

- source_path:

  Optional path to the assignment document to record so the template can
  be re-opened and re-validated later.

## Value

The output `path`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
tmpl = read_template_yaml("hw01-template.yaml")
write_template_yaml(tmpl, "hw01-template-copy.yaml")
} # }
```
