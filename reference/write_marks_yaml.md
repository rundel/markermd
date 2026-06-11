# Write grading marks to a YAML file

Serializes grading marks (per-repository rubric item selections plus
optional public and private comments) to a human-readable,
schema-validatable YAML file. Each question entry is declarative: the
listed rubric item descriptions are the selected items, and `items: []`
states that no items apply.

## Usage

``` r
write_marks_yaml(marks, path)
```

## Arguments

- marks:

  A marks list as returned by
  [`read_marks_yaml()`](https://rundel.github.io/markermd/reference/read_marks_yaml.md)
  or built by
  [`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md):
  `format_version` plus a `repos` list whose entries have `name` and
  `questions`, each question with `name`, `items` (a character vector of
  rubric item descriptions) and optionally `comment` /
  `private_comment`.

- path:

  Output file path (`.yaml`).

## Value

The output `path`, invisibly.

## See also

[`read_marks_yaml()`](https://rundel.github.io/markermd/reference/read_marks_yaml.md),
[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md),
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md)
