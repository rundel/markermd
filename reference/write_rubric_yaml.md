# Write a grading rubric to a YAML file

Serializes a rubric (one or more questions' rubric items plus optional
scoring setup and item groups) to a human-readable, schema-validatable
YAML file. Items are written in display order; order is meaningful, as
the first ten items are bound to keyboard hotkeys on import and any
further items (there is no limit) are toggled by clicking. A group's
items are nested under a group entry (`group`, optional `floor`,
`ceiling` and `exclusive`, then `items`). The file is written as
`format_version` `"1.1"` when it contains a group and `"1.0"` otherwise.
No per-repository grading data is written.

## Usage

``` r
write_rubric_yaml(rubric, path)
```

## Arguments

- rubric:

  A rubric list as returned by
  [`read_rubric_yaml()`](https://rundel.github.io/markermd/reference/read_rubric_yaml.md)
  or
  [`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md):
  `format_version` plus a `questions` list whose entries have `name`,
  `items` (a list of `markermd_rubric_item` objects in display order,
  each item's `group` naming its group or `NA`), optionally `groups` (a
  named list of `markermd_rubric_group` objects keyed by group id) and
  optionally `scoring` (a `markermd_grade_state` object).

- path:

  Output file path (`.yaml`).

## Value

The output `path`, invisibly.

## See also

[`read_rubric_yaml()`](https://rundel.github.io/markermd/reference/read_rubric_yaml.md),
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md),
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md)
