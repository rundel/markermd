# Read a grading rubric from a YAML file

Parses a rubric YAML file into the rubric list structure used by
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md)
and
[`write_rubric_yaml()`](https://rundel.github.io/markermd/reference/write_rubric_yaml.md).
Item order in the file determines display order and keyboard hotkeys
(1-10 by position, none beyond ten). Validation is performed by the S7
constructors as items are built.

## Usage

``` r
read_rubric_yaml(path)
```

## Arguments

- path:

  Path to a rubric `.yaml`/`.yml` file.

## Value

A rubric list: `format_version` plus `questions`, each with `name`,
`items` (a list of `markermd_rubric_item` objects) and `scoring` (a
`markermd_grade_state` object, or `NULL` when the file omits it).

## See also

[`write_rubric_yaml()`](https://rundel.github.io/markermd/reference/write_rubric_yaml.md),
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
[`validate_rubric_file()`](https://rundel.github.io/markermd/reference/validate_rubric_file.md)
