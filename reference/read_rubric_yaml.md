# Read a grading rubric from a YAML file

Parses a rubric YAML file into the rubric list structure used by
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md)
and
[`write_rubric_yaml()`](https://rundel.github.io/markermd/reference/write_rubric_yaml.md).
Item order in the file determines display order and keyboard hotkeys:
the first ten items of a question get the keys 1-9 and 0, and any
further items have no hotkey and are toggled by clicking in
[`mark()`](https://rundel.github.io/markermd/reference/mark.md). There
is no limit on the number of items. A group entry in the items list
(format `"1.1"`) is flattened: its members join the question's item list
in place, each with its `group` set, and the group itself is recorded in
the question's `groups`. Validation is performed by the S7 constructors
as items and groups are built.

## Usage

``` r
read_rubric_yaml(path)
```

## Arguments

- path:

  Path to a rubric `.yaml`/`.yml` file.

## Value

A rubric list: `format_version` plus `questions`, each with `name`,
`items` (a list of `markermd_rubric_item` objects), `groups` (a named
list of `markermd_rubric_group` objects, empty when the question has
none) and `scoring` (a `markermd_grade_state` object, or `NULL` when the
file omits it).

## See also

[`write_rubric_yaml()`](https://rundel.github.io/markermd/reference/write_rubric_yaml.md),
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
[`validate_rubric_file()`](https://rundel.github.io/markermd/reference/validate_rubric_file.md)
