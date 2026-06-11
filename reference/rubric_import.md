# Import a grading rubric from YAML into a project's database

Reads a rubric YAML file (see
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md)
for the format) and applies it to the project's grading database. Item
order in the file determines display order and keyboard hotkeys. This is
the inverse of
[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md).

## Usage

``` r
rubric_import(
  path,
  project = ".",
  mode = c("append", "replace"),
  question = NULL
)
```

## Arguments

- path:

  Path to a rubric `.yaml`/`.yml` file. Absolute or relative to the
  current directory; a bare relative path is also resolved against the
  project root.

- project:

  Path to the project directory. Defaults to the working directory.

- mode:

  Either `"append"` or `"replace"`; see Details.

- question:

  Optional character vector restricting the import to those question
  names within the file.

## Value

The parsed rubric list, invisibly.

## Details

With `mode = "append"` (the default) the file's items are added after
each question's existing items. With `mode = "replace"` each affected
question's existing items are deleted first; this also deletes any
recorded selections of those items, for every repository, and cannot be
undone. A question's scoring setup is only updated when the file
provides a `scoring` block.

Question names in the file must exactly match question names in the
project's stored template; mismatches abort before anything is written.

## See also

[`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md),
[`read_rubric_yaml()`](https://rundel.github.io/markermd/reference/read_rubric_yaml.md),
[`validate_rubric_file()`](https://rundel.github.io/markermd/reference/validate_rubric_file.md)
