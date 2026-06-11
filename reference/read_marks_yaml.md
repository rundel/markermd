# Read grading marks from a YAML file

Parses a marks YAML file into the marks list structure used by
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md)
and
[`write_marks_yaml()`](https://rundel.github.io/markermd/reference/write_marks_yaml.md).
Rubric items are identified by their description text, which must match
the question's rubric verbatim; the match itself is performed (and
validated) by
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
not here.

## Usage

``` r
read_marks_yaml(path)
```

## Arguments

- path:

  Path to a marks `.yaml`/`.yml` file.

## Value

A marks list: `format_version` plus `repos`, each with `name` and
`questions`, each question with `name`, `items` (a character vector of
rubric item descriptions) and `comment` / `private_comment` (a string,
or `NULL` when the file omits it).

## See also

[`write_marks_yaml()`](https://rundel.github.io/markermd/reference/write_marks_yaml.md),
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
[`validate_marks_file()`](https://rundel.github.io/markermd/reference/validate_marks_file.md)
