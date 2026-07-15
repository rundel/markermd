# Record grading marks for one repository/question pair

Programmatically marks a single (repository, question) pair in the
project's grading database, without going through a marks YAML file: the
one-off counterpart to
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md).
Rubric items are identified by their description text, matched verbatim.

## Usage

``` r
marks_set(
  repo,
  question,
  items = NULL,
  comment = NULL,
  private_comment = NULL,
  project = ".",
  overwrite = FALSE
)
```

## Arguments

- repo:

  Repository name (a directory under the project's repos directory).

- question:

  Question name from the project's template.

- items:

  Character vector of rubric item descriptions to select (`character(0)`
  to deselect everything), or `NULL` to leave selections unchanged.

- comment:

  Public, student-facing comment; an empty string clears the stored
  comment, `NULL` leaves it unchanged.

- private_comment:

  Private grader note, never shown to students; an empty string clears
  it, `NULL` leaves it unchanged.

- project:

  Path to the project directory. Defaults to the working directory.

- overwrite:

  When `TRUE`, a pair with existing grading activity is re-marked
  instead of raising an error.

## Value

Invisibly, a one-row data frame with columns `repo`, `question`,
`action` (`"written"`), and `n_selected` (`NA` when `items` is `NULL`).

## Details

`items` is declarative: the listed items are selected and every other
rubric item of the question is deselected, so `character(0)` records
that no items apply. `items = NULL` leaves the pair's selections
untouched and only writes the supplied comments. At least one of
`items`, `comment`, or `private_comment` must be supplied.

Unlike
[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
which skips pairs that already have grading activity, this targeted
setter errors on such a pair unless `overwrite = TRUE`, so a script
cannot believe a write happened when it was ignored.

## See also

[`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)

## Examples

``` r
if (FALSE) { # \dontrun{
marks_set("student1-hw01", "Question 2",
  items = "Looks good",
  private_comment = "Quartiles and summary statistics all present.",
  project = "hw01"
)
} # }
```
