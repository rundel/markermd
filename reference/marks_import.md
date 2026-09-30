# Import grading marks from YAML into a project's database

Reads a marks YAML file (see
[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)
for the format) and records the rubric item selections and comments it
describes in the project's grading database, as if a grader had toggled
them in [`mark()`](https://rundel.github.io/markermd/reference/mark.md).
Rubric items are identified by their description text, matched verbatim
against each question's rubric.

## Usage

``` r
marks_import(
  path,
  project = ".",
  overwrite = FALSE,
  repo = NULL,
  question = NULL,
  snapshot = TRUE,
  summary = NULL
)
```

## Arguments

- path:

  Path to a marks `.yaml`/`.yml` file. Absolute or relative to the
  current directory; a bare relative path is also resolved against the
  project root.

- project:

  Path to the project directory. Defaults to the working directory.

- overwrite:

  When `TRUE`, pairs with existing grading activity are re-marked
  instead of skipped.

- repo:

  Optional character vector restricting the import to those repository
  names within the file.

- question:

  Optional character vector restricting the import to those question
  names within the file.

- snapshot:

  Whether to take an automatic snapshot before an import that overwrites
  existing grading. `FALSE` is for scripts that take their own snapshot
  once before several imports.

- summary:

  Optional text recorded on the automatic snapshot: what the import
  changes and why.

## Value

Invisibly, a data frame with one row per (repository, question) pair:
columns `repo`, `question`, `action` (`"written"` or `"skipped"`), and
`n_selected`.

## Details

Each (repository, question) entry is declarative: the listed items are
selected and every other rubric item of that question is explicitly
deselected, so `items: []` records that no items apply. A pair imported
with no selected items and no public comment still shows as ungraded in
[`mark()`](https://rundel.github.io/markermd/reference/mark.md) until a
human confirms it, by design: machine-written marks are suggestions
awaiting review.

Pairs that already have any grading activity (any recorded selection
event, or a non-empty public or private comment) are skipped and
reported unless `overwrite = TRUE`, so an automated pass cannot silently
clobber a human's grading. An omitted `comment` / `private_comment`
field leaves the stored comment unchanged, while an empty string clears
it.

All validation (repository names against the project's repos directory,
question names against the stored template, item descriptions against
each question's rubric, and at most one selected item per exclusive item
group) happens before anything is written, and the import itself is a
single transaction.

When the import re-marks pairs that already have grading activity
(`overwrite = TRUE`), an automatic snapshot of the state being replaced
is taken in the same transaction (see
[`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md));
an import that only fills empty pairs replaces nothing and takes none.

## See also

[`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md),
[`marks_set()`](https://rundel.github.io/markermd/reference/marks_set.md),
[`read_marks_yaml()`](https://rundel.github.io/markermd/reference/read_marks_yaml.md),
[`validate_marks_file()`](https://rundel.github.io/markermd/reference/validate_marks_file.md),
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md)

## Examples

``` r
if (FALSE) { # \dontrun{
marks_import("hw01-marks.yaml", project = "hw01")

# Re-mark pairs that already have grading activity, recording why
marks_import("hw01-marks.yaml", project = "hw01", overwrite = TRUE,
  summary = "Adversarial re-check of the first-pass grading")
} # }
```
