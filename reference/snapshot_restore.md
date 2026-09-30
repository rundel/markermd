# Restore a snapshot's grading state

Writes a snapshot's rubric and marks (and optionally its template) back
into the project database, undoing every change made since. An automatic
snapshot of the current state is taken first, so a restore can itself be
undone. The parts are written through the same paths as the imports, in
one transaction: the template is stored, each chosen question's rubric
items, groups and scoring are replaced, and each chosen repository's
selections and comments are set to the snapshot's (pairs the snapshot
did not know are cleared). The restored scores are then recomputed and
compared with the scores the snapshot recorded; a mismatch aborts and
rolls everything back.

## Usage

``` r
snapshot_restore(
  id,
  project = ".",
  part = c("rubric", "marks"),
  question = NULL,
  repo = NULL,
  summary = NULL
)
```

## Arguments

- id:

  The snapshot: its id, a unique prefix of it, a unique label,
  `"latest"`, or a non-positive integer counting back from the latest
  (`0` is the latest, `-1` the one before it).

- project:

  Path to the project directory. Defaults to the working directory.

- part:

  Which parts to restore: any of `"rubric"`, `"marks"`, `"template"`.
  Defaults to the rubric and the marks.

- question:

  Optional character vector of question names to restore (rubric and
  marks). Defaults to every question in the snapshot.

- repo:

  Optional character vector of repository names whose marks to restore.
  Defaults to every repository in the snapshot.

- summary:

  Optional summary recorded on the automatic pre-restore snapshot: why
  the restore was needed.

## Value

Invisibly, a list with `snapshot_id` (the restored snapshot) and
`pre_restore` (the id of the snapshot taken before restoring, or `NULL`
when the state was already recorded).

## Details

Restoring the rubric alone drops every selection of an item the snapshot
does not have, and restoring the marks alone can leave selections that
name items the current rubric lacks, so the default restores both and a
partial restore warns. `question` and `repo` narrow the restore ("undo
the review for Q1c only"). Repositories in the snapshot that no longer
exist under the project's repos directory are skipped with a warning. A
question the snapshot has that is not in the current template aborts
unless `"template"` is among the parts.

## See also

[`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md),
[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md)

## Examples

``` r
if (FALSE) { # \dontrun{
snapshot_restore("Before review", project = "hw01",
  summary = "The review's Q1c severities were wrong; going back to re-do them.")

# Only one question
snapshot_restore("Before review", project = "hw01", question = "Q1c")
} # }
```
