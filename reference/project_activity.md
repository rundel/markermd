# The activity log of a project

Lists what has changed the project's grading state: every call of a
markermd function that changed it
([`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md),
[`marks_set()`](https://rundel.github.io/markermd/reference/marks_set.md),
[`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
[`rubric_item_set()`](https://rundel.github.io/markermd/reference/rubric_item_set.md),
[`template_import()`](https://rundel.github.io/markermd/reference/template_import.md),
`project_set(template = ...)`,
[`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md)),
the bulk operations made in
[`mark()`](https://rundel.github.io/markermd/reference/mark.md) (rubric
imports and edits, item and group deletions, restores, recorded with the
actor `"app"`), and the grading done in
[`mark()`](https://rundel.github.io/markermd/reference/mark.md), grouped
into sessions of activity. Each recorded call notes who made it: the
`MARKERMD_ACTOR` environment variable when set, `"claude"` or `"codex"`
when the call ran under one of those agent CLIs, and `"R"` otherwise.
Grading recorded before the log existed lists as app grading.

## Usage

``` r
project_activity(
  project = ".",
  since = NULL,
  actor = NULL,
  app = TRUE,
  gap = 15 * 60
)
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

- since:

  Optional date-time (or something
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) accepts):
  only entries whose latest change is at or after it.

- actor:

  Optional character vector: only entries by these actors (`"app"` for
  grading in
  [`mark()`](https://rundel.github.io/markermd/reference/mark.md)).

- app:

  Whether to include the grading done in
  [`mark()`](https://rundel.github.io/markermd/reference/mark.md).

- gap:

  Seconds without activity that end one app grading session.

## Value

A data frame, newest first, with columns `id`, `kind` (`"action"` or
`"app"`), `created_at` (the call, or an app session's last change),
`started_at`, `actor`, `username`, `fn`, `call`, `summary`, `n_pairs`,
`n_events`, `snapshot_id` (the snapshot taken before the change, if any)
and the list columns `repos`, `questions` and `parts` (the template /
rubric parts an action changed).

## Details

Use
[`activity_detail()`](https://rundel.github.io/markermd/reference/activity_detail.md)
for what one entry changed.

## See also

[`activity_detail()`](https://rundel.github.io/markermd/reference/activity_detail.md),
[`snapshot_list()`](https://rundel.github.io/markermd/reference/snapshot_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
project_activity("hw01", actor = "claude")
} # }
```
