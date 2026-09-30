# Changelog

## markermd 0.1.0

- Initial version.
- Added
  [`parse_project()`](https://rundel.github.io/markermd/reference/parse_project.md),
  a headless per-repository parse check, and the bundled
  `markermd-fix-parsing` skill that repairs submissions the strict q2r
  parser rejects on a `grading-fixes` branch.
- The bundled skills now ship in two sets, for Claude Code
  (`inst/skills/claude/`, installed to `.claude/skills/`) and Codex
  (`inst/skills/codex/`, installed to `.agents/skills/`);
  [`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)
  gained an `agents` argument (default: both) and
  [`markermd_skills_path()`](https://rundel.github.io/markermd/reference/markermd_skills_path.md)
  an `agent` argument.
- [`mark()`](https://rundel.github.io/markermd/reference/mark.md) now
  opens on an Assignments tab holding one table with a row per
  repository: its folder, GitHub, report and source links, its
  validation status and grading progress, and its score per question and
  in total as entered through the rubric, with the graded count, mean,
  median and standard deviation per question pinned beneath as summary
  rows. The table filters by name and status, sorts by any column, and
  updates as grading proceeds. The validation results moved to their own
  Validation tab, with a repository selector kept in step with the rest
  of the app; clicking a repository’s validation marker in the table
  opens it there, and clicking a score opens that repository and
  question in the Rubric tab. Repository names in the table are plain
  text rather than selection buttons. The table is a `reactable` (added
  to Imports); `gt` is no longer a dependency.
- The Validation tab’s repository selector now shows each repository’s
  count of passed questions, and a question selector beside it lists
  each question with a coloured pass or fail mark for the current
  repository; choosing one scrolls that question’s results into view.
  Each question’s results card collapses and expands from a chevron in
  its header, a collapsed question stays collapsed across repositories,
  and the document tree in each card is no longer capped in height.
- [`mark()`](https://rundel.github.io/markermd/reference/mark.md) is now
  permissive about where a repo’s rendered report lives: any HTML file
  named for the repo, or anywhere under a directory named for the repo
  (`<repo>/`, or `<repo>` plus a delimiter such as ghclass’s
  `<repo>_<artifact-name>/`), is accepted when it is the only one.
  Previously only three exact layouts were checked, so projects laid out
  by
  [`ghclass::org_grade_assignment()`](https://rundel.github.io/ghclass/reference/org_grade_assignment.html)
  showed “No Artifact Available” for every repo. When several HTML files
  are found for a repo, a heuristic modelled on the ghclass layout picks
  the most likely report (rendering resource directories such as
  `*_files/` rank last), a consolidated warning at launch names each
  affected repo and its choice, and the Content pane shows a notice
  above the report.
- Rubric items are no longer effectively limited to ten per question.
  The first ten keep the hotkeys 1-9 and 0; further items are toggled by
  clicking (their button is blank with a dashed outline). Display order
  is now stored explicitly in the grading database (`items.position`,
  schema version 2, migrated in place the first time a project is
  opened), so reordering items past the tenth survives a restart and
  export; previously those items always reloaded in insertion order.
- Raw blocks that are nothing but `<!-- ... -->` comments are now their
  own node kind, `HTML comment`, so rules and filters distinguish
  `{=html}` write-ups (still `Raw Block`) from skeleton placeholders and
  repair markers, and
  [`assignment_outline()`](https://rundel.github.io/markermd/reference/assignment_outline.md)
  no longer counts comments as content.
- Template questions have a `scope`: `"sections"` (the default) targets
  the selected headings and divs, and `"document"` covers the document
  as a whole, for problems that belong to no single question (a render
  setting in the YAML header, global chunk options, a document that does
  not render). A document-scope question selects no nodes; its filters
  and rules, if any, evaluate against the whole document.
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  gained an “Add Document Entry” button,
  [`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)
  writes document-scope feedback first, and templates with such a
  question are written as format 3.1 (3.0 files still load).
- A question’s scoring can be marked `optional`: an optional question
  counts as graded for every repository and scores as if no rubric item
  were selected until something is recorded, so a document-level
  question that applies to few submissions needs no selection in the
  rest and no longer leaves their totals `NA`. It is set from the grade
  settings popover in
  [`mark()`](https://rundel.github.io/markermd/reference/mark.md) or
  with `optional: true` in the rubric YAML (format 1.2, written only
  when used), stored in `settings.optional` (schema version 4, migrated
  in place), and marked with a small red dot beside the question’s name
  in the Assignments table.
- The bundled skills record document-level problems once in a
  document-scope question (0 points, optional, not bounded below zero)
  added only when a submission needs one, instead of spreading them
  across questions, and after a `mode = "replace"` rubric import give
  pairs left with no selection their confirmation item.
- [`project_set()`](https://rundel.github.io/markermd/reference/project_set.md)
  gained `artifacts`, to record the directories of rendered reports (or
  none) without editing `config.yml`.
- Re-running
  [`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)
  now keeps the configured artifact directories (dropping any that no
  longer exist) instead of re-deriving them from every top-level folder,
  which added working folders such as notes or backups; new folders are
  listed instead. `rescan_artifacts = TRUE` restores the full rescan,
  and `overwrite_instructions = TRUE` replaces existing `CLAUDE.md` /
  `AGENTS.md` files with the packaged versions.
- Snapshots of the grading state. A snapshot is a named, timestamped,
  annotated copy of the template, rubric and marks, kept inside the
  project database (schema version 5; an existing database gains a
  baseline snapshot when first opened). One is taken automatically, in
  the same transaction, before every bulk change:
  [`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md),
  `marks_import(overwrite = TRUE)`,
  [`template_import()`](https://rundel.github.io/markermd/reference/template_import.md),
  `project_set(template = )`,
  [`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md),
  saving in
  [`template()`](https://rundel.github.io/markermd/reference/template.md),
  and in [`mark()`](https://rundel.github.io/markermd/reference/mark.md)
  a rubric import, deleting an item that has selections, or deleting a
  group. Those functions take `snapshot` and `summary` arguments so the
  reason for a change is recorded with it; an automatic snapshot of an
  unchanged state becomes a note on the latest snapshot instead of a new
  row.
  [`snapshot_create()`](https://rundel.github.io/markermd/reference/snapshot_create.md)
  takes a named one by hand;
  [`snapshot_list()`](https://rundel.github.io/markermd/reference/snapshot_list.md),
  [`snapshot_info()`](https://rundel.github.io/markermd/reference/snapshot_info.md)
  and
  [`snapshot_annotate()`](https://rundel.github.io/markermd/reference/snapshot_annotate.md)
  inspect and annotate them;
  [`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md)
  reports what changed between two snapshots or since one (rubric items
  added, removed, reworded, re-pointed or regrouped, group bounds and
  scoring, per-repository selection and comment changes, per-pair score
  deltas and totals before and after, template questions changed);
  [`snapshot_scores()`](https://rundel.github.io/markermd/reference/snapshot_scores.md)
  returns a snapshot’s recorded scores;
  [`snapshot_restore()`](https://rundel.github.io/markermd/reference/snapshot_restore.md)
  writes a snapshot’s rubric and marks (optionally its template, or only
  chosen questions and repositories) back through the import paths in
  one transaction, after snapshotting the current state, and aborts if
  the restored scores differ from the recorded ones;
  [`snapshot_export()`](https://rundel.github.io/markermd/reference/snapshot_export.md)
  writes a snapshot as YAML files with a `snapshot.yaml` sidecar;
  [`snapshot_pin()`](https://rundel.github.io/markermd/reference/snapshot_pin.md),
  [`snapshot_delete()`](https://rundel.github.io/markermd/reference/snapshot_delete.md)
  and
  [`snapshot_prune()`](https://rundel.github.io/markermd/reference/snapshot_prune.md)
  manage retention;
  [`project_backup()`](https://rundel.github.io/markermd/reference/project_backup.md)
  copies the database file with `VACUUM INTO`.
  [`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md),
  [`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)
  and
  [`template_export()`](https://rundel.github.io/markermd/reference/template_export.md)
  gained a `snapshot` argument to export a past state,
  [`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md)
  and
  [`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)
  record which snapshot each export came from, and
  [`project_sitrep()`](https://rundel.github.io/markermd/reference/project_sitrep.md)
  reports the snapshot state and whether the exports are stale.
- [`mark()`](https://rundel.github.io/markermd/reference/mark.md) gained
  a History tab listing the snapshots with a detail pane (overview and
  notes, changes against the current state or another snapshot, and the
  recorded scores) and the take, restore, export, pin, delete and backup
  actions; the `s` key opens the take-snapshot dialog; the rubric import
  dialog takes a summary; and the navbar shows when the latest snapshot
  was taken and how many selections were recorded since.
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  gained a Snapshots menu to take a snapshot or load a snapshot’s
  template into the editor.
- The bundled skills pass a `summary` to every import and report a
  change with
  [`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md);
  the generated `CLAUDE.md` / `AGENTS.md` describe the snapshot
  functions and drop the need for hand-made backup copies.
