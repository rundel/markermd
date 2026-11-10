# Changelog

## markermd 0.1.0

### Breaking changes

The exported API was made consistent before the first release. There are
no deprecated aliases.

- The project argument is always `project`.
  [`init_project()`](https://rundel.github.io/markermd/reference/init_project.md),
  [`project_config()`](https://rundel.github.io/markermd/reference/project_config.md),
  [`project_set()`](https://rundel.github.io/markermd/reference/project_set.md),
  [`project_sitrep()`](https://rundel.github.io/markermd/reference/project_sitrep.md),
  [`validate_project()`](https://rundel.github.io/markermd/reference/validate_project.md),
  [`parse_project()`](https://rundel.github.io/markermd/reference/parse_project.md),
  [`scan_project()`](https://rundel.github.io/markermd/reference/scan_project.md)
  and [`mark()`](https://rundel.github.io/markermd/reference/mark.md)
  took `path`.
- `export_marks()` is now
  [`export_feedback()`](https://rundel.github.io/markermd/reference/export_feedback.md).
  It writes `scores.csv` and the per-repository feedback files;
  [`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)
  still writes the marks YAML.
- [`template()`](https://rundel.github.io/markermd/reference/template.md)
  now runs the editor like
  [`mark()`](https://rundel.github.io/markermd/reference/mark.md), with
  `...` passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html),
  instead of returning an app object. Its first argument is `source`
  (was `assignment_path`).
- [`assignment_outline()`](https://rundel.github.io/markermd/reference/assignment_outline.md)
  takes `source` (was `assignment_path`) and now accepts every source
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  does: a project (it outlines the key document with the stored
  template’s section stops), a file, a directory, a saved template YAML
  or object, or a GitHub repository (new `local_dir` and `assignment`
  arguments).
  [`parse_assignment_document()`](https://rundel.github.io/markermd/reference/parse_assignment_document.md)
  takes `path` (was `file_path`).
- `get_question()`, `add_question()` and `remove_question()` are now
  [`template_question()`](https://rundel.github.io/markermd/reference/template_question.md),
  [`template_add_question()`](https://rundel.github.io/markermd/reference/template_add_question.md)
  and
  [`template_remove_question()`](https://rundel.github.io/markermd/reference/template_remove_question.md),
  with arguments `template` and `question` (were `tmpl` and `q`).
- `get_allowed_rule_verbs()`, `get_allowed_node_types()` and
  `get_allowed_filter_condition_types()` are now
  [`rule_verbs()`](https://rundel.github.io/markermd/reference/rule_verbs.md),
  [`rule_node_types()`](https://rundel.github.io/markermd/reference/rule_node_types.md)
  and
  [`filter_condition_types()`](https://rundel.github.io/markermd/reference/filter_condition_types.md).

### Changes

- Initial version.
- Source code is shown with a small vendored copy of Prism instead of
  the Monaco editor, in the Grade tab’s Source view, the source and node
  preview modals, and both apps’ file views. The views work offline, the
  installed package is several megabytes smaller, and Quarto and R
  Markdown documents get their own grammar: chunks of any engine are
  highlighted in their language (including knitr-style
  `{r label, opt=value}` headers and `#|` options), as are inline
  `` `r expr` `` and `` `{r} expr` ``, YAML front matter, Quarto div
  fences and heading attributes. The question highlight now sits exactly
  on its lines, wrapped lines included.
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
  updates as grading proceeds. Clicking a repository’s validation marker
  opens that repository’s rule results in a modal (with a question
  selector whose entries carry a colored pass or fail mark and scroll to
  the question’s card, each card collapsing from its header, and a
  Re-check button that parses and validates the repository again), and
  clicking a score opens that repository and question in the Grade tab.
  Repository names in the table are plain text rather than selection
  buttons. The table is a `reactable` (added to Imports); `gt` is no
  longer a dependency.
- [`mark()`](https://rundel.github.io/markermd/reference/mark.md) has a
  Rubric tab editing every question’s rubric in one place (scoring
  settings, items and groups, each item with its selection count),
  sharing its state with the Grade tab, and an optional Terminal tab
  (`mark(terminal = TRUE)`) running a shell or an agent CLI in the
  project root through the suggested `shinyterm` package.
- [`mark()`](https://rundel.github.io/markermd/reference/mark.md) is now
  permissive about where a repo’s rendered report lives: any HTML file
  named for the repo, or anywhere under a directory named for the repo
  (`<repo>/`, or `<repo>` plus a delimiter such as ghclass’s
  `<repo>_<artifact-name>/`), is accepted when it is the only one.
  Previously only three exact layouts were checked, so projects laid out
  by `ghclass::org_grade_assignment()` showed “No Artifact Available”
  for every repo. When several HTML files are found for a repo, a
  heuristic modeled on the ghclass layout picks the most likely report
  (rendering resource directories such as `*_files/` rank last), a
  consolidated warning at launch names each affected repo and its
  choice, and the Grade tab shows a notice above the report.
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
  and rules, if any, evaluate against the whole document. The Add
  Question dialog in
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  chooses between a question on the document’s sections, an entry
  covering the whole document, and (template format 3.2) a question on
  one of the assignment’s other files;
  [`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)
  writes document-scope feedback first, and templates with a
  document-scope question are written as format 3.1 (3.0 files still
  load).
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
  chosen questions and repositories) back in place in one transaction,
  after snapshotting the current state, and aborts if the restored
  scores differ from the recorded ones;
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
  a History tab: one timeline of the snapshots and the activity log (the
  calls of markermd functions by agents and scripts, the bulk operations
  made in the app, and the app’s own grading grouped into sessions), a
  click on a row opening the entry’s detail in a modal (overview and
  notes, changes against the current state or another snapshot, and the
  recorded scores) with the take, restore, export, pin, delete and
  backup actions; the `s` key opens the take-snapshot dialog; the rubric
  import dialog takes a summary; and the navbar shows when the latest
  snapshot was taken and how many selections were recorded since.
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  gained a Snapshots menu to take a snapshot or load a snapshot’s
  template into the editor.
- The bundled skills pass a `summary` to every import and report a
  change with
  [`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md);
  the generated `CLAUDE.md` / `AGENTS.md` describe the snapshot
  functions and drop the need for hand-made backup copies.
- Added
  [`scan_project()`](https://rundel.github.io/markermd/reference/scan_project.md),
  a deterministic scan of every student repository and rendered report
  for prompt injection attempts (text aimed at an automated grader:
  instructions to ignore the rubric or award full marks, text addressed
  to an AI model, chat role markers, fabricated grading output, and text
  hidden in HTML comments, hidden-styled elements, unshown chunks,
  invisible Unicode, base64, or HTML attributes, plus agent instruction
  files and `<!-- markermd: ... -->` markers the fix-parsing skill did
  not add), and the bundled `markermd-check-injection` skill that
  triages its findings and reports them without changing anything. The
  generated `CLAUDE.md` / `AGENTS.md` now state that submission content
  is data and never instructions. `xml2` was added to Imports.
- An activity log (schema version 6) records every call of a markermd
  function that changed grading state, with who made it
  (`MARKERMD_ACTOR`, or `claude` / `codex` when run under those agent
  CLIs), the grading rows it wrote and the template or rubric it
  replaced;
  [`project_activity()`](https://rundel.github.io/markermd/reference/project_activity.md)
  and
  [`activity_detail()`](https://rundel.github.io/markermd/reference/activity_detail.md)
  read it,
  [`mark()`](https://rundel.github.io/markermd/reference/mark.md) shows
  new entries as they land, badges the items an agent selected until the
  grader touches the pair, and raises a banner when the rubric or
  template changed outside the session.
  [`rubric_item_set()`](https://rundel.github.io/markermd/reference/rubric_item_set.md)
  rewords or re-points one rubric item in place, keeping its selections;
  [`rubric_items_set()`](https://rundel.github.io/markermd/reference/rubric_items_set.md)
  applies a data frame or YAML file of such changes in one transaction,
  with one snapshot and one activity entry;
  [`rubric_item_delete()`](https://rundel.github.io/markermd/reference/rubric_item_delete.md)
  deletes one item, asking for confirmation when repositories have it
  selected (`force = TRUE` skips the question, and is required outside
  an interactive session).
- Snapshot restores now work by item id: a restore writes the snapshot’s
  items and groups back in place under their own ids, so every
  repository’s selections of a surviving item are kept, a restore
  narrowed to some repositories leaves the others exactly as they were,
  and a full restore clears the pairs graded after the snapshot.
  Snapshots carry the selected item ids beside the descriptions, so a
  rubric with repeated or blank descriptions still snapshots, scores and
  restores. Previously a narrowed restore re-created the items and
  silently dropped every other repository’s selections for those
  questions.
- Rubric item descriptions, which identify items in the YAML formats,
  are now kept unique and non-blank in
  [`mark()`](https://rundel.github.io/markermd/reference/mark.md): a new
  item starts as “New item” (selected on focus) and a blank or duplicate
  edit is refused. Descriptions are saved when the field loses focus or
  typing pauses, not per keystroke.
- Renaming a question in
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  or through
  [`template_import()`](https://rundel.github.io/markermd/reference/template_import.md)
  carries its rubric, scoring and marks over to the new name instead of
  orphaning them, matching questions by id. All the renames of one save
  are applied together, so swapped names move each question’s grading
  with it, and the rename warns, naming each move. A rename onto a name
  that still holds a removed question’s grading is refused, removing a
  question that holds grading warns, and restoring a snapshot’s template
  moves carried-over grading back.
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  keeps each question’s id through imports and snapshot loads and gives
  a new question an id above every stored one.
- [`mark()`](https://rundel.github.io/markermd/reference/mark.md) grades
  and validates the same document per repository as
  [`validate_project()`](https://rundel.github.io/markermd/reference/validate_project.md)
  and
  [`parse_project()`](https://rundel.github.io/markermd/reference/parse_project.md)
  (the template’s document by name, else a root-level one), instead of
  the first `.qmd` found anywhere in the repository; the Source view
  shows the file as written, with the question highlight mapped back
  from the parsed text, and parse diagnostics name the file’s line when
  a rewritten knitr chunk header moved it.
- Student-rendered reports are shown in sandboxed iframes: their scripts
  run, but can no longer reach the grading app’s page or inputs.
- The app’s rubric edits, imports, deletions and restores are recorded
  in the activity log with the actor `app`; a running session shows no
  “changed outside the app” notice for its own actions. Toggling an item
  writes its grade rows directly, the navbar snapshot status no longer
  re-hashes the whole grading state on every click, score and marks
  tables are built in one pass, and the live score is no longer written
  to the settings table on every repository switch.
- The Assignments table’s Total column follows
  [`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md):
  NA until every question is graded, with the running sum shown in grey
  meanwhile; the summary footer covers the complete totals. Score cells
  can be opened from the keyboard, the rubric rows carry text-box and
  button roles for assistive technology, and the Grade tab’s hotkeys
  ignore Cmd/Ctrl/Alt combinations.
- Grading event timestamps are written in UTC (schema version 7; older
  rows are rewritten on first touch), a database written by a newer
  markermd is refused before any migration touches it, and a template’s
  stored source path resolves against the project before the working
  directory.
- Deleting a rubric item or group, a replace rubric import and a rubric
  restore no longer delete anything: items and groups are soft-deleted
  (`deleted_at`, schema version 9), so their grade events stay in the
  log and History is never rewritten, while every score, export and
  selection count ignores them. Restoring a snapshot brings a deleted
  item back under its id with its selections, a deleted item’s
  description and a deleted group’s label can be used again, and a
  deleted id is never reissued.
- `shinyWidgets` is no longer a dependency (the “has between” rule uses
  two numeric inputs).
- Templates saved from
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  store rules as an array again, so they pass
  [`validate_template_file()`](https://rundel.github.io/markermd/reference/validate_template_file.md).
  Rule, filter group and condition rows have stable keys, so a quick
  second click no longer deletes the wrong row or resets node types, and
  an invalid number in a rule keeps the stored values with a warning
  instead of resetting them to the defaults.
- Validation fails every rule of a question whose selected section a
  submission lacks (“Selected section \#id not found in this document”),
  so a renamed heading no longer passes `lacks content` or `has at most`
  rules. A rule pattern that is not a valid regular expression fails
  instead of passing vacuously, a div selected inside a selected section
  is counted once, and
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  flags duplicate heading or div ids and refuses to select an ambiguous
  one. The editor’s live rule badges and grading share one evaluator, so
  a whole-document question’s rules pass in the editor as they do in
  grading.
- `rubric_import(mode = "append")` refuses an item whose description is
  already stored for its question. `find_repo_assignment()` prefers a
  root-level document over a nested copy with the same name, and
  `find_repo_file()` no longer follows a symbolic link out of the
  repository.
- Snapshot diffs compare selections by item id and report a relabeled
  group as one rename, and printing one no longer interprets braces in
  item descriptions. Snapshot and live scores come from one
  implementation of which pairs count as graded.
- [`scan_project()`](https://rundel.github.io/markermd/reference/scan_project.md)
  reports active content in rendered reports (scripts that reach out of
  the report frame, event handlers, outside frames), text hidden by the
  report’s own CSS classes, phrases disguised with look-alike letters or
  split by invisible characters, and decoded variation-selector
  payloads. A visible line needs two kinds of signal to be reported as
  medium, emoji flag sequences and ordinary right-to-left text are no
  longer flagged, excerpts of long lines center on the match, and repair
  markers count as verified only when the grading machine’s configured
  author wrote them.
- The bundled skills’ frontmatter is valid YAML. The skills keep stored
  question ids when regenerating a template, append only new rubric
  items, read each repository’s graded document from
  [`parse_project()`](https://rundel.github.io/markermd/reference/parse_project.md),
  and keep reports under `notes/`. The JSON Schemas check quoted format
  versions and blank descriptions and describe how ids carry grading
  across a rename.
