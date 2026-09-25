# markermd grading project

This directory is a markermd grading project: a `ghclass::org_grade_assignment()`
layout initialized with `markermd::init_project()`. The markermd R package
(https://github.com/rundel/markermd) supplies the grading tools. Run them
headlessly with `Rscript -e '...'`; if a command fails because markermd is not
installed, ask the user to install it before doing anything else.

## Layout

- `.markermd/config.yml` is the project config. `paths.repos` is the directory
  of student repositories, `paths.key` the key (solution) repository,
  `paths.comments` the directory exported feedback is written to, and
  `artifacts` lists directories of downloaded rendered reports. Paths are
  relative to this directory. Read the config rather than guessing where
  things are; `Rscript -e 'markermd::project_sitrep(".")'` prints an overview.
- `.markermd/markermd.sqlite` is the grading database and the canonical store
  for the grading template, the rubric, and every recorded mark and comment.
  Never read or write it directly; go through the markermd functions below.
- `repos/<name>/` (or whatever `paths.repos` names) holds one git repository
  per student submission. These are the students' work.
- The key directory (`paths.key`) holds the instructor's solution.
- `markermd-template.yaml`, `markermd-rubric.yaml`, and `markermd-marks.yaml`
  at the project root are scratch import/export files written by the skills
  and the export functions. They are not the source of truth and may be stale;
  regenerate them with the export functions instead of trusting a copy.
- `.claude/skills/` holds the markermd skills. `init_project()` refreshes them
  to the packaged version on every run, so edits made there are lost.

## Workflow

The grading workflow runs in this order, each step with its own skill:

1. `/markermd-fix-parsing` repairs submissions that markermd's strict parser
   rejects. `parse_project()` finds them; every edit is committed on a
   `grading-fixes` branch in the student's repository.
2. `/markermd-scaffold-template` builds the grading template (questions and
   validation rules) from the key and validates it across the submissions.
3. `/markermd-scaffold-rubric` drafts per-question rubric items and scoring.
4. `/markermd-apply-rubric` records a first-pass machine grading as rubric
   selections plus private notes, for human review.
5. `markermd::mark(".")` is where the human grades. `markermd::template(".")`
   is the interactive template editor.

Use the matching skill whenever a request fits one; each skill documents its
own steps, checks, and the questions to ask the user. `mark()` and
`template()` are Shiny apps you cannot drive, so tell the user to run them.

Headless entry points, all taking the project root:

- `project_sitrep()`, `project_config()`, `project_set()`: inspect and edit
  the config.
- `parse_project()`: per-repository parse check, no template needed.
- `validate_project()`: the stored template's rules across every repository.
- `template_export()` / `template_import()`, `rubric_export()` /
  `rubric_import()`, `marks_export()` / `marks_import()` / `marks_set()`: move
  the template, rubric, and marks between the database and YAML. The export
  is the way to learn the current questions, items, and marks.
- `export_scores()`, `export_comments()`, `export_marks()`: final outputs.
- `assignment_outline()`: a document's sections with their exact Pandoc
  anchors and content counts. Anchors in a template always come from here,
  never from guesswork.

## File formats

The template, rubric, and marks each have a YAML form, validated against the
JSON Schemas at `system.file("schema", package = "markermd")`
(`markermd-template.json`, `markermd-rubric.json`, `markermd-marks.json`).
`validate_template_file()`, `validate_rubric_file()`, and
`validate_marks_file()` check a file against its schema (they need the
`jsonvalidate` package); `read_*_yaml()` parses one. Question names are the
key that links the three: the rubric and marks files name questions exactly
as the template does.

### Template (`format_version: "3.0"`)

```yaml
format_version: "3.0"
source:
  path: hw1-key/hw1.qmd
questions:
  - id: 1
    name: Q1
    node_ids:
      - question-1
    rules:
      - node_type: Chunk
        verb: has at least
        count: 1
      - node_type: [Markdown, Raw Block]
        verb: has at least
        count: 1
      - node_type: Chunk
        verb: has content
        pattern: "lm\\("
    filters:
      - conditions:
          - type: has option
            value: "eval: false"
            negate: true
```

- `source.path` is the assignment document, relative to the project root. It
  is re-parsed whenever the template is loaded; the parsed structure is never
  stored. Its basename is how each student repository's document is matched.
- Each question has a unique integer `id`, a unique `name`, `node_ids` (the
  Pandoc heading or div anchors of the sections it targets, taken verbatim
  from `assignment_outline()`), `rules`, and optional `filters`. A rule's
  nodes are everything under the selected headings, subsections included.
- A rule is a `node_type`, a `verb`, and the value key the verb needs:
  `has at least` / `has at most` take `count`; `has between` takes `min` and
  `max`; `has content` / `lacks content` / `has name` take `pattern`, a
  case-insensitive regular expression matched against each node's text
  (for `has name`, against the heading title or chunk label).
- `node_type` is one of `Any node`, `Heading`, `Markdown`, `Chunk`,
  `Code block`, `Raw Block`, `HTML comment`, `Div`, `Bullet list`,
  `Ordered list`, `Block quote`, `Table`, `Figure`, `Horizontal rule`,
  `Definition list`, `Line block`, or a list of them meaning any of those. A
  raw block that is nothing but `<!-- ... -->` comments is an `HTML comment`,
  never a `Raw Block`, so `[Markdown, Raw Block]` counts `{=html}` write-ups
  but not comment placeholders or repair markers; `Any node` still counts them.
- Filters narrow the question's nodes before the rules run. Groups are ORed;
  the `conditions` within a group are ANDed; `negate: true` inverts a
  condition or a whole group. A condition's `type` is `node type` (value: a
  kind or list of kinds), `has class` / `has id` / `has engine` (exact
  match), `has text` (regular expression), `has label` (glob on the chunk
  label), or `has option` (a Quarto cell option as `key` or `key: value`).
- Import with `template_import(path, project)`; export the stored template
  with `template_export()`. The import re-parses `source.path` but does not
  check that each `node_id` exists; a wrong anchor surfaces as a per-repo
  error from `validate_project()`.

### Rubric (`format_version: "1.1"`)

```yaml
format_version: "1.1"
questions:
- name: Q1
  scoring:
    total_score: 10
    grading_mode: negative
    bound_above_zero: true
    bound_below_max: true
  items:
  - points: 0
    description: Looks good
  - points: -10
    description: Question not attempted
  - points: -5
    description: Confidence interval uses the normal quantile instead of qt() with n-1 df
  - group: Examples
    floor: -2
    items:
    - points: -1
      description: No example with a sample size below 30
    - points: -1
      description: Examples do not vary the confidence level
  - group: Write-up
    exclusive: true
    items:
    - points: -3
      description: The interpretation of the interval is missing
    - points: -1.5
      description: The interpretation of the interval is incomplete
```

- `name` must match a stored template question exactly; the import aborts
  on any mismatch before writing.
- `scoring` is optional and only updates the question when present:
  `total_score` (required), `grading_mode` (`positive` builds the score up
  from 0 by adding selected item points; `negative` starts from
  `total_score` and applies the selected, typically negative, points as
  deductions; the default is `positive`), and `bound_above_zero` /
  `bound_below_max` (clamp the score to `[0, total_score]`; both default to
  true).
- Each item is `points` plus a `description`. The description is the item's
  identity: non-empty, unique within its question (across all of its
  groups), and copied verbatim wherever the item is referenced. Item order
  is display order, and the first ten items of a question, counting a
  group's items in place, are bound to the hotkeys 1-9 and 0, so the
  most-used items come first. Give every question a 0-point confirmation item
  (`Looks good`) in negative mode, or a full-credit item in positive mode, so
  a correct answer can be recorded as graded. Descriptions are shown to
  students: phrase them as feedback about the work, never as grading
  mechanics.
- An entry of `items` may instead be an item group: a `group` label (unique
  within the question) with its own `items` and optionally `floor` (`<= 0`),
  `ceiling` (`>= 0`) and `exclusive` (a boolean). The combined points of the
  group's selected items are clamped to `[floor, ceiling]` before the
  question's scoring mode and bounds apply, so a group with nothing selected
  contributes zero and a capped group lets the grader record every defect
  without the deductions stacking past the cap. An exclusive group allows at
  most one selected item (severity tiers); `marks_import()` aborts when a
  pair selects two. Groups cannot nest. A file without groups is written as
  `format_version: "1.0"`, and both versions are read. Group labels reach
  students only when `export_comments(group_headings = TRUE)` is used.
- Import with `rubric_import(path, project, mode = "append")`, which adds the
  file's items after each question's existing ones, merging a group into the
  existing group of the same label (items appended to it, bounds
  overwritten) and creating a group for a new label; `mode = "replace"`
  deletes each affected question's items and groups first, together with
  every recorded selection of the items across all repositories, and cannot
  be undone. Making an existing group exclusive while some repository has
  two of its items selected aborts the import. Export the stored rubric with
  `rubric_export()`.

### Marks (`format_version: "1.0"`)

```yaml
format_version: "1.0"
repos:
- name: hw01-team01
  questions:
  - name: Q1
    items:
    - Confidence interval uses the normal quantile instead of qt() with n-1 df
    private_comment: "qnorm(0.975) on the CI line; everything else is correct."
  - name: Q2
    items:
    - Looks good
```

- `repos[].name` is a directory under `paths.repos`; `questions[].name` a
  template question name; `items` the descriptions of the selected rubric
  items, copied verbatim from the stored rubric.
- Each (repository, question) entry is declarative: listed items are selected,
  every other item of that question is deselected, and `items: []` records
  that none apply. `items` is required.
- `comment` is the public, student-facing comment and `private_comment` the
  grader's internal note. Omitting either leaves the stored value unchanged;
  an empty string clears it.
- A pair counts as graded once it has a selected item or a non-empty public
  comment. `items: []` with no public comment shows as ungraded in `mark()`
  until a human confirms it, which is why the confirmation item exists.
- Import with `marks_import(path, project, overwrite = FALSE)`. Every
  repository name, question name, and item description is validated before
  anything is written, and the write is one transaction. Pairs that already
  have any grading activity (a recorded selection event, or a non-empty
  public or private comment) are skipped and reported unless
  `overwrite = TRUE`. `marks_set()` writes a single pair from R, and
  `marks_export()` writes the recorded marks back out.

## Rules

- Student repositories are submissions. Never modify one except through the
  fix-parsing skill, which works on a `grading-fixes` branch and leaves the
  original branch untouched. Never push, stash, reset, or discard anything in
  them, and never commit to a student's original branch.
- The key is the instructor's document. Do not edit it without asking.
- Every change to grading state goes through the import functions or the
  apps, never through the SQLite file or hand-edited YAML that is not then
  imported.
- Names match verbatim on import: question names against the stored template,
  rubric item descriptions against the stored rubric, repository names against
  the directories under `paths.repos`. Fix the file being imported; never edit
  the rubric or template to make an import succeed.
- Private notes (`private_comment`) are the grader's internal record and are
  never shown to students. Public comments (`comment`) are student-facing and
  stay the human grader's voice; do not write them unless asked. Recorded text
  of either kind states the evidence in the student's own work, never mentions
  the key or the solution, never identifies another student or repository,
  and never mentions grading mechanics such as deductions.
- `<!-- markermd: ... -->` comments are repair markers left by the fix-parsing
  skill and are not the student's work. A repository whose checked-out branch
  is `grading-fixes` has been repaired; `git -C <repo> diff
  <original-branch>..grading-fixes` shows exactly what changed.
- A parse failure is not evidence that a document fails to render: markermd's
  parser is stricter than Quarto. Never fix a parse error by editing the
  template, and never loosen a validation rule just to make a repository pass.
- Student work is personal data. Keep it in this directory: do not copy it
  elsewhere, send it to other services, or share one student's work with
  another.
