---
name: markermd-apply-rubric
description: Apply a markermd project's stored rubric to the student repositories as a first-pass machine grading: reads each repository question by question, selects the rubric items that apply (every applicable item of a capped group, at most one tier of an exclusive group), records the reasoning as private notes, and imports the marks with marks_import() for human review in mark(). Use when asked to grade, mark, pre-grade, or apply the rubric to student repos in a markermd project.
---

# Apply a markermd rubric to the student repositories

## Overview

markermd grading is per (repository, question) pair: the grader toggles rubric
items on or off and the score follows from the question's scoring setup. This
skill performs that pass headlessly across the student repositories, recording
which rubric items apply to each pair plus a private note explaining why. The
result is a first draft: selections appear pre-toggled in `mark()`, where the
human grader reviews and corrects them. The marks travel as YAML
(`format_version: "1.0"`), with the full JSON Schema bundled at
`system.file("schema/markermd-marks.json", package = "markermd")`, and are
imported with `markermd::marks_import()`.

It works on an **initialized markermd project** (`markermd::init_project()`,
which sets up a `ghclass::org_grade_assignment()` layout) that already has
**both a stored grading template and a rubric with items**. The template
defines the questions and where each one lives in the assignment document; the
rubric defines the items that can be marked. Without them this skill cannot
proceed (use the `markermd-scaffold-template` and `markermd-scaffold-rubric`
skills first).

The flow is: read the config -> confirm the template and rubric and learn the
questions and items -> ask the user for scope and overwrite behavior -> run the
structural validation -> read each repository's answers question by question ->
decide which items apply -> write the marks YAML -> show the user the marks and
ask to import -> validate -> import -> report.

Hard rules:

- Question names and rubric item descriptions must be copied **verbatim** from
  the project (steps 2 and 3). `marks_import()` matches descriptions exactly
  and aborts the whole import on any mismatch. Never paraphrase a description,
  and never edit the rubric to match the marks file.
- Marks are declarative per pair: the listed items are selected and every
  other item is deselected. `items: []` states that no items apply.
- Write only `private_comment` notes. The public `comment` field is
  student-facing and stays the human grader's voice; never fill it.
- Recorded text never mentions the key (solution) directly or identifies
  another student or repository: notes state the evidence in the student's
  own work ("uses `qnorm()` instead of `qt()`"), not comparisons to the key.
- When unsure whether an item applies, do **not** select it; say what was
  unclear in the pair's private note instead. A wrong machine selection that a
  reviewer misses becomes a wrong grade.
- If any of the work is delegated to subagents (for example one per
  repository when the class is large), the user chooses the subagents' model
  first (step 4). Never launch a subagent on the session's current model by
  default: a cheaper model is often adequate for reading sections and
  applying concrete rubric items, and the choice is the user's.

This skill requires the `markermd` R package to be installed. If the commands
below fail with a package-not-found error, ask the user to install it first.

## Steps

### 1. Confirm the project is initialized and read its config

The user provides the project directory (the `init_project()` root). Read
`<root>/.markermd/config.yml`. If it does not exist, the directory is not a
markermd project: tell the user to run `markermd::init_project("<root>")` first,
then stop. For a human-readable overview you can also run:

```
Rscript -e 'markermd::project_sitrep("<root>")'
```

This skill needs `paths.repos` (the student repositories). If it is
missing/null, ask the user where the student repositories live relative to the
project root and record it:

```
Rscript -e 'markermd::project_set("<root>", repos = "<repos-dir>")'
```

### 2. Confirm a template is stored and learn its questions

Export the stored template and read it:

```
Rscript -e 'p <- tempfile(fileext = ".yaml"); markermd::template_export(p, project = "<root>"); cat(readLines(p), sep = "\n")'
```

If this errors with "No template is stored in this project's database", stop
and point the user at the `markermd-scaffold-template` skill (or the
`markermd::template()` app).

From the printed YAML record, per question:

- `name` -- copy verbatim; this is the marks file's question key.
- `node_ids` -- the heading/div anchors locating the question's section in the
  assignment documents (used to slice each repo in step 6).

Also note the top-level `source.path`: the assignment document's path, whose
basename is how each repository's document is matched.

### 3. Confirm the rubric and learn its items

Export the stored rubric and read it:

```
Rscript -e 'p <- tempfile(fileext = ".yaml"); markermd::rubric_export(p, project = "<root>"); cat(readLines(p), sep = "\n")'
```

From the printed YAML record, per question:

- the ordered item `description` values -- copy verbatim; these are the only
  legal entries for the marks file's `items` lists. A `group` entry nests
  some of the question's items under a label: its items are ordinary items
  for the marks file (listed by description like any other), but note the
  group's `floor` / `ceiling` (the bound on the group's combined points,
  applied by markermd when scoring) and whether it is `exclusive` (at most
  one of its items may be selected);
- each item's `points` and the question's `scoring.grading_mode`, which tell
  you what a selection means: in `negative` mode items describe defects to
  mark when present, in `positive` mode achievements to mark when earned.

If every question's `items` list is empty, stop and point the user at the
`markermd-scaffold-rubric` skill (or the Rubric pane of `mark()`); there is
nothing to apply yet.

Also identify each question's **confirmation item**: the item the grader
selects when the answer is fine (a 0-point "Looks good" style item in
`negative` mode, a full-credit item in `positive` mode). A pair only counts
as graded in `mark()` once at least one item is selected or a public comment
is written, so this item is how a correct answer gets recorded as graded.
Note any questions that lack one; step 4 offers to add them.

### 4. Ask the user: scope and overwrite behavior

Ask both questions in one round before reading any
repositories, plus a third when subagents will be used:

- **Scope** -- mark all student repositories, or a named subset.
- **Existing grading** (recommend skip): `marks_import()` by default skips
  any (repository, question) pair that already has any grading activity and
  reports it, so a previous human or machine pass is never clobbered. Activity
  here is broad: any recorded grade row (even one where every item is
  deselected), or any non-empty public **or private** comment. Because this
  skill records a private note for every pair it processes, re-running it skips
  every pair from its own previous pass unless you pass `overwrite = TRUE`.
  Offer `overwrite = TRUE` only if the user explicitly wants those pairs
  re-marked, and say that it replaces their selections.
- **Subagent model** (only when you intend to delegate per-repository
  reading or marking to subagents, which is worth doing once there are more
  than a handful of repositories): which model the subagents should run on.
  Offer the tiers from cheapest up, with the cheapest tier you judge adequate
  marked as recommended, and state what each is good for: a small, cheap
  model tier for reading sections and applying concrete, literal rubric
  items; a mid-tier model for judging correctness of code and interpretation
  against the items, adequate for most assignments; the session's current
  model only when the assignment demands it (subtle reasoning, long or
  unusual submissions). Never default to the current model. Pass the chosen
  model explicitly to every subagent launch (your agent's per-subagent model
  setting), keep the final review of the subagents' proposed selections and
  private notes in this session, and record the choice in the report
  (step 11).

If step 3 found questions without a confirmation item, also ask whether to
add one to each (recommended): a 0-point "Looks good" item in deduction mode,
or a full-credit "Solution is correct and complete" item in additive mode.
Phrase it as positive student-facing feedback and never mention grading
mechanics such as deductions in the description. Without it a correct answer
can only be recorded as `items: []`, which leaves the pair looking ungraded
in `mark()` until the human confirms it. To add them, write a small rubric
YAML covering just those questions and append it:

```yaml
format_version: "1.0"
questions:
- name: Q2
  items:
  - points: 0
    description: Looks good
```

```
Rscript -e 'markermd::rubric_import("<file>.yaml", project = "<root>", mode = "append")'
```

Then re-run step 3's export so the item list you mark against is current.

### 5. Run the structural validation

Get the cheap structural signal first -- the stored template's validation
results across all repos:

```
Rscript -e 'res <- markermd::validate_project("<root>"); print(res, row.names = FALSE)'
```

Repos with `status` of `error` have a missing or unparseable document. For
the unparseable ones, suggest the `markermd-fix-parsing` skill first: it
repairs the document on a `grading-fixes` branch in the repository so it can
be read here. Those still failing, and the missing ones, cannot be read and
are excluded; list them in the final report (step 11) so the user grades them
by hand. Per-question `fail` results indicate missing sections or
rule violations -- useful pointers for which rubric items are likely to apply,
but read the actual answer before selecting anything.

### 6. Read each repository, question by question

For every in-scope repository under `<root>/<paths.repos>`:

- Match the assignment document by the basename of `source.path` from step 2,
  falling back to the first `.qmd` file when absent.
- A repo whose checked-out branch is `grading-fixes` (`git -C <repo> branch
  --show-current`) carries repairs made by the `markermd-fix-parsing` skill,
  and the document you read includes them. `<!-- markermd: ... -->` comments
  are that skill's markers for substantive repairs, never student content:
  skip them when reading, and for any question whose section carries one,
  run `git -C <repo> diff <original-branch>..grading-fixes -- <file>` to see
  what was removed or rewritten before judging the answer (the untouched
  submission is on the original branch, usually `main`). Grade the student's
  original meaning. A repair is never by itself a reason to select an item
  (markermd's parser is stricter than Quarto 1.x, so a needed repair is not
  evidence of a rendering failure), and the pair's private note should say
  when a repair touched the question so the reviewer knows to look at the
  diff.
- Slice each question's section from its anchor heading (the template's
  `node_ids`) to the next same-or-higher-level heading -- the same technique
  the scaffold skills use. Students sometimes edit headings so an anchor is
  absent; fall back to matching heading text, and treat a question whose
  section is missing entirely as exactly that (often a "not attempted" rubric
  item).
- Read the question's full answer: prose, code, and (when the repo has
  rendered output) results. Judge what the student actually did, not how it
  is formatted.

### 7. Decide which items apply to each (repository, question) pair

Work pair by pair against step 3's item lists:

- **Deduction mode** (`negative`): select the items whose defect is actually
  present in the answer. When none of the defect items apply, select the
  question's confirmation item ("Looks good") so the pair is recorded as
  graded; fall back to `items: []` only when the question has no
  confirmation item.
- **Additive mode** (`positive`): select the items whose achievement the
  answer earns; a fully correct answer gets the full-credit confirmation item
  alone. An answer that earns nothing gets `items: []` and a private note
  saying why.
- Select an item only when the evidence is in the student's work. Do not
  select for things that are merely possible, and never select an item whose
  description does not match what you observed -- not even as "the closest
  one". If no item fits an observed problem, leave it unselected and note the
  problem in the private note (the human can add a rubric item later).
- In a capped group (one with a `floor` or `ceiling`), select every item
  that applies and never pre-apply the cap by leaving some out: the bound
  does the arithmetic, and the selections are the record of what was wrong.
- In an exclusive group select at most one item, the tier the evidence
  supports; when the answer falls between two tiers, take the milder one
  and say so in the private note. `marks_import()` aborts the whole import
  when any pair selects two items of an exclusive group, so check every
  pair for this before writing the file.
- For every pair, draft a one-to-three sentence `private_comment` recording
  the evidence: what you saw in the answer that justifies the selections (or
  the clean pass), citing the relevant function/value/line. State the
  evidence on its own terms and never mention the key directly. These notes
  are internal -- never shown to students -- but write them as if the grader
  will rely on them, because they will.

### 8. Write the marks YAML

Write the file to `<root>/markermd-marks.yaml` (ask if the user wants a
different location). Shape:

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
    private_comment: "Correct approach and result throughout."
- name: hw01-team02
  questions:
  - name: Q1
    items:
    - Question not attempted
    private_comment: "The Q1 section heading is present but the body is empty."
```

Rules and constraints:

- `name` values (repositories and questions) and every `items` entry must
  match the project verbatim (steps 1-3).
- `items` is required for every question entry; `items: []` means no items
  apply. Prefer the confirmation item over `items: []` for correct answers:
  a pair with no selected items and no public comment shows as **ungraded**
  in `mark()`, so an `items: []` pair sits waiting for the human's explicit
  confirmation.
- Only `private_comment`, never `comment` (hard rule above).
- Skip repos excluded in step 5; do not invent entries for documents you
  could not read.

### 9. Show the user the marks and confirm before importing

The user must see the marks themselves before being asked to import them.
Show the marks YAML written in step 8 (print the file's contents; for a large
class show it repo by repo rather than eliding entries), followed by a
per-repository summary: for each question, the selected item descriptions (or
"none apply") and the implied score arithmetic from the scoring mode (e.g.
"Q1: -5, -1 -> 4/10", or with a capped group "Q1: -5, Examples -1 -1 -1 ->
-2 at the floor -> 3/10"): a capped group's selected items count together,
clamped to the group's floor or ceiling before the question's own bounds,
and an exclusive group contributes its one selected tier. Flag any pairs
where you were uncertain. Then ask
explicitly whether to import these marks. Never run `marks_import()` without
an explicit yes; if the user adjusts anything, update the file and show the
affected entries again before asking.

### 10. Validate the marks file and import it

Confirm the file parses:

```
Rscript -e 'invisible(markermd::read_marks_yaml("<root>/markermd-marks.yaml")); cat("OK\n")'
```

If `jsonvalidate` is installed, also check it against the schema:

```
Rscript -e 'print(markermd::validate_marks_file("<root>/markermd-marks.yaml"))'
```

`TRUE` means it conforms. Then, with the user's confirmation from step 9,
import (set `overwrite` per step 4):

```
Rscript -e 'markermd::marks_import("markermd-marks.yaml", project = "<root>", overwrite = FALSE)'
```

The import validates every repository name, question name, and item
description before writing anything; on a description mismatch, fix the marks
YAML against step 3's verbatim list and retry -- never edit the rubric to
match the marks file. Relay the skipped-pairs report to the user: those pairs
already had grading activity and were left untouched.

### 11. Report

Summarize for the user: how many pairs were marked and how many item
selections recorded (the import prints both), which pairs were skipped for
existing grading activity, which repos were excluded as unreadable in
step 5 and need manual grading, and which repos were read from a
`grading-fixes` branch (naming the questions whose section carried a
markermd repair marker). If subagents were used, say which model they ran on
and that their selections were reviewed here before import. Then direct them to review in:

```
markermd::mark("<root>")
```

Point out that the marked selections appear pre-toggled in the Rubric pane
(a group's header shows its subtotal and flags a bound that was hit), the
reasoning is in the private-notes box under the public comment (private
notes are never shared with students), correct answers were registered
through each question's confirmation item so they count as graded, and any
`items: []` fallback pairs still show as ungraded until they confirm them.
These are machine suggestions: the human review is the grading.
