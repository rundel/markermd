---
name: markermd-scaffold-rubric
description: Scaffold a markermd grading rubric (per-question rubric items plus scoring setup, as YAML) for an initialized markermd project that already has a stored grading template. Asks whether to work through the questions all at once or one at a time (showing and importing each question's proposed items before moving to the next), and whether to base the rubric on the key (solution) repository alone or on the key plus the student repositories (sampling submissions to anticipate common mistakes); asks for the assignment's total points and how to distribute them across questions (proposing equal, content-weighted, and assignment-stated splits); drafts concrete key-derived rubric items for each template question, writes the rubric YAML, validates it against the bundled JSON Schema, and imports it into the project's grading database with rubric_import(). Use when a user wants to bootstrap, scaffold, or generate a markermd rubric or grading rubric items for an assignment project created with markermd::init_project() / ghclass::org_grade_assignment().
---

# Scaffold a markermd grading rubric

## Overview

markermd grades each question against a rubric: a flat list of rubric items,
where each item is a point value plus a feedback description that the grader
toggles on or off per student, and a scoring setup (`total_score`, a
`grading_mode` of `positive` or `negative`, and score bounds). Item order is
the display order, and the first ten items per question are bound to the
keyboard hotkeys 1-9 and 0, so the most-used items belong first. The canonical
store for the rubric is the project's grading database
(`.markermd/markermd.sqlite`); YAML (`format_version: "1.0"`) is the
human-authorable import/export format, with the full JSON Schema bundled at
`system.file("schema/markermd-rubric.json", package = "markermd")`. This skill
builds that scaffolding so the user does not have to author items one at a
time in the app.

It works on an **initialized markermd project** (`markermd::init_project()`,
which sets up a `ghclass::org_grade_assignment()` layout) that **already has a
grading template stored** in its database. The template defines the questions;
`markermd::rubric_import()` validates the rubric's question names against it
before writing anything, so without a stored template this skill cannot
proceed (use the `markermd-scaffold-template` skill first).

The flow is: read the config -> confirm the template and learn its questions ->
ask the user for the workflow (all questions at once, or one question at a
time), the rubric basis, and the grading mode -> read the key's solution per
question -> ask the user for the assignment's total points and confirm a
per-question distribution -> if requested, sample the student repos for
common mistakes -> draft, confirm, and import. With the all-at-once workflow
that last part is one pass: draft every question -> show the user the full
draft and confirm -> write the rubric YAML -> validate -> show the final YAML
and ask to import -> import into the project database (step 9). With the
question-at-a-time workflow it repeats per question: draft -> show that
question's proposed items -> confirm -> import, before moving to the next
question (step 10). Either way, report at the end. The scaffold is a starting
point; the user refines items while grading in the Rubric pane of `mark()`.

Two hard rules:

- Question `name` values must be copied **verbatim** from the stored template
  (step 2). Never invent, rename, or "fix" a question name; `rubric_import()`
  rejects names that do not match the template exactly.
- Item descriptions are reusable feedback text shown while grading every
  student. Never quote or identify a specific student or repository in them,
  and never mention the key (solution) directly -- state the expected result
  or the defect on its own terms ("uses `qnorm()` instead of `qt()`"), not as
  a comparison to the key.

This skill requires the `markermd` R package to be installed. If the commands
below fail with a package-not-found error, ask the user to install it first.

## Steps

### 1. Confirm the project is initialized and read its config

The user provides the project directory (the `init_project()` root). Read
`<root>/.markermd/config.yml`. If it does not exist, the directory is not a
markermd project: tell the user to run `markermd::init_project("<root>")` first
(it also needs to be a `ghclass::org_grade_assignment()`-style layout), then
stop. For a human-readable overview you can also run:

```
Rscript -e 'markermd::project_sitrep("<root>")'
```

Note the `paths.key` and `paths.repos` values; you will use or fill in each
below. The rubric itself lives in the grading database, not in the config.

### 2. Confirm a template is stored and learn its questions

The rubric hangs off the template's questions, so export the stored template
and read it:

```
Rscript -e 'p <- tempfile(fileext = ".yaml"); markermd::template_export(p, project = "<root>"); cat(readLines(p), sep = "\n")'
```

If this errors with "No template is stored in this project's database", stop
and tell the user to scaffold one first (the `markermd-scaffold-template`
skill, the `markermd::template()` app, or `markermd::template_import()`).

From the printed YAML record, per question:

- `name` -- copy verbatim; this is the rubric's question key.
- `node_ids` -- the heading/div anchors locating the question's section in the
  assignment documents (used to slice the key in step 4).

Templates exported by older markermd versions may also carry a per-question
`points` field; ignore it -- question points live in the rubric's scoring
setup and are decided in step 5.

Also note the top-level `source.path`: the key's assignment document relative
to the project root.

### 3. Ask the user: workflow, rubric basis, and grading mode

Ask all three questions in one round (AskUserQuestion) before reading anything
else:

- **Workflow** -- how to work through the template's questions:
  - *All at once*: draft every question, show the user one full draft to
    confirm, and import the whole rubric in a single pass (step 9).
  - *One question at a time*: for each question in turn, show its proposed
    items, confirm or adjust them, and import that question before moving on
    to the next (step 10). More rounds, but the user shapes each question
    while it is in front of them and can stop partway with everything
    confirmed so far already imported.
- **Basis** -- what the rubric should be built from:
  - *Key only*: items are derived from the key's solution (what a correct
    answer must contain).
  - *Key + student repos*: additionally read or sample the student
    submissions to anticipate the mistakes students actually made and draft
    deduction items for them.
- **Grading mode** (recommend deductions):
  - *Deductions* (`negative`, recommended): items carry negative points and
    the score starts at `total_score`; graders mark what is wrong.
  - *Additive* (`positive`): items carry positive points and the score builds
    up from 0; graders mark what is present.

The assignment's total points and their distribution across questions are
asked separately (step 5), once the key has been read.

If the user chose key + student repos and `paths.repos` is missing/null in the
config, ask where the student repositories live (relative to the project root)
and record it:

```
Rscript -e 'markermd::project_set("<root>", repos = "<repos-dir>")'
```

### 4. Read the key's solution, question by question

The key is the ground truth for what a correct answer contains.

- If `paths.key` is set, the key directory is `<root>/<paths.key>`.
- If `paths.key` is missing/null, **ask the user** where the key (solution)
  repository is, relative to the project root, then record it:

  ```
  Rscript -e 'markermd::project_set("<root>", key = "<key-dir>")'
  ```

To map each question's `node_ids` to its place in the document, get the key's
outline (replace KEYDIR with `<root>/<paths.key>`):

```
Rscript -e 'o <- markermd::assignment_outline("KEYDIR"); cat(jsonlite::toJSON(list(source_file = attr(o, "source_file"), sections = o), dataframe = "rows", auto_unbox = TRUE, pretty = TRUE))'
```

Then read the key's assignment document (`<root>/<source.path>` from step 2)
directly, slicing each question's section from its anchor's heading to the
next same-or-higher-level heading. For each question capture three things:

- the **question prose** -- what the assignment asks for;
- the **solution code** -- the specific functions, arguments, and approach the
  key uses;
- the **written answer** -- what a correct interpretation or write-up says.

Also read `<root>/<paths.key>/README.md` when present; most assignments state
per-question expectations there, and often point values. Note any stated
point values -- per question or an assignment total -- verbatim: they seed
the distribution proposals in step 5.

### 5. Ask the user: assignment total and per-question point distribution

Question points live in the rubric (each question's `scoring.total_score`),
not the template, and they are decided here: once, for the whole assignment,
before any drafting -- in both workflows. The question-at-a-time loop must
not revisit them per question.

First build concrete proposals from what step 4 surfaced. Always include the
per-question numbers; never offer an abstract "weighted" option without them.

- *Stated values* (when found): if the key's README or the assignment prose
  states per-question point values or an assignment total, turn those exact
  numbers into a proposal -- the leading option when present.
- *Equal split*: the same value for every question from a round total
  (e.g. 5 questions x 10 pts = 50).
- *Weighted by content*: weight each question by what its key section
  actually demands -- multiple parts, code plus interpretation, or a plot
  plus a write-up earn more than a one-line answer -- normalized to a round
  total.

Then ask, in one AskUserQuestion round:

1. **Total points for the assignment** -- offer the totals the proposals
   imply (the stated total when there is one, questions x 10, 100, ...).
2. **Distribution across questions** -- one option per proposal, each with
   its per-question numbers in the description (e.g. "Weighted: Q1 5, Q2 10,
   Q3 15"). With many questions, show the full per-question table in the
   message before asking and name the schemes in the options.

If the chosen total and distribution disagree (e.g. an equal split of 100
over 7 questions), rescale the distribution proportionally and round to
sensible values (halves are fine), then show the final per-question table
and confirm it. The confirmed totals become each question's
`scoring.total_score` in everything that follows.

### 6. (key + repos basis only) Sample the student repositories

Skip this step entirely for the key-only basis.

Start with the cheap structural signal -- the stored template's validation
results across all repos:

```
Rscript -e 'res <- markermd::validate_project("<root>"); print(res, row.names = FALSE)'
```

Questions that `fail` widely are where deduction items will earn their keep;
repos with `status` of `error` (missing or unparseable documents) are excluded
from sampling.

Then read actual submissions under `<root>/<paths.repos>`:

- When there are roughly 8 repos or fewer, read the assignment document in all
  of them.
- With more, sample about 5-8: prioritize repos whose validation failures fall
  on *different* questions, plus one or two fully passing repos as the
  "typical correct" baseline.
- Match each repo's document by the same filename as the key's `source.path`.
  Students sometimes edit headings so the template anchors are absent; fall
  back to matching heading text rather than skipping the repo.

For each question, look for:

- recurring wrong approaches (wrong function, wrong formula, wrong
  interpretation of the question);
- missing pieces (no write-up, missing labels, missing seed, unrendered
  output);
- structural failures already surfaced by the validation results.

A mistake observed in two or more sampled repos is a candidate deduction item.
Phrase it generically -- never quote or identify a specific student or
repository.

### 7. Draft items and scoring for each question

These drafting rules apply in both workflows: all at once, draft every
question before step 9; one question at a time, draft each question as you
reach it in step 10's loop.

Scoring setup, per question:

- `total_score`: the question's points from the distribution confirmed in
  step 5.
- `grading_mode`: the user's choice from step 3.
- `bound_above_zero: true` and `bound_below_max: true` (clamped scores; the
  user can loosen these in `mark()`).

The central rule for items is **concreteness**: every description names the
specific check, derived from the key's actual code and answer. "Standard error
computed as `sd(x)/n` instead of `sd(x)/sqrt(n)`" or "Density plot does not
map `fill` to species" -- never "calculation error" or "incorrect plot".
Generic placeholder items defeat the point of the scaffold.

The second rule is a **confirmation item for every question**: a (question,
repository) pair only counts as graded in `mark()` once at least one item is
selected or a public comment is written, so a flawless answer with nothing to
deduct would otherwise still look ungraded. Give every question a first item
the grader selects when the solution is fine: in deduction mode a 0-point
item ("Looks good"); in additive mode a full-credit item at `total_score`
("Solution is correct and complete"). Phrase it as positive student-facing
feedback ("Looks good", "Solution ok") and never mention grading mechanics
such as deductions in the description. It is the most-used item in a typical
class, which is also why it belongs first, on hotkey 1.

Guidelines:

- **Order matters.** Items are displayed in file order and the first ten get
  hotkeys 1-9 and 0, so put the most-expected items first: the confirmation
  item, then core correctness checks (from the key) before secondary ones
  (labels, write-up quality).
- **Deduction mode** (`negative`): describe the defect; points are negative
  and scaled to severity relative to `total_score`. For a 10-point question,
  roughly: a catch-all "Question not attempted" at `-total_score`; a
  fundamentally wrong approach -5 to -8; a major missing element (no write-up,
  no plot) -2 to -4; a minor issue (missing axis labels, stray debug output)
  -0.5 to -1.
- **Additive mode** (`positive`): describe the achievement; after the
  full-credit confirmation item, the partial-credit items' points are
  positive and should sum to approximately `total_score`. The grader selects
  the confirmation item alone for a fully correct answer, or the partial
  items otherwise; `bound_below_max` keeps combinations clamped.
- Keep each item to **one issue**; split compound problems into separate
  items.
- Do not put point values inside the description text (the app displays the
  points alongside).
- About **3-7 items per question** (including the confirmation item) is right
  for a scaffold; never more than 10 (the hotkey limit).
- With the key + repos basis, add items only for mistakes actually observed in
  step 6 beyond the key-derived core checks -- do not pad with hypotheticals.

### 8. The rubric YAML: shape, validation, and import mode

Both workflows write the rubric to `<root>/markermd-rubric.yaml` (ask if the
user wants a different location) and import from that file; the
question-at-a-time workflow grows it one question per round. Shape:

```yaml
format_version: "1.0"
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
  - points: -1
    description: Plot is missing axis labels
```

Rules and constraints:

- `name` values must match the stored template's question names exactly
  (step 2).
- There are **no item ids and no hotkeys** in the YAML: item order is the
  binding (positions 1-10 become the hotkeys on import).
- `scoring` is optional per question; when omitted the project's existing
  scoring settings are left unchanged. In this skill, always emit `scoring`
  for every question, using the step 5 totals.
- The full JSON Schema is bundled at
  `system.file("schema/markermd-rubric.json", package = "markermd")` if you
  need to check the exact structure.

To validate the file (re-run after every edit), confirm it parses:

```
Rscript -e 'invisible(markermd::read_rubric_yaml("<root>/markermd-rubric.yaml")); cat("OK\n")'
```

If `jsonvalidate` is installed, also check it against the schema:

```
Rscript -e 'print(markermd::validate_rubric_file("<root>/markermd-rubric.yaml"))'
```

`TRUE` means it conforms. Fix any reported problems (commonly a mistyped
question name, a missing `items` key, or a string where a number belongs) and
re-validate until it loads cleanly.

Finally, before anything is imported, check whether the project already has
rubric items by exporting the current rubric (this succeeds, with empty
`items` lists, whenever a template is stored -- an error is not the signal
here):

```
Rscript -e 'p <- tempfile(fileext = ".yaml"); markermd::rubric_export(p, project = "<root>"); cat(readLines(p), sep = "\n")'
```

If any question already has items, **ask the user** which import mode to use:

- `append` (the default): the file's items are added after each question's
  existing items.
- `replace`: each affected question's existing items are deleted first --
  along with **all recorded selections of those items, for every repository**.
  This cannot be undone, so say so explicitly when offering it.

Ask this once: in the question-at-a-time workflow the chosen mode applies to
every per-question import, and `replace` only touches the questions named in
each individual import call.

### 9. All at once: confirm the full draft, then write and import

Skip this step if the user chose one question at a time (step 10).

Show the complete draft: for each question, its name, total (from the step 5
distribution), and mode, and every item in order with its points and full
description. Never summarize or elide items; the user must see exactly what
would be imported. Ask the user to confirm or adjust (item wording, points
magnitudes, items to add, drop, or reorder). Do not write any files until the
user confirms.

Then write the full rubric YAML and validate it (step 8). Importing is a
separate decision from drafting: show the user the final rubric YAML exactly
as written (print the file's contents) and ask whether to import it, even
though they confirmed the draft above -- fixes made during validation may
have changed it. Never run `rubric_import()` without an explicit yes to this
question. Then import, with the mode chosen in step 8:

```
Rscript -e 'markermd::rubric_import("markermd-rubric.yaml", project = "<root>", mode = "append")'
```

If the import aborts because a question name is not in the project's template,
correct the YAML to the exact names from step 2 and retry. Never rename the
template's questions to match the rubric file.

### 10. One question at a time: confirm and import each question

Skip this step if the user chose all at once (step 9).

Work through the template's questions in order. For each question:

1. Draft its items (step 7) from the key reading in step 4 and, with the
   key + repos basis, the sampling findings from step 6 (the sampling itself
   is done once up front, not redone per question).
2. Show the question's complete proposed entry: its name, total (from the
   step 5 distribution), and mode, and every item in order with its points
   and full description. Never summarize or elide items.
3. Ask the user to confirm or adjust (item wording, points magnitudes, items
   to add, drop, or reorder), iterating until they confirm. This confirmation
   is the explicit yes that authorizes importing this question; never import
   without it.
4. Add the confirmed question to `<root>/markermd-rubric.yaml` -- on the
   first question, create the file with its `format_version` header -- and
   validate the file (step 8). If fixing a validation problem changed the
   question's entry, show it again before importing.
5. Import just this question, leaving every other question in the file
   untouched, with the mode chosen in step 8:

   ```
   Rscript -e 'markermd::rubric_import("markermd-rubric.yaml", project = "<root>", mode = "append", question = "<name>")'
   ```

   If the import aborts because the question name is not in the project's
   template, correct the YAML to the exact name from step 2 and retry; never
   rename the template's questions.
6. Relay the import's confirmation message, then move on to the next
   question.

The user can stop after any question: confirmed questions are already in the
database, and the YAML file keeps everything drafted so far. If they stop
early, list the questions that remain; a later run of this skill can pick up
there (the step 8 existing-items check shows which questions already have
items).

### 11. Report

Summarize for the user: the basis (key only vs key + repos) and grading mode
chosen, the assignment's total points, each imported question's total and
item count, and that the rubric was written to `<root>/markermd-rubric.yaml`
and imported into the project database (in the question-at-a-time workflow,
also list any questions left for later). Then tell them to start grading,
where the rubric is refined in place:

```
markermd::mark("<root>")
```

Point out that the Rubric pane shows the items with their hotkeys (the first
ten per question, in order), that items can be edited, reordered, and added
while grading, and that the pane's menu offers the same YAML import/export for
later round trips. Remind them the scaffold is a starting point: real
submissions will suggest items no one anticipated.
