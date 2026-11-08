---
name: markermd-scaffold-rubric
description: Scaffold a markermd grading rubric (per-question items and scoring) for a project with a stored template: derives concrete items from the key, optionally samples student repositories for common mistakes, places items that stack in capped item groups and severity tiers in exclusive groups, confirms points and grading mode with the user, and imports the rubric with rubric_import(). Use when asked to create, draft, or scaffold a markermd rubric or grading rubric items.
---

# Scaffold a markermd grading rubric

## Overview

markermd grades each question against a rubric: a list of rubric items,
where each item is a point value plus a feedback description that the grader
toggles on or off per student, and a scoring setup (`total_score`, a
`grading_mode` of `positive` or `negative`, and score bounds). Items that
stack (several example checks, several documentation checks, the tiers of a
write-up) can be placed in a named **item group** whose combined points are
bounded by a `floor` and/or `ceiling`, or which is `exclusive` (at most one
selected item), so the grader records every defect and the score still
respects the question's calibration. Item order is the display order; the
first ten items per question, counting a group's items in place, are bound
to the keyboard hotkeys 1-9 and 0 and any further items are toggled by
clicking (there is no limit on the number of items), so the most-used items
belong first. The canonical store for the rubric is the project's grading
database (`.markermd/markermd.sqlite`); YAML (`format_version: "1.1"`, or
`"1.0"` for a file without groups) is the human-authorable import/export
format, with the full JSON Schema bundled at
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

Never create a file or directory at the top level of the project. Every
working file this skill writes (the import/export YAML, evidence or review
JSON, helper scripts, `.rds` diffs, draft reports, images) goes under
`<root>/scratch/`, in a subdirectory named for the task when there are more
than one or two (for example `<root>/scratch/apply-rubric/`); create it if it
is missing. Notes meant to be kept go under `<root>/notes/`. Files holding
student work stay inside the project, never in a system temporary directory.
When delegating to subagents, give each the exact `scratch/` subdirectory to
write to.

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
  assignment documents (used to slice the key in step 4), or `file` -- for a
  question on one of the assignment's other files (listed under the
  template's `files:`), the file it covers as a whole.

Templates exported by older markermd versions may also carry a per-question
`points` field; ignore it -- question points live in the rubric's scoring
setup and are decided in step 5.

Also note the top-level `source.path`: the key's assignment document relative
to the project root.

### 3. Ask the user: workflow, rubric basis, and grading mode

Ask all three questions in one round before reading anything
else, plus a fourth when subagents will be used:

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

- **Subagent model** (only when you intend to delegate the reading of
  student repositories in step 6 to subagents, which is worth doing for a
  large class with the key + student repos basis): which model the subagents
  should run on. Offer the tiers from cheapest up, with the cheapest tier you
  judge adequate marked as recommended, and state what each is good for: a
  small, cheap model tier for reading each submission's sections
  and reporting what it did per question; a mid-tier model
  for spotting wrong approaches and interpretations, adequate for most
  assignments; the session's current model only when the assignment demands
  it. Never default to the current model. Pass the chosen model explicitly
  to every subagent launch (your agent's per-subagent model setting), keep the
  drafting of items in this session, and record the choice in the report
  (step 11).

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
next same-or-higher-level heading; for a question with `file:`, read that
file in the key (`<root>/<paths.key>/<file>`) whole, along with whatever the
README or the document says it should do. For each question capture three
things:

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

Then ask, in one round:

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

Questions that `fail` widely are where deduction items will earn their keep.
Repos with `status` of `error` have a missing or unparseable document: for
the unparseable ones, suggest the `markermd-fix-parsing` skill (which repairs
the document on a `grading-fixes` branch in the repository) before sampling;
any still failing are excluded from sampling.

Then read actual submissions under `<root>/<paths.repos>`:

- When there are roughly 8 repos or fewer, read the assignment document in all
  of them.
- With more, sample about 5-8: prioritize repos whose validation failures fall
  on *different* questions, plus one or two fully passing repos as the
  "typical correct" baseline.
- Match each repo's document by the same filename as the key's `source.path`.
  Students sometimes edit headings so the template anchors are absent; fall
  back to matching heading text rather than skipping the repo.
- A repo whose checked-out branch is `grading-fixes` (`git -C <repo> branch
  --show-current`) carries repairs made by the `markermd-fix-parsing` skill
  so the document parses. Read it as-is, but `<!-- markermd: ... -->`
  comments are that skill's markers, never student content, and
  `git -C <repo> diff <original-branch>..grading-fixes` shows exactly what
  was changed (the untouched submission is on the original branch, usually
  `main`). A repair is never itself a candidate mistake: markermd's parser is
  stricter than Quarto 1.x, so a needed repair is not evidence that the
  student's document fails to render. Judge rendering from the rendered
  artifact instead.

For each question, look for:

- recurring wrong approaches (wrong function, wrong formula, wrong
  interpretation of the question);
- missing pieces (no write-up, missing labels, missing seed, unrendered
  output);
- structural failures already surfaced by the validation results.

A mistake observed in two or more sampled repos is a candidate deduction item.
Phrase it generically -- never quote or identify a specific student or
repository.

Also record how the observed mistakes relate to each other, because that
decides the grouping in step 7: several small omissions of one kind in the
same answer (missing examples, missing pieces of documentation, leftover
debug output) are a category that stacks and belongs in a capped group, and
one shortfall seen at different depths across repos (a write-up absent in
some, cursory in others) is a pair of severity tiers for an exclusive group.

Sort every observed problem by where its cause lies. Most belong to one
question. A problem whose cause lies outside every question -- a render
setting in the YAML header (such as `engine: jupyter`, which stops the R
chunks from running), global chunk options, a document that does not render
at all -- is document-level: note it once per affected repo and do not
count its consequences (missing output, a table that never builds) as
mistakes in the questions it touches. A defect inside one question's own
chunks (conflict markers, a misplaced fence) stays with that question, even
when it also breaks rendering, and a parse repair is never document-level.

If subagents do this reading, they run on the model the user chose in step 3
and report per-question observations back, plus any document-level problems
separately; the drafting of items from those observations stays in this
session.

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

- **Order matters.** Items are displayed in file order; the first ten get
  hotkeys 1-9 and 0 and any further items are toggled by clicking, so put the
  most-expected items first: the confirmation item, then core correctness
  checks (from the key) before secondary ones (labels, write-up quality).
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
  for a scaffold. There is no hard limit: items past the tenth are toggled by
  clicking rather than by hotkey, so keep the ten most-used items first and go
  beyond ten only when the observed mistakes warrant it (a tail of rare
  deductions after the core items is fine).
- With the key + repos basis, add items only for mistakes actually observed in
  step 6 beyond the key-derived core checks -- do not pad with hypotheticals.

Item groups, for items that stack. Several items often describe the same
category of shortfall (the examples a solution should demonstrate, the
pieces of its documentation, style and leftover debug output, the quality
of a write-up). Selected together they can swamp the question, and a
"never select both" rule between two severities of the same problem is
only enforced by grader discipline. Put such items in a group:

- A **capped group** carries a `floor` (deduction mode: the most the group
  can take off together) or a `ceiling` (additive mode: the most it can
  earn together). Its items keep their standalone points; the bound, not
  the item points, does the proportioning. Choose the bound from the
  question's weight: roughly 15-20 percent of `total_score` for the
  examples category, less for style and debug output, and about the weight
  of the element for documentation or a write-up. Then size it so it can
  bind: its magnitude at least the largest single item in the group (so one
  defect on its own is never trimmed) and less than the sum of the group's
  items (otherwise the cap never applies and the group is pointless). The
  grader still selects every defect that applies, and the record of what
  was wrong stays complete.
- An **exclusive group** (`exclusive: true`) holds severity tiers of one
  problem ("write-up absent" versus "write-up partial", "comparison missing"
  versus "comparison superficial"): at most one of its items can be
  selected, so it needs no bound. Use it instead of two separate items plus
  an informal rule. Give each tier its own points, and put the tier graders
  will pick most often first.
- Keep the confirmation item, the "not attempted" catch-all and the core
  correctness checks ungrouped and ahead of any group: each is a distinct
  defect whose full points should apply (a cap would soften it), and the
  first hotkeys belong to the most-used items. Group only items of one
  category that stack, or the tiers of one problem. A group is only worth
  creating for two or more items; one level of grouping only, since
  groups cannot nest.
- A group's `group` label is shown to the grader and, only when feedback
  is exported with group headings, to students, so phrase it as a neutral
  category name ("Examples", "Documentation", "Write-up"), never as a
  grading mechanic.

Document-level problems (key + repos basis only, and only when step 6 found
at least one): propose adding a document-scope question to hold them, and ask
the user for each item's deduction. Never spread one such problem across the
questions it affects. When the user agrees:

- Add the question to the template: export it, append
  `{id: <next id>, name: Document, scope: document}` to `questions`, and
  import it again (this touches no rubric items or marks):

  ```
  Rscript -e 'markermd::template_export("<root>/scratch/markermd-template.yaml", "<root>")'
  # append the Document question to scratch/markermd-template.yaml, then
  Rscript -e 'markermd::template_import("<root>/scratch/markermd-template.yaml", "<root>")'
  ```

- Give it scoring `total_score: 0`, `grading_mode: negative`,
  `bound_above_zero: false`, `bound_below_max: true`, `optional: true`, so
  it is outside the step 5 distribution, only ever deducts from the
  assignment total, and needs no selection in repos it does not apply to.
- Its items are the observed document-level problems, phrased as feedback
  (for example "`engine: jupyter` in the YAML header means the R chunks never
  run when the document renders, so none of the R output appears"). No
  confirmation item is needed while the question is optional.

Without such problems, add no document question.

### 8. The rubric YAML: shape, validation, and import mode

Both workflows write the rubric to `<root>/scratch/markermd-rubric.yaml` (ask if the
user wants a different location) and import from that file; the
question-at-a-time workflow grows it one question per round. Shape:

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
      description: No example where the interval misses the true mean
    - points: -1
      description: Examples do not vary the confidence level
  - group: Write-up
    exclusive: true
    items:
    - points: -3
      description: The interpretation of the interval is missing
    - points: -1.5
      description: The interpretation of the interval is incomplete
  - points: -1
    description: Plot is missing axis labels
```

Rules and constraints:

- `name` values must match the stored template's question names exactly
  (step 2).
- There are **no item ids and no hotkeys** in the YAML: item order is the
  binding (positions 1-10, counting a group's items in place, become the
  hotkeys on import; later positions have none).
- A group entry has a `group` label (unique within the question), its
  `items`, and optionally `floor` (a number `<= 0`), `ceiling` (a number
  `>= 0`) and `exclusive` (a boolean). Groups cannot nest, and item
  descriptions stay unique across the whole question, groups included.
  Write `format_version: "1.1"` whenever the file has a group; `"1.0"` is
  the version for a file without groups (both are read).
- `scoring` is optional per question; when omitted the project's existing
  scoring settings are left unchanged. In this skill, always emit `scoring`
  for every question, using the step 5 totals.
- The full JSON Schema is bundled at
  `system.file("schema/markermd-rubric.json", package = "markermd")` if you
  need to check the exact structure.

To validate the file (re-run after every edit), confirm it parses:

```
Rscript -e 'invisible(markermd::read_rubric_yaml("<root>/scratch/markermd-rubric.yaml")); cat("OK\n")'
```

If `jsonvalidate` is installed, also check it against the schema:

```
Rscript -e 'print(markermd::validate_rubric_file("<root>/scratch/markermd-rubric.yaml"))'
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
  existing items. A group whose label matches an existing group of the
  question is merged into it (its items appended to that group, its bounds
  overwriting the stored ones); a new label creates a new group. So when
  the export shows existing groups, reuse a label only to add to that group
  and pick a new label to keep the new items apart. Making an existing group
  exclusive while some repository has two of its items selected aborts the
  import, listing the repositories.
- `replace`: each affected question's existing items and groups are deleted
  first -- along with **all recorded selections of those items, for every
  repository**. This cannot be undone, so say so explicitly when offering
  it.

Ask this once: in the question-at-a-time workflow the chosen mode applies to
every per-question import, and `replace` only touches the questions named in
each individual import call.

### 9. All at once: confirm the full draft, then write and import

Skip this step if the user chose one question at a time (step 10).

Show the complete draft: for each question, its name, total (from the step 5
distribution), and mode, and every item in order with its points and full
description, with each group shown as a labelled section carrying its
floor, ceiling or exclusive flag. Never summarize or elide items; the user
must see exactly what would be imported. Ask the user to confirm or adjust
(item wording, points magnitudes, group bounds, items to add, drop, reorder,
group or ungroup). Do not write any files until the user confirms.

Then write the full rubric YAML and validate it (step 8). Importing is a
separate decision from drafting: show the user the final rubric YAML exactly
as written (print the file's contents) and ask whether to import it, even
though they confirmed the draft above -- fixes made during validation may
have changed it. Never run `rubric_import()` without an explicit yes to this
question. Then import, with the mode chosen in step 8:

```
Rscript -e 'markermd::rubric_import("scratch/markermd-rubric.yaml", project = "<root>", mode = "append", summary = "<one line: what this rubric adds or changes, and why>")'
```

The import takes an automatic snapshot of the rubric and marks it replaces
and records `summary` on it, so always pass one that says what changed and
why (a `replace` import deletes items and every selection of them; the
snapshot is the way back, with `markermd::snapshot_restore()`). After the
import, `Rscript -e 'print(markermd::snapshot_diff("latest", project =
"<root>"))'` reports exactly what it changed; relay that to the user.

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
   and full description, with each group shown as a labelled section
   carrying its floor, ceiling or exclusive flag. Never summarize or elide
   items.
3. Ask the user to confirm or adjust (item wording, points magnitudes, group
   bounds, items to add, drop, reorder, group or ungroup), iterating until
   they confirm. This confirmation
   is the explicit yes that authorizes importing this question; never import
   without it.
4. Add the confirmed question to `<root>/scratch/markermd-rubric.yaml` -- on the
   first question, create the file with its `format_version` header -- and
   validate the file (step 8). If fixing a validation problem changed the
   question's entry, show it again before importing.
5. Import just this question, leaving every other question in the file
   untouched, with the mode chosen in step 8:

   ```
   Rscript -e 'markermd::rubric_import("scratch/markermd-rubric.yaml", project = "<root>", mode = "append", question = "<name>", summary = "<one line: what this question's rubric adds or changes>")'
   ```

   The `summary` is recorded on the automatic snapshot the import takes.

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
chosen, the assignment's total points, each imported question's total, item
count and groups (label and bound or exclusive flag), any document-scope
question added and the problems it holds, and that the rubric
was written to `<root>/scratch/markermd-rubric.yaml`
and imported into the project database (in the question-at-a-time workflow,
also list any questions left for later). If subagents read the student
repositories, say which model they ran on. Then tell them to start grading,
where the rubric is refined in place:

```
markermd::mark("<root>")
```

Point out that the Rubric pane shows the items with their hotkeys (the first
ten per question, in order; any further items are toggled by clicking), that
groups appear as sections whose header shows the live subtotal and flags a
hit bound, that items can be edited, reordered, added and moved between
groups while grading (Add Group creates a group; each item's group menu
moves it), and that the pane's menu offers the same YAML import/export for
later round trips. Remind them the scaffold is a starting point: real
submissions will suggest items no one anticipated.
