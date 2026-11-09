---
name: markermd-scaffold-template
description: >-
  Scaffold a markermd grading template (YAML) for an initialized markermd project, using the project's key (solution) repository as the basis, then validate it against every student repository. Reads the project config (.markermd/config.yml) to locate the key and the student repos, writes a starter template with a few basic validation rules, imports it into the project's grading database, and reports how the rules hold up across submissions. Use when a user wants to bootstrap, scaffold, or generate a markermd template for an assignment project created with markermd::init_project() / ghclass::org_grade_assignment().
---

# Scaffold a markermd grading template

## Overview

markermd grades student assignments against a template: a set of questions,
where each question targets one or more document sections by their Pandoc
heading (or div) anchors and carries validation rules. The canonical store for
the template is the project's grading database (`.markermd/markermd.sqlite`);
YAML is the human-authorable import/export format. This skill builds that
scaffolding so the user does not have to do it by hand.

It works on an **initialized markermd project** (`markermd::init_project()`,
which sets up a `ghclass::org_grade_assignment()` layout). The project records
its file locations in `<root>/.markermd/config.yml`:

- `paths.key` -- the key (solution) repository; its assignment document is the
  **basis for the template**.
- `paths.repos` -- the directory of student repositories the template is
  **validated against**.

The flow is: read the config -> build the template YAML from the key -> import
it into the project database -> validate it across all student repos -> report.
The scaffold is intentionally basic; the user refines the specifics in the
`template()` app.

The default assumption is that **every question expects both a code chunk and a
markdown write-up**. The key's content counts and the assignment's question text
(usually `README.md`, sometimes the qmd prose) are used to confirm or adjust
that per question; anything genuinely ambiguous is checked with the user before
generating.

One hard rule: heading and div anchors must be the exact ids that Pandoc
assigns. Never invent, guess, or hand-derive an anchor. Always take anchors from
the `assignment_outline()` helper in step 3.

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
below. The template itself lives in the grading database, not in the config.

### 2. Locate the key (the template basis)

The template is built from the key/solution repository's assignment document.

- If `paths.key` is set, the key directory is `<root>/<paths.key>`.
- If `paths.key` is missing/null, **ask the user** where the key (solution)
  repository is, relative to the project root, then record it:

  ```
  Rscript -e 'markermd::project_set("<root>", key = "<key-dir>")'
  ```

  Re-read the config afterwards.

### 3. Get the ground-truth outline from the key's assignment

Run this, replacing KEYDIR with `<root>/<paths.key>`:

```
Rscript -e 'o <- markermd::assignment_outline("KEYDIR"); cat(jsonlite::toJSON(list(source_file = attr(o, "source_file"), sections = o, files = attr(o, "files")), dataframe = "rows", auto_unbox = TRUE, pretty = TRUE))'
```

It resolves the single assignment file in the key directory and prints its
`source_file`, a `sections` array, and a `files` array of the key's other
files (each with its `path`, `size`, and `filtered`, true for hidden files,
render output, lock files, project files and the README). If it instead aborts with a parser
diagnostic (a bracketed code such as `[Q-2-10] Closed Quote Without Matching
Open Quote` and a source excerpt), the key itself does not parse and nothing
downstream works, since `source.path` is re-parsed whenever the template is
built or loaded. Hand this to the `markermd-fix-parsing` skill, whose key step
applies the same minimal escape to the instructor's document with their
permission, then re-run this command. If the key holds more than one assignment
document, `assignment_outline()` errors listing the candidates; ask the user
which one and pass that file directly instead of the directory. The resolved
`source_file` is an absolute path; you will need it as a path relative to the
project root for `source.path` in step 6.

Each section has:

- `type`: `heading` or `div`
- `id`: the anchor to use verbatim as a `node_id`
- `level`: heading level (null for divs)
- `title`: the heading text (or a label for divs)
- `n_subsections`: number of nested child headings
- content counts: `n_chunk` (executable code chunks), `n_code_block` (static
  code), `n_markdown` (prose paragraphs), `n_list`, `n_table`, `n_div`,
  `n_other`, and `n_content` (all non-heading blocks)

The counts are cumulative for the whole section, exactly as a rule scoped to
that anchor would see them during grading.

### 4. Read the question prose and choose which sections become questions

Before scoping rules, read the assignment's instructions so the template
reflects what each question actually asks for, not just what the key happens to
contain. Look in:

- `<root>/<paths.key>/README.md` -- most assignments state the per-question
  expectations here.
- the assignment qmd itself (the `source_file` from step 3) -- the prose under
  each question heading.

Use the titles and levels to pick the gradable units. These are usually the
repeated sections such as "Question 1", "Exercise 2", "Task 3", or "Part A".
Skip non-gradable material such as a document title, "Setup", "Instructions",
"Getting started", or "Submission".

Pick the level that isolates each question on its own (often `h2`, sometimes
`h1`). Do not also turn that question's child subsections into separate
questions unless each child is independently graded, since their content
overlaps the parent and would produce redundant rules. A section whose counts
are large only because `n_subsections` is high is a container, not a single
question; prefer its children.

Use each chosen section's `id` verbatim as the question's single `node_id`, and
give the question a short `name` derived from its title (for example `Q1`).

When the assignment asks for work in files besides the document (the
README or the document's prose names scripts students write, such as
`get_data.R` or `parse_lq.py`), add one question per graded file: list the
file under the template's top-level `files:`, give the question
`file: <path>` (the path from the `files` array, relative to the key root)
and no `node_ids`, and name it after the file or the task it serves. Such a
question takes no rules; it covers the whole file, and validation only
checks that each repository has it. Leave out support files nobody grades
(lock files, project files, a provided Makefile or data file), and do not
make a file question for code that lives in the document's chunks.

Do not add a document-scope question (`scope: document`) here. It exists for
problems outside every question, such as a render setting in the YAML header,
and is added by the rubric skills only once some submission turns out to have
one; a template that anticipates it gives every project an empty question.

### 5. Add starter rules (default to code + write-up)

The default expectation is that **every question contains both an executable
code chunk and a markdown write-up**, so the starter rules for a typical
question are a `Chunk has at least 1` and a `Markdown has at least 1`. Use the
question prose from step 4 together with the key's content counts to decide when
to deviate from that default:

- **Code rule.** Add `node_type: Chunk`, `verb: has at least`, `count: 1` by
  default. A lack of a code chunk in the key (`n_chunk == 0`), together with
  prose that does not ask for code, is a decent signal that code is not expected
  -- drop the code rule in that case. If the key has no chunk but uses static
  code (`n_code_block >= 1`), use `node_type: Code block` instead.
- **Write-up rule.** Add `node_type: Markdown`, `verb: has at least`, `count: 1`
  by default. A prose write-up is expected for almost every question, so keep
  this rule even when `n_markdown == 0` in the key, unless the question is
  clearly code-only (e.g. "write a function", with no narrative requested).
- **Other content.** If `n_table >= 1`, optionally add `node_type: Table`,
  `verb: has at least`, `count: 1`.

Do not add `has content` / `lacks content` / `has name` pattern rules: those are
assignment-specific and are exactly what the user will fill in.

These defaults are deliberately generous and easy to correct in the `template()`
app later. But if a question's expectation is genuinely unclear -- the prose is
ambiguous and the key's content does not settle whether code or a write-up is
required -- **ask the user before generating that question's rules** rather than
guessing.

### 6. Write the template YAML and import it into the project

Write the file to `<root>/scratch/markermd-template.yaml` (ask if the user wants a
different location). This YAML is the import source; the template is stored in
the project database once imported. Set `source.path` to the key's assignment
**relative to the project root** (the absolute `source_file` from step 3 with the
project-root prefix stripped, e.g. `hw1-key/hw1.qmd`) so the template can be
re-opened against the key in the `template()` app and so the student documents
are matched by the same filename.
Use the exact `id`s from step 3. Shape:

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
      - node_type: Markdown
        verb: has at least
        count: 1
```

Rules and constraints:

- Value keys depend on the verb: `has between` uses `min` and `max`;
  `has at least` / `has at most` use `count`; `has content` / `lacks content` /
  `has name` use `pattern`.
- Allowed `node_type` values: `Any node`, `Heading`, `Markdown`, `Chunk`,
  `Code block`, `Raw Block`, `HTML comment`, `Div`, `Bullet list`,
  `Ordered list`, `Block quote`, `Table`, `Figure`, `Horizontal rule`,
  `Definition list`, `Line block`. A list-valued `node_type` is a union.
  A raw block that is nothing but `<!-- ... -->` comments is an
  `HTML comment`, never a `Raw Block`.
- Question `id`s and `name`s must each be unique.
- The full JSON Schema is bundled at
  `system.file("schema/markermd-template.json", package = "markermd")` if you
  need to check the exact structure.

Before importing, confirm the file parses and the key document can be located.
Pass the YAML file's absolute path: `source.path` is relative to the project
root, and `read_template_yaml()` resolves it against the root of the project
the YAML file lies in (then the file's directory, then the working
directory), so a file under `<root>/scratch/` finds the key from any working
directory. `require_ast = TRUE` only re-parses
the key to confirm the document resolves; it does NOT check that each
`node_id` exists in the document, so a mistyped anchor passes here and only
surfaces in step 7, where `validate_project()` fails every repository on that
question with "Selected section #id not found in this document":

```
Rscript -e 'invisible(markermd::read_template_yaml("<root>/scratch/markermd-template.yaml", require_ast = TRUE)); cat("OK\n")'
```

If `jsonvalidate` is installed, also check it against the schema:

```
Rscript -e 'print(markermd::validate_template_file("<root>/scratch/markermd-template.yaml"))'
```

`TRUE` means it conforms. Fix any reported problems (commonly a missing value
key for a verb, or duplicate names) and re-validate until it loads cleanly.
Anchor typos are not caught here; they are caught by `validate_project()` in
step 7. A `pattern` that is not a valid regular expression fails its rule in
every repository, so escape literal parentheses and brackets.

Then import the template into the project database (its canonical store):

```
Rscript -e 'markermd::template_import("<root>/scratch/markermd-template.yaml", project = "<root>", summary = "<one line: what this template covers, or what changed>")'
```

If a template is already stored in the project, ask the user whether to
regenerate it or just refine the existing one before overwriting. Either way,
start from the stored template rather than a fresh numbering: export it with
`markermd::template_export("<root>/scratch/markermd-template.yaml", project =
"<root>")` and edit that file. Question ids are what carry grading across a
rename, so every existing question keeps its `id` (even when its name or
anchors change) and a new question takes an id above the largest stored one.
Renumbering the questions 1..n would move each shifted id's rubric and marks
onto whichever question now holds that id. The import warns about every
rename it carries grading through; a rename you did not intend means an id
moved, so undo the import and fix the file. The import takes an automatic
snapshot of the state it replaces and records `summary` on it, so always pass
one; `markermd::snapshot_restore("latest", part = "template", project =
"<root>")` undoes the import, moving any carried-over grading back to the
restored question names.

### 7. Validate the template against every student repository

The template and its rules are now used to check all student submissions.

- If `paths.repos` is missing/null in the config, **ask the user** where the
  student repositories live (relative to the project root) and record it:

  ```
  Rscript -e 'markermd::project_set("<root>", repos = "<repos-dir>")'
  ```

Then run the headless validator, which loads the stored template from the
project database, discovers the repositories under `paths.repos`, and validates
each:

```
Rscript -e 'res <- markermd::validate_project("<root>"); print(res, row.names = FALSE)'
```

It returns one row per repository/question with columns `repo`, `question`,
`status` (`pass`, `fail`, or `error`), and `detail`. It does not report parse
warnings, so also run `markermd::parse_project("<root>")`: its `warning` rows
are documents that parsed with diagnostics (an HTML element passed through as
raw HTML, for example). Grade those normally, but check a widely failing rule
against those warnings before calling the rule too strict. Summarize the
results: how many repos pass each question, and which rules fail widely.
Interpretation:

- A rule that **fails for almost every repo** is usually too strict -- flag it
  for relaxing. When the `detail` reads "Selected section #id not found in
  this document", the anchor is wrong or the students renamed the heading:
  fix the `node_id`, never the rule.
- A `status` of `error` means the repo's assignment file was missing or failed
  to parse; list those repos for the user. For the ones that failed to parse,
  point the user at the `markermd-fix-parsing` skill: it shows each parser
  diagnostic (`markermd::parse_project("<root>")`), repairs the document with
  the smallest possible edit on a `grading-fixes` branch in the student's
  repository, and re-runs this validation.
- Scattered failures are expected (those students genuinely did not meet the
  rule) and are the point of grading.
- A question with `file:` fails where the repo lacks that file (`detail`
  reads "not found in the repository"); a file found elsewhere in the repo
  passes and its `detail` names where. A file question that fails for almost
  every repo usually has the wrong path: check it against a few student
  repos.
- A repo that **fails the write-up rule on every question** usually wrote its
  answers inside `{=html}` raw blocks, which are `Raw Block` nodes rather than
  `Markdown`. Node kinds are never template problems to fix by loosening
  rules blindly, so ask the user whether such write-ups are acceptable. If
  they are, widen the write-up rule's `node_type` to the union
  `[Markdown, Raw Block]`. No filter is needed to keep comments out: a raw
  block that is nothing but `<!-- ... -->` comments is its own `HTML comment`
  kind, so a write-up left inside the skeleton's HTML comment placeholder
  (which never renders) still fails, and the `<!-- markermd: ... -->` repair
  markers left by the `markermd-fix-parsing` skill are not counted either:

  ```yaml
  rules:
    - node_type: Chunk
      verb: has at least
      count: 1
    - node_type:
        - Markdown
        - Raw Block
      verb: has at least
      count: 1
  ```

### 8. Report

Summarize for the user: which sections became questions, the anchors used, the
starter rules added, that the template was written to
`<root>/scratch/markermd-template.yaml` and imported into the project database, and the
validation summary from step 7. Then tell them to open and refine it (passing
the project root loads the stored template and saves edits back to the database):

```
markermd::template("<root>")
```

Remind them the scaffold is deliberately minimal and that the point of the app
is to tighten the rules and add the assignment-specific content checks.
