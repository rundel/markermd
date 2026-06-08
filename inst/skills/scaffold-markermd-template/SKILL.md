---
name: scaffold-markermd-template
description: Scaffold a markermd grading template (YAML) for an initialized markermd project, using the project's key (solution) repository as the basis, then validate it against every student repository. Reads the project config (.markermd/config.yml) to locate the key and the student repos, writes a starter template with a few basic validation rules, records it in the config, and reports how the rules hold up across submissions. Use when a user wants to bootstrap, scaffold, or generate a markermd template for an assignment project created with markermd::init_project() / ghclass::org_grade_assignment().
---

# Scaffold a markermd grading template

## Overview

markermd grades student assignments against a template: a YAML file listing
questions, where each question targets one or more document sections by their
Pandoc heading (or div) anchors and carries validation rules. This skill builds
that scaffolding so the user does not have to do it by hand.

It works on an **initialized markermd project** (`markermd::init_project()`,
which sets up a `ghclass::org_grade_assignment()` layout). The project records
its file locations in `<root>/.markermd/config.yml`:

- `paths.key` -- the key (solution) repository; its assignment document is the
  **basis for the template**.
- `paths.repos` -- the directory of student repositories the template is
  **validated against**.
- `paths.template` -- where the generated template is recorded.

The flow is: read the config -> build the template from the key -> record it ->
validate it across all student repos -> report. The scaffold is intentionally
basic; the user refines the specifics in the `template()` app.

The default assumption is that **every question expects both a code chunk and a
markdown write-up**. The key's content counts and the assignment's question text
(usually `README.md`, sometimes the qmd prose) are used to confirm or adjust
that per question; anything genuinely ambiguous is checked with the user before
generating.

One hard rule: heading and div anchors must be the exact ids that Pandoc
assigns. Never invent, guess, or hand-derive an anchor. Always take anchors from
the `assignment_outline()` helper in step 3.

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

Note the `paths.key`, `paths.repos`, and `paths.template` values; you will use or
fill in each below.

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
Rscript -e 'o <- markermd::assignment_outline("KEYDIR"); cat(jsonlite::toJSON(list(source_file = attr(o, "source_file"), sections = o), dataframe = "rows", auto_unbox = TRUE, pretty = TRUE))'
```

It resolves the single assignment file in the key directory and prints its
`source_file` plus a `sections` array. If the key holds more than one assignment
document, `assignment_outline()` errors listing the candidates; ask the user
which one and pass that file directly instead of the directory. Note the
resolved `source_file` -- you need its path (relative to the project root) for
`source.path` in step 6.

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

### 6. Write the template YAML and record it in the config

Write the file to `<root>/markermd-template.yaml` (ask if the user wants a
different location). Set `source.path` to the key's assignment **relative to the
project root** (the `source_file` from step 3, e.g. `hw1-key/hw1.qmd`) so the
template can be re-opened against the key in the `template()` app and so the
student documents are matched by the same filename. Use the exact `id`s from
step 3. Shape:

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
  `Code block`, `Raw Block`, `Div`, `Bullet list`, `Ordered list`,
  `Block quote`, `Table`.
- Question `id`s and `name`s must each be unique.
- The full JSON Schema is bundled at
  `system.file("schema/markermd-template.json", package = "markermd")` if you
  need to check the exact structure.

Then record the template in the project config:

```
Rscript -e 'markermd::project_set("<root>", template = "markermd-template.yaml")'
```

If `paths.template` was already set, ask the user whether to regenerate it or
just refine the existing one before overwriting.

Confirm the file loads and every anchor resolves against the re-parsed key:

```
Rscript -e 'invisible(markermd::read_template_yaml("<root>/markermd-template.yaml", require_ast = TRUE)); cat("OK\n")'
```

If `jsonvalidate` is installed, also check it against the schema:

```
Rscript -e 'print(markermd::validate_template_file("<root>/markermd-template.yaml"))'
```

`TRUE` means it conforms. Fix any reported problems (commonly a mistyped anchor,
a missing value key for a verb, or duplicate names) and re-validate until it
loads cleanly.

### 7. Validate the template against every student repository

The template and its rules are now used to check all student submissions.

- If `paths.repos` is missing/null in the config, **ask the user** where the
  student repositories live (relative to the project root) and record it:

  ```
  Rscript -e 'markermd::project_set("<root>", repos = "<repos-dir>")'
  ```

Then run the headless validator, which loads the configured template, discovers
the repositories under `paths.repos`, and validates each:

```
Rscript -e 'res <- markermd::validate_project("<root>"); print(res, row.names = FALSE)'
```

It returns one row per repository/question with columns `repo`, `question`,
`status` (`pass`, `fail`, or `error`), and `detail`. Summarize the results:
how many repos pass each question, and which rules fail widely. Interpretation:

- A rule that **fails for almost every repo** is usually too strict (or the
  anchor does not exist in the students' documents) -- flag it for relaxing.
- A `status` of `error` means the repo's assignment file was missing or failed
  to parse; list those repos for the user.
- Scattered failures are expected (those students genuinely did not meet the
  rule) and are the point of grading.

### 8. Report

Summarize for the user: which sections became questions, the anchors used, the
starter rules added, that the template was written to
`<root>/markermd-template.yaml` and recorded in the project config, and the
validation summary from step 7. Then tell them to open and refine it:

```
markermd::template("<root>/markermd-template.yaml")
```

Remind them the scaffold is deliberately minimal and that the point of the app
is to tighten the rules and add the assignment-specific content checks.
