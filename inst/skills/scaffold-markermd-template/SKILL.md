---
name: scaffold-markermd-template
description: Scaffold a markermd grading template (YAML) from an assignment directory or .qmd/.Rmd file. Detects the document's gradable sections and their exact Pandoc heading anchors and writes a starter template with a few basic validation rules, ready to refine in the markermd template() app. Use when a user wants to bootstrap, scaffold, or generate a markermd template for an assignment.
---

# Scaffold a markermd grading template

## Overview

markermd grades student assignments against a template: a YAML file listing
questions, where each question targets one or more document sections by their
Pandoc heading anchors and carries validation rules. This skill builds the
scaffolding so the user does not have to do it by hand: it identifies the
gradable sections and their exact anchors and adds a few minimal starter rules.
The user then refines the specifics in the `template()` app.

The scaffold is intentionally basic. The goal is to save the user the
mechanical setup (finding sections, copying anchors, wiring up the file shape),
not to produce a finished rubric.

One hard rule: heading and div anchors must be the exact ids that Pandoc
assigns. Never invent, guess, or hand-derive an anchor. Always take anchors from
the `assignment_outline()` helper in step 2.

## Inputs

The user provides a path to an assignment directory or a single `.qmd`/`.Rmd`
file. If they only describe the assignment without a path, ask for it. The
template is written to `markermd-template.yaml` inside the assignment directory
by default; ask if the user wants a different location.

This skill requires the `markermd` R package to be installed. If the commands
below fail with a package-not-found error, ask the user to install it first.

## Steps

### 1. Locate the assignment file

If given a directory, find the single assignment in it, for example
`ls <dir>/*.qmd` (also check `*.Rmd`). If more than one matches, ask the user
which one. Remember the filename: you will use it as `source.path`.

### 2. Get the ground-truth outline (sections, anchors, content counts)

Run this, replacing PATH with the directory or file from step 1:

```
Rscript -e 'o <- markermd::assignment_outline("PATH"); cat(jsonlite::toJSON(list(source_file = attr(o, "source_file"), sections = o), dataframe = "rows", auto_unbox = TRUE, pretty = TRUE))'
```

It prints the resolved `source_file` and a `sections` array. Each section has:

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

### 3. Choose which sections become questions

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

### 4. Add starter rules (keep them minimal)

For each chosen question, add a few rules driven by its content counts:

- if `n_chunk >= 1`: `node_type: Chunk`, `verb: has at least`, `count: 1`
- else if `n_code_block >= 1`: `node_type: Code block`, `verb: has at least`, `count: 1`
- if `n_markdown >= 1`: `node_type: Markdown`, `verb: has at least`, `count: 1`
- if `n_table >= 1`: optionally `node_type: Table`, `verb: has at least`, `count: 1`

Do not add `has content` / `lacks content` / `has name` pattern rules: those are
assignment-specific and are exactly what the user will fill in. If a section has
no content counts, give it no rules.

### 5. Write the template YAML

Write the file (default `markermd-template.yaml` in the assignment directory).
Set `source.path` to the assignment filename relative to the template's location
(just the filename when both sit in the same directory). Use the exact `id`s
from step 2. Shape:

```yaml
format_version: "3.0"
source:
  path: assignment.qmd
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

### 6. Validate

Confirm the file loads and every anchor resolves against the re-parsed
assignment:

```
Rscript -e 'invisible(markermd::read_template_yaml("PATH_TO_YAML", require_ast = TRUE)); cat("OK\n")'
```

If `jsonvalidate` is installed, also check it against the schema:

```
Rscript -e 'print(markermd::validate_template_file("PATH_TO_YAML"))'
```

`TRUE` means it conforms. Fix any reported problems (commonly a mistyped anchor,
a missing value key for a verb, or duplicate names) and re-validate until it
loads cleanly.

### 7. Report

Summarize for the user: which sections became questions, the anchors used, and
the starter rules added. Then tell them to open and refine it:

```
markermd::template("PATH_TO_YAML")
```

Remind them the scaffold is deliberately minimal and that the point of the app
is to tighten the rules and add the assignment-specific content checks.
