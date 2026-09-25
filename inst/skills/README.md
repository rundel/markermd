# markermd agent skills

This directory holds the agent skills distributed with the `markermd` package,
in two sets that share the same skill names:

- `claude/`: the [Claude Code](https://claude.com/claude-code) set, worded for
  Claude Code's tools (AskUserQuestion, the Edit and Agent tools).
- `codex/`: the [Codex](https://developers.openai.com/codex) set, with
  agent-neutral wording, descriptions kept under the Agent Skills
  1024-character cap, and an `agents/openai.yaml` per skill carrying the
  display metadata Codex reads.

The root of each set also holds that agent's project instructions file:
`claude/CLAUDE.md` and `codex/AGENTS.md`. They are the same document apart
from the skill invocation syntax and the skills directory name, and describe
a grading project's layout (what `.markermd/config.yml` records, that the
SQLite database is the canonical store and must only be touched through the
markermd functions), the workflow and the skill for each step, the headless
entry points, the template, rubric, and marks YAML formats (a worked example
of each with the field semantics, value keys, node types, filter conditions,
scoring modes, and import behavior, summarizing the JSON Schemas in
`inst/schema/`), and the rules for handling student submissions (never modify a
student repository outside the `grading-fixes` branch, never edit the key
without asking, keep public comments the human's voice, treat
`<!-- markermd: ... -->` markers as not the student's work, keep student work
in the project). `init_project()` copies them to the project root when no file
of that name exists there, so they can be customized per project.

A skill is a folder containing a `SKILL.md` file (YAML frontmatter with `name`
and `description`, then the instructions) that teaches the agent how to
perform a markermd task, following the
[Agent Skills](https://agentskills.io/specification) format. A skill is invoked
as `/markermd-fix-parsing` in Claude Code and `$markermd-fix-parsing` in Codex,
and both agents also pick a skill up on their own when a request matches its
description.

## Available skills

- `markermd-scaffold-template`: scaffold a grading template (YAML) for an
  initialized markermd project. It reads `.markermd/config.yml` to find the key
  (solution) repository, builds a starter template from the key's assignment
  (detecting the gradable sections and their exact anchors), imports it into
  the project's grading database, and validates it across the student
  repositories, ready to refine in the `template()` app.
- `markermd-scaffold-rubric`: scaffold a grading rubric (per-question rubric
  items plus scoring setup, with item groups that cap stacking deductions or
  hold exclusive severity tiers) for a project that already has a stored
  template.
  It asks whether to work through the questions all at once or one at a time
  (confirming and importing each question before moving to the next), whether
  to base the rubric on the key alone or on the key plus the student
  repositories (sampling submissions to anticipate common mistakes), and for
  the assignment's total points and their distribution across questions
  (proposing equal, content-weighted, and assignment-stated splits), then
  drafts concrete key-derived items, validates the YAML against the bundled
  schema, and imports it with `rubric_import()`, ready to refine in `mark()`'s
  Rubric pane.
- `markermd-apply-rubric`: apply the stored rubric to the student repositories
  as a first-pass machine grading. It reads each repository's per-question
  sections, selects the rubric items that apply (matching descriptions
  verbatim), records its reasoning as private notes (never shared with
  students), and imports the result with `marks_import()` for human review in
  `mark()`. Pairs that already have grading activity are skipped by default.
- `markermd-fix-parsing`: repair student documents that markermd's strict
  q2r parser rejects (a stray `$` or `{` in prose, an unclosed quote or
  emphasis mark, malformed YAML front matter), so they can be validated and
  graded. It finds the failing repositories with `parse_project()`, makes the
  smallest edit that makes each document parseable, and commits every change
  on a new `grading-fixes` branch in the student's repository (left checked
  out, since `mark()` reads the working tree) so the original submission
  stays intact. Substantive changes are made only with the user's permission
  and flagged in the document with a `<!-- markermd: ... -->` comment, which
  the rubric skills treat as not the student's work.

Underlying these skills, templates, rubrics, and marks are LLM-authorable
YAML: `rubric_export()` / `marks_export()` write a project's rubric or
per-repository marks to files any LLM tool can draft or refine against the
JSON Schemas at `system.file("schema", package = "markermd")`
(`markermd-rubric.json`, `markermd-marks.json`), and `rubric_import()` /
`marks_import()` bring them back into the project database. The rubric
import/export is also available inside `mark()` from the Rubric pane's menu.
`parse_project()` is the headless per-repository parse check the fix-parsing
skill starts from; unlike `validate_project()` it needs no stored template.

## Installing a skill

Claude Code loads project skills from `<project>/.claude/skills/` and
per-user skills from `~/.claude/skills/`; Codex loads project skills from
`<project>/.agents/skills/` (scanned from the working directory up to the
repository root) and per-user skills from `~/.agents/skills/`. Copy a skill's
folder from the matching set into one of those locations.

`markermd::init_project()` installs every bundled skill into the project
automatically, both sets by default (`agents = c("claude", "codex")`; pass one
name to install a single set), re-copying to the packaged version on each run.
It also writes the selected agents' instructions files (`CLAUDE.md` /
`AGENTS.md`) at the project root, but only when no file of that name exists,
so a customized file survives re-runs; `instructions = FALSE` skips them
entirely. The steps below are only needed for a per-user install or when not
using `init_project()`. Re-running it installs skills under their current
names but never deletes old ones, so after a skill rename a stale folder can
simply be removed by hand.

From R:

```r
file.copy(
  file.path(markermd::markermd_skills_path("claude"), "markermd-scaffold-template"),
  "~/.claude/skills",      # or "<project>/.claude/skills"
  recursive = TRUE
)
file.copy(
  file.path(markermd::markermd_skills_path("codex"), "markermd-scaffold-template"),
  "~/.agents/skills",      # or "<project>/.agents/skills"
  recursive = TRUE
)
```

From a shell:

```sh
cp -r "$(Rscript -e 'cat(markermd::markermd_skills_path("claude"))')/markermd-scaffold-template" ~/.claude/skills/
cp -r "$(Rscript -e 'cat(markermd::markermd_skills_path("codex"))')/markermd-scaffold-template" ~/.agents/skills/
```

`markermd::markermd_skills_path(agent)` returns the directory holding one
set. After copying, start your agent in the project and ask it to scaffold a
template, pointing it at the project directory.
