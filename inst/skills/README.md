# markermd skills for Claude Code

This directory holds [Claude Code](https://claude.com/claude-code) skills
distributed with the `markermd` package. A skill is a folder containing a
`SKILL.md` file that teaches Claude Code how to perform a markermd task.

## Available skills

- `markermd-scaffold-template`: scaffold a grading template (YAML) for an
  initialized markermd project. It reads `.markermd/config.yml` to find the key
  (solution) repository, builds a starter template from the key's assignment
  (detecting the gradable sections and their exact anchors), records it in the
  config, and validates it across the student repositories, ready to refine in
  the `template()` app.
- `markermd-scaffold-rubric`: scaffold a grading rubric (per-question rubric
  items plus scoring setup) for a project that already has a stored template.
  It asks whether to base the rubric on the key alone or on the key plus the
  student repositories (sampling submissions to anticipate common mistakes),
  drafts concrete key-derived items, validates the YAML against the bundled
  schema, and imports it with `rubric_import()`, ready to refine in `mark()`'s
  Rubric pane.
- `markermd-apply-rubric`: apply the stored rubric to the student repositories
  as a first-pass machine grading. It reads each repository's per-question
  sections, selects the rubric items that apply (matching descriptions
  verbatim), records its reasoning as private notes (never shared with
  students), and imports the result with `marks_import()` for human review in
  `mark()`. Pairs that already have grading activity are skipped by default.

Underlying these skills, templates, rubrics, and marks are LLM-authorable
YAML: `rubric_export()` / `marks_export()` write a project's rubric or
per-repository marks to files any LLM tool can draft or refine against the
JSON Schemas at `system.file("schema", package = "markermd")`
(`markermd-rubric.json`, `markermd-marks.json`), and `rubric_import()` /
`marks_import()` bring them back into the project database. The rubric
import/export is also available inside `mark()` from the Rubric pane's menu.

## Installing a skill

Claude Code loads skills from a `.claude/skills/` directory, either per-user
(`~/.claude/skills/`) or per-project (`<project>/.claude/skills/`). Copy the
skill's folder into one of those locations.

`markermd::init_project()` installs every bundled skill into a project's
`<project>/.claude/skills/` automatically (re-copying to the packaged version on
each run), so the steps below are only needed for a per-user install or when not
using `init_project()`. Re-running it installs skills under their current names
but never deletes old ones, so after a skill rename a stale folder (e.g.
`scaffold-markermd-template/`) can simply be removed by hand.

From R:

```r
file.copy(
  file.path(markermd::markermd_skills_path(), "markermd-scaffold-template"),
  "~/.claude/skills",      # or "<project>/.claude/skills"
  recursive = TRUE
)
```

From a shell:

```sh
cp -r "$(Rscript -e 'cat(markermd::markermd_skills_path())')/markermd-scaffold-template" ~/.claude/skills/
```

`markermd::markermd_skills_path()` returns the directory these skills are
installed in. After copying, start Claude Code in your project and ask it to
scaffold a template, pointing it at your assignment directory.
