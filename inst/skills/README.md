# markermd skills for Claude Code

This directory holds [Claude Code](https://claude.com/claude-code) skills
distributed with the `markermd` package. A skill is a folder containing a
`SKILL.md` file that teaches Claude Code how to perform a markermd task.

## Available skills

- `scaffold-markermd-template`: scaffold a grading template (YAML) for an
  initialized markermd project. It reads `.markermd/config.yml` to find the key
  (solution) repository, builds a starter template from the key's assignment
  (detecting the gradable sections and their exact anchors), records it in the
  config, and validates it across the student repositories, ready to refine in
  the `template()` app.

## Installing a skill

Claude Code loads skills from a `.claude/skills/` directory, either per-user
(`~/.claude/skills/`) or per-project (`<project>/.claude/skills/`). Copy the
skill's folder into one of those locations.

`markermd::init_project()` installs every bundled skill into a project's
`<project>/.claude/skills/` automatically (re-copying to the packaged version on
each run), so the steps below are only needed for a per-user install or when not
using `init_project()`.

From R:

```r
file.copy(
  file.path(markermd::markermd_skills_path(), "scaffold-markermd-template"),
  "~/.claude/skills",      # or "<project>/.claude/skills"
  recursive = TRUE
)
```

From a shell:

```sh
cp -r "$(Rscript -e 'cat(markermd::markermd_skills_path())')/scaffold-markermd-template" ~/.claude/skills/
```

`markermd::markermd_skills_path()` returns the directory these skills are
installed in. After copying, start Claude Code in your project and ask it to
scaffold a template, pointing it at your assignment directory.
