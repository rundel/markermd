# markermd skills for Claude Code

This directory holds [Claude Code](https://claude.com/claude-code) skills
distributed with the `markermd` package. A skill is a folder containing a
`SKILL.md` file that teaches Claude Code how to perform a markermd task.

## Available skills

- `scaffold-markermd-template`: scaffold a grading template (YAML) from an
  assignment. It detects the gradable sections and their exact heading anchors
  and writes a starter template with a few basic validation rules, ready to
  refine in the `template()` app.

## Installing a skill

Claude Code loads skills from a `.claude/skills/` directory, either per-user
(`~/.claude/skills/`) or per-project (`<project>/.claude/skills/`). Copy the
skill's folder into one of those locations.

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
