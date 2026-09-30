# Path to the bundled markermd agent skills

Returns the directory holding one agent's set of the skills distributed
with the package: the Claude Code set, which
[`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)
installs into `<project>/.claude/skills/`, or the Codex set, installed
into `<project>/.agents/skills/`. Both follow the Agent Skills format (a
`SKILL.md` per skill) and share skill names, differing only in
agent-specific wording and metadata. The root of each set also holds
that agent's project instructions file (`CLAUDE.md` or `AGENTS.md`),
which
[`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)
copies to the project root when none exists there. Copy a skill's
sub-directory into the per-user directory (`~/.claude/skills/` or
`~/.agents/skills/`) to make it available outside a project. See the
README in the `skills` directory for details.

## Usage

``` r
markermd_skills_path(agent = c("claude", "codex"))
```

## Arguments

- agent:

  Which set: `"claude"` (the default) or `"codex"`.

## Value

Character path to the installed skills directory for that agent.
