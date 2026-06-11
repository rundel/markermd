# Path to the bundled markermd Claude Code skills

Returns the directory holding the Claude Code skills distributed with
the package (e.g. the assignment template scaffolding skill). Copy a
skill's sub-directory into a `.claude/skills/` directory (per-user
`~/.claude/skills` or per-project) to make it available to Claude Code.
See the README in that directory for details.

## Usage

``` r
markermd_skills_path()
```

## Value

Character path to the installed `skills` directory.
