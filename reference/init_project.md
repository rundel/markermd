# Initialize a markermd grading project

Bootstraps a markermd project in a directory laid out by
`ghclass::org_grade_assignment()` (a `repos/` directory of student
repositories, a `comments/` directory, an optional key/solution
repository, and a directory per downloaded artifact). It creates a
`.markermd/` directory at the project root holding the SQLite grading
database and a `config.yml` recording the project's file locations, and
installs the bundled agent skills so they are discoverable from the
project: the Claude Code set into `<project>/.claude/skills/` and the
Codex set into `<project>/.agents/skills/` (see `agents`). It also
writes each selected agent's project instructions file at the project
root (`CLAUDE.md` for Claude Code, `AGENTS.md` for Codex) describing the
project layout, the grading workflow and skills, and the rules an agent
must follow when working with student submissions. An existing
instructions file is kept, so it can be customized freely, unless
`overwrite_instructions = TRUE`; pass `instructions = FALSE` to skip
this step.

## Usage

``` r
init_project(
  project,
  agents = c("claude", "codex"),
  instructions = TRUE,
  overwrite_instructions = FALSE,
  rescan_artifacts = FALSE
)
```

## Arguments

- project:

  Path to the project directory.

- agents:

  Which agents' skill sets to install: `"claude"` (into
  `<project>/.claude/skills/`), `"codex"` (into
  `<project>/.agents/skills/`), or both, the default. The same selection
  decides which instructions files are written.

- instructions:

  Whether to write the selected agents' project instructions files
  (`CLAUDE.md` / `AGENTS.md`) at the project root when they do not
  already exist. Defaults to `TRUE`.

- overwrite_instructions:

  Whether to replace existing instructions files with the packaged
  versions, discarding any local edits to them (for example to pick up
  the instructions shipped with a newer markermd). Defaults to `FALSE`;
  has no effect when `instructions = FALSE`.

- rescan_artifacts:

  Whether a re-run rebuilds the artifact directory list from every
  non-repository top-level directory instead of keeping the configured
  one. Defaults to `FALSE`; the first run always scans.

## Value

A `markermd_project` object, invisibly.

## Details

The key (solution) repository is detected as the top-level git
repository (student repositories live under `repos/`); when several
top-level repos exist, the one whose name contains "key" is used. When
no top-level git repository exists (a copied or unpacked layout, such as
the bundled example project), a directory whose name contains "key" and
that holds an assignment document (`.qmd`/`.Rmd`) is used instead. On
the first run the remaining top-level directories that are not git
repositories are recorded as artifact directories.

Re-running `init_project()` on an already-initialized directory updates
it in place: the existing grading database (including any stored grading
template) is preserved, the skills are refreshed to the packaged
version, existing instructions files are left untouched (unless
`overwrite_instructions = TRUE`), and a previously configured key is
kept (or cleared if it no longer exists). The configured artifact
directories are kept too, minus any that no longer exist: top-level
directories created since (working notes, backups) are listed but not
added, so add one with `project_set(artifacts = )`, or pass
`rescan_artifacts = TRUE` to rebuild the list from every non-repository
top-level directory as on the first run.

## Examples

``` r
if (FALSE) { # \dontrun{
ghclass::org_grade_assignment("hw01", org = "my-course", repo_filter = "hw01-")
init_project("hw01")
} # }
```
