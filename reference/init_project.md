# Initialize a markermd grading project

Bootstraps a markermd project in a directory laid out by
[`ghclass::org_grade_assignment()`](https://rdrr.io/pkg/ghclass/man/org_grade_assignment.html)
(a `repos/` directory of student repositories, a `comments/` directory,
an optional key/solution repository, and a directory per downloaded
artifact). It creates a `.markermd/` directory at the project root
holding the SQLite grading database and a `config.yml` recording the
project's file locations, and installs the bundled Claude Code skills
into `<path>/.claude/skills/` so they are discoverable from the project.

## Usage

``` r
init_project(path)
```

## Arguments

- path:

  Path to the project directory.

## Value

A `markermd_project` object, invisibly.

## Details

The key (solution) repository is detected as the top-level git
repository (student repositories live under `repos/`); when several
top-level repos exist, the one whose name contains "key" is used.
Remaining top-level directories that are not git repositories are
recorded as artifact directories.

Re-running `init_project()` on an already-initialized directory updates
it in place: the existing grading database (including any stored grading
template) is preserved, the skills are refreshed to the packaged
version, the key and artifact directories are re-scanned, and a
previously configured key is kept (or cleared if it no longer exists).

## Examples

``` r
if (FALSE) { # \dontrun{
ghclass::org_grade_assignment("hw01", org = "my-course", repo_filter = "hw01-")
init_project("hw01")
} # }
```
