# Report a markermd project's status

Prints a situation report for a markermd project: its root, version and
timestamps, the configured locations along with whether they exist (repo
and comment counts, artifact directories, the grading database, and the
template), and the state of its snapshots (how many, the latest one,
whether the grading state changed since it, and when the scores and
comments were last exported relative to the last change).

## Usage

``` r
project_sitrep(project = ".")
```

## Arguments

- project:

  Path to the project directory. Defaults to the working directory.

## Value

The project's `markermd_project` object, invisibly.

## Examples

``` r
src = system.file("examples/test_assignment", package = "markermd")
project = fs::dir_copy(src, fs::file_temp("markermd_example_"))
init_project(project)
#> ✔ Initialized markermd project at /tmp/RtmpRULnR6/markermd_example_1a8b169dc517
#> • database: .markermd/markermd.sqlite (created)
#> • skills (claude): markermd-apply-rubric, markermd-check-injection,
#>   markermd-fix-parsing, markermd-scaffold-rubric, markermd-scaffold-template
#>   (.claude/skills/)
#> • skills (codex): markermd-apply-rubric, markermd-check-injection,
#>   markermd-fix-parsing, markermd-scaffold-rubric, markermd-scaffold-template
#>   (.agents/skills/)
#> • instructions: CLAUDE.md (created), AGENTS.md (created)
#> • repos: repos/
#> • comments: not found
#> • key: hw3-key
#> • artifacts: html

project_sitrep(project)
#> 
#> ── markermd project ────────────────────────────────────────────────────────────
#> root: /tmp/RtmpRULnR6/markermd_example_1a8b169dc517
#> version: 1.0
#> created: 2026-11-10 03:11:41
#> updated: 2026-11-10 03:11:41
#> 
#> ── Locations ──
#> 
#> • repos: repos (5 repos)
#> • comments: not found
#> • key: hw3-key (present)
#> • artifacts: html
#> • database: .markermd/markermd.sqlite (present)
#> • template: not set
#> 
#> ── Snapshots ──
#> 
#>   • none yet (`snapshot_create()` takes one)
```
