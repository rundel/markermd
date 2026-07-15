# Report a markermd project's status

Prints a situation report for a markermd project: its root, version and
timestamps, and the configured locations along with whether they exist
(repo and comment counts, artifact directories, the grading database,
and the template).

## Usage

``` r
project_sitrep(path = ".")
```

## Arguments

- path:

  Path to the project directory. Defaults to the working directory.

## Value

The project's `markermd_project` object, invisibly.

## Examples

``` r
src = system.file("examples/test_assignment", package = "markermd")
project = fs::dir_copy(src, fs::file_temp("markermd_example_"))
init_project(project)
#> ✔ Initialized markermd project at /tmp/Rtmp8Lc6kN/markermd_example_22113e9e02e8
#> • database: .markermd/markermd.sqlite (created)
#> • skills: markermd-apply-rubric, markermd-scaffold-rubric,
#>   markermd-scaffold-template (.claude/skills/)
#> • repos: repos/
#> • comments: not found
#> • key: hw3-key
#> • artifacts: html

project_sitrep(project)
#> 
#> ── markermd project ────────────────────────────────────────────────────────────
#> root: /tmp/Rtmp8Lc6kN/markermd_example_22113e9e02e8
#> version: 1.0
#> created: 2026-07-15 11:54:50
#> updated: 2026-07-15 11:54:50
#> 
#> ── Locations ──
#> 
#> • repos: repos (5 repos)
#> • comments: not found
#> • key: hw3-key (present)
#> • artifacts: html
#> • database: .markermd/markermd.sqlite (present)
#> • template: not set
```
