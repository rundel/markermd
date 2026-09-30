# Read a markermd project's configuration

Loads the `config.yml` written by
[`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)
and returns it as a `markermd_project` object. Errors if the directory
has not been initialized.

## Usage

``` r
project_config(path = ".")
```

## Arguments

- path:

  Path to the project directory. Defaults to the working directory.

## Value

A `markermd_project` object.

## Examples

``` r
src = system.file("examples/test_assignment", package = "markermd")
project = fs::dir_copy(src, fs::file_temp("markermd_example_"))
init_project(project)
#> ✔ Initialized markermd project at /tmp/RtmpXDFX45/markermd_example_382363137177
#> • database: .markermd/markermd.sqlite (created)
#> • skills (claude): markermd-apply-rubric, markermd-fix-parsing,
#>   markermd-scaffold-rubric, markermd-scaffold-template (.claude/skills/)
#> • skills (codex): markermd-apply-rubric, markermd-fix-parsing,
#>   markermd-scaffold-rubric, markermd-scaffold-template (.agents/skills/)
#> • instructions: CLAUDE.md (created), AGENTS.md (created)
#> • repos: repos/
#> • comments: not found
#> • key: hw3-key
#> • artifacts: html

project_config(project)
#> 
#> ── markermd project ────────────────────────────────────────────────────────────
#> root: /tmp/RtmpXDFX45/markermd_example_382363137177
#> version: 1.0
#> created: 2026-09-30 02:41:48
#> updated: 2026-09-30 02:41:49
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
