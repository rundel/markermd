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
#> ✔ Initialized markermd project at /tmp/RtmpvYDUmn/markermd_example_21ca3e0f9de0
#> • database: .markermd/markermd.sqlite (created)
#> • skills: markermd-apply-rubric, markermd-scaffold-rubric,
#>   markermd-scaffold-template (.claude/skills/)
#> • repos: repos/
#> • comments: not found
#> • key: hw3-key
#> • artifacts: html

project_config(project)
#> 
#> ── markermd project ────────────────────────────────────────────────────────────
#> root: /tmp/RtmpvYDUmn/markermd_example_21ca3e0f9de0
#> version: 1.0
#> created: 2026-07-15 12:34:49
#> updated: 2026-07-15 12:34:49
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
