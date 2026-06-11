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
