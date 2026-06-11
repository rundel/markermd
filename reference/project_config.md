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
