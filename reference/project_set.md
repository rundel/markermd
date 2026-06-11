# Change a markermd project's configuration

Updates path entries in a project's `config.yml`. Only supplied
arguments are changed; an omitted (`NULL`) argument is left untouched.
Paths are interpreted relative to the project root.

## Usage

``` r
project_set(
  path = ".",
  repos = NULL,
  comments = NULL,
  template = NULL,
  key = NULL
)
```

## Arguments

- path:

  Path to the project directory. Defaults to the working directory.

- repos:

  Root-relative repos directory to record. Warns if it does not exist.

- comments:

  Root-relative comments directory to record. Warns if it does not
  exist.

- template:

  A grading template to store in the project database: a
  `markermd_template` object or a path to a template YAML file (which is
  parsed and imported, so an invalid file errors immediately). Pass `NA`
  to clear the stored template. See also
  [`template_import()`](https://rundel.github.io/markermd/reference/template_import.md).

- key:

  Root-relative key (solution) repository directory to record. Warns if
  it does not exist. Pass `NA` to clear a previously configured key.

## Value

The updated `markermd_project` object, invisibly.
