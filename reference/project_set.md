# Change a markermd project's configuration

Updates path entries in a project's `config.yml`. Only supplied
arguments are changed; an omitted (`NULL`) argument is left untouched.
Paths are interpreted relative to the project root.

## Usage

``` r
project_set(
  project = ".",
  repos = NULL,
  comments = NULL,
  template = NULL,
  key = NULL,
  artifacts = NULL
)
```

## Arguments

- project:

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

- artifacts:

  Character vector of root-relative directories holding the students'
  rendered reports, replacing the recorded list. Warns for any that does
  not exist. Pass `NA` or `character(0)` to record none.

## Value

The updated `markermd_project` object, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
# Record the key repository and store a template from YAML
project_set("hw01", key = "hw01-key")
project_set("hw01", template = "hw01-template.yaml")

# Record the directories of rendered reports
project_set("hw01", artifacts = c("html", "md"))
} # }
```
