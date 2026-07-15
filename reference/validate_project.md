# Validate a project's student repositories against its template

Loads the project's grading template and validates every student
repository (discovered from the project config's `repos` directory)
against the template's questions and rules. This is the same
section-based matching used during grading, run headlessly so a freshly
scaffolded template can be sanity-checked: a rule that fails across most
repositories is usually too strict and should be relaxed in
[`template()`](https://rundel.github.io/markermd/reference/template.md).

## Usage

``` r
validate_project(path = ".", template = NULL, use_qmd = TRUE)
```

## Arguments

- path:

  Path to the project directory. Defaults to the working directory.

- template:

  Optional override for the template stored in the project database: a
  path to a template `.yaml` file or a `markermd_template` object. When
  omitted, the database-stored template is used (and an error is raised
  if none has been stored).

- use_qmd:

  Logical. Match `.qmd` files (TRUE) or `.Rmd` files (FALSE).

## Value

A data frame with one row per repository/question: columns `repo`,
`question`, `status` (`"pass"`, `"fail"`, or `"error"`), and `detail`. A
repo whose assignment cannot be found or parsed yields a single
`"error"` row.

## Details

Each repository's assignment file is matched by the basename recorded in
the template's `source.path` (falling back to the first matching
document), so the template's heading/div anchors line up with the
student documents.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check every student repository against the stored template
validate_project("hw01")

# Validate against a template YAML instead of the stored one
validate_project("hw01", template = "hw01-template.yaml")
} # }
```
