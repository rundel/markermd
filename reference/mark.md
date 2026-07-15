# Launch the markermd Marking Application

Points the marking app at an initialized markermd project (a directory
containing a `.markermd/` folder; see
[`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)).
The project's configuration determines where the student repositories,
grading template, grading database, and rendered HTML reports
("artifacts") are located.

## Usage

``` r
mark(path, template = NULL, use_qmd = TRUE, ...)
```

## Arguments

- path:

  Character string. Path to a markermd project directory.

- template:

  Optional template override for validation, taking precedence over the
  project's configured template. Can be:

  - Character path to a saved template (`.yaml`/`.yml`)

  - A `markermd_template` S7 object

  - NULL (use the project's configured template)

- use_qmd:

  Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE).
  Default is TRUE.

- ...:

  Additional arguments passed to shiny::runApp()

## Value

The value returned by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) when the
app exits; called for its side effect of running the marking
application.

## Examples

``` r
if (FALSE) { # \dontrun{
# Mark an initialized project
mark("/path/to/project/")

# Mark .Rmd assignments
mark("/path/to/project/", use_qmd = FALSE)

# Override the project's configured template
mark("/path/to/project/", template = "template.yaml")
} # }
```
