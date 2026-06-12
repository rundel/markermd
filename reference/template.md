# Launch the markermd Template Creation Application

Launch the markermd Template Creation Application

## Usage

``` r
template(
  assignment_path,
  local_dir = NULL,
  filename = "*.[Rq]md",
  assignment = NULL,
  ...
)
```

## Arguments

- assignment_path:

  Assignment source or existing template. Can be:

  - Character path to an initialized markermd project directory (one
    containing `.markermd/config.yml`; see
    [`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)).
    The template is authored against the project's key (solution)
    repository, any configured template is preloaded for editing, and
    saving writes the template into the project and records it in the
    config.

  - Character path to local directory containing assignment

  - Character GitHub repo in format "owner/repo"

  - Character path to a saved template (`.yaml`/`.yml`)

  - markermd_template S7 object

- local_dir:

  Character string. Local directory for cloning (required for remote
  GitHub repos, ignored for templates)

- filename:

  Character string. Glob pattern to match Rmd/qmd file to grade (ignored
  for templates). Default glob matches any .Rmd or .qmd file.

- assignment:

  Character string. Optional path to the assignment document, used when
  loading a template whose stored `source.path` cannot be located.

- ...:

  Additional arguments passed to shiny::runApp()

## Value

Launches Shiny application for template creation

## Examples

``` r
if (FALSE) { # \dontrun{
# Author a template for an initialized project (uses the key repo)
template("/path/to/project")

# Local assignment with default pattern
template("/path/to/assignment")

# Local assignment with specific filename
template("/path/to/assignment", filename = "homework.Rmd")

# Remote GitHub repo
template("username/repo-name", local_dir = "/tmp/grading", filename = "assignment.qmd")

# Load existing template from file
template("/path/to/saved_template.yaml")

# Load existing template from S7 object
my_template = read_template_yaml("/path/to/template.yaml")
template(my_template)
} # }
```
