# Launch the markermd Marking Application

Points the marking app at an initialized markermd project (a directory
containing a `.markermd/` folder; see
[`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)).
The project's configuration determines where the student repositories,
grading template, grading database, and rendered HTML reports
("artifacts") are located.

## Usage

``` r
mark(project, template = NULL, use_qmd = TRUE, terminal = FALSE, ...)
```

## Arguments

- project:

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

- terminal:

  Whether to add a Terminal tab that runs a shell or an agent CLI
  (`claude`, `codex`) in the project root, using the optional
  [shinyterm](https://github.com/rundel/shinyterm) package. One of:

  - `FALSE` (the default): no Terminal tab

  - `TRUE`: offer the user's shell plus `claude` and `codex` when they
    are on the `PATH`, each agent with its own sandbox confined to the
    project (see
    [`shinyterm::default_commands()`](https://rdrr.io/pkg/shinyterm/man/default_commands.html))

  - A named list of commands, as accepted by
    [`shinyterm::terminal_tabs_server()`](https://rdrr.io/pkg/shinyterm/man/terminal_tabs_ui.html),
    e.g.
    `list(Claude = shinyterm::claude_command(wd = project, permission_mode = "acceptEdits"))`

  A terminal runs as you, with your files and credentials, so only use
  it on your own machine; shinyterm refuses to run on a hosted server.
  With the tab on, the app is protected by a random access token: the
  browser opens at a URL ending in `?shinyterm_token=...` (also printed
  when the app starts), and a page opened without it gets no terminal
  process.

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

# Add a Terminal tab for a shell, Claude Code or Codex
mark("/path/to/project/", terminal = TRUE)
} # }
```
