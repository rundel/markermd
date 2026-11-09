# Check that every student repository's assignment document parses

Locates each student repository's assignment document (discovered from
the project config's `repos` directory and matched by the stored
template's `source.path` basename when a template exists) and parses it
with
[`parse_assignment_document()`](https://rundel.github.io/markermd/reference/parse_assignment_document.md),
reporting the outcome per repository. The q2r parser is strict, so small
markdown slips in a submission (a stray `$` or `{`, an unclosed quote,
emphasis mark, or bracket, malformed YAML front matter) fail the parse
and leave the repository ungradable until repaired. This headless check
finds them and returns the parser's diagnostic verbatim, which is what
the bundled `markermd-fix-parsing` skill works from; unlike
[`validate_project()`](https://rundel.github.io/markermd/reference/validate_project.md)
it needs no stored template.

## Usage

``` r
parse_project(project = ".", use_qmd = TRUE)
```

## Arguments

- project:

  Path to an initialized markermd project directory.

- use_qmd:

  Logical. Match `.qmd` files (`TRUE`, the default) or `.Rmd` files
  (`FALSE`).

## Value

A data frame with one row per repository and columns `repo`, `file`
(path of the document that was parsed, `NA` when none was found),
`status` (`"ok"`, `"warning"`, `"error"`, or `"missing"`), and `detail`
(the parser's diagnostics with ANSI styling stripped, or `NA` when there
are none).

## Examples

``` r
if (FALSE) { # \dontrun{
res = parse_project("hw01")
res[res$status != "ok", ]
} # }
```
