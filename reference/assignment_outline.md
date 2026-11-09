# Outline the gradable sections of an assignment

Parses an assignment document and returns one row per anchorable
section: a heading or an id'd `div`. Each row records the section's
Pandoc anchor (the `id` used as a `node_id` in a template), its level
and title, and counts of the content it contains. The counts are
cumulative for the section exactly as a validation rule scoped to that
anchor would see them (via the same section-selection logic used during
grading), so they can drive starter-rule heuristics when scaffolding a
template.

## Usage

``` r
assignment_outline(
  source,
  filename = "*.[Rq]md",
  template = NULL,
  local_dir = NULL,
  assignment = NULL
)
```

## Arguments

- source:

  Anything
  [`template()`](https://rundel.github.io/markermd/reference/template.md)
  accepts: an initialized markermd project directory (outlines the key
  repository's document), a path to an assignment file (`.qmd`/`.Rmd`),
  a directory containing one, a saved template (`.yaml`/`.yml`), a
  `markermd_template` object, or a GitHub `<owner>/<repo>` (cloned into
  `local_dir`).

- filename:

  Glob used to find the assignment when `source` is a directory, a
  project or a repository. Defaults to any `.Rmd`/`.qmd` file.

- template:

  Optional `markermd_template`. When given, a heading's counts stop at
  the first heading one of the template's questions selects (other than
  the row's own), exactly as grading ends that question's section, so an
  outline taken against a template shows what each question would be
  graded on. When `NULL` (the default) the source's own template is used
  if it has one (a template object or file, or a project's stored
  template); otherwise a section runs to the next heading of the same or
  higher level.

- local_dir:

  Directory to clone into when `source` is a GitHub repository.

- assignment:

  Optional assignment document for a template `source` whose
  `source.path` cannot be located.

## Value

A data frame with one row per anchorable section and columns `type`
(`"heading"` or `"div"`), `id`, `level`, `title`, `n_subsections`, and
the content counts `n_chunk`, `n_code_block`, `n_markdown`, `n_list`,
`n_table`, `n_div`, `n_other` and `n_content` (all non-heading blocks;
HTML comments, which never render, are not counted anywhere). The
resolved source file is attached as the `"source_file"` attribute
(`NULL` for a template object whose document is unknown), and the
assignment's other files (everything else in the repository, which a
template question can cover with `file:`) as the `"files"` attribute: a
data frame with `path` (relative to the repository root: the project's
key repository, `source` when it is a directory, the cloned repository,
else the enclosing git repository when the document sits at most two
directories below it, else the document's directory; empty when the
document is unknown), `size` (bytes), `filtered` (`TRUE` for hidden
files, render output, lock files, project files and the README, which
are seldom graded) and `reason`.

## Examples

``` r
key = system.file("examples/test_assignment/hw3-key", package = "markermd")
assignment_outline(key)
#>      type                             id level                           title
#> 1 heading                   introduction     1                    Introduction
#> 2 heading     question-1-understanding-r     2     Question 1: Understanding R
#> 3 heading   question-2-basic-programming     2   Question 2: Basic Programming
#> 4 heading  question-3-data-visualization     2  Question 3: Data Visualization
#> 5 heading question-4-species-comparisons     2 Question 4: Species Comparisons
#>   n_subsections n_chunk n_code_block n_markdown n_list n_table n_div n_other
#> 1             4       3            0          9      0       0     0       0
#> 2             0       0            0          2      0       0     0       0
#> 3             0       1            0          2      0       0     0       0
#> 4             0       1            0          2      0       0     0       0
#> 5             0       1            0          2      0       0     0       0
#>   n_content
#> 1        12
#> 2         2
#> 3         3
#> 4         3
#> 5         3
```
