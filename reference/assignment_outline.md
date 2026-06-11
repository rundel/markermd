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
assignment_outline(assignment_path, filename = "*.[Rq]md")
```

## Arguments

- assignment_path:

  Path to an assignment file (`.qmd`/`.Rmd`) or a directory containing
  one.

- filename:

  Glob used to find the assignment when `assignment_path` is a
  directory. Defaults to any `.Rmd`/`.qmd` file.

## Value

A data frame with one row per anchorable section and columns `type`
(`"heading"` or `"div"`), `id`, `level`, `title`, `n_subsections`, and
the content counts `n_chunk`, `n_code_block`, `n_markdown`, `n_list`,
`n_table`, `n_div`, `n_other` and `n_content` (all non-heading blocks).
The resolved source file is attached as the `"source_file"` attribute.
