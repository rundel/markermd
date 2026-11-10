# Reword or re-point several rubric items at once

The bulk form of
[`rubric_item_set()`](https://rundel.github.io/markermd/reference/rubric_item_set.md):
applies a table of description and points changes in one transaction,
with one automatic snapshot and one activity log entry, so a wording
pass over a whole rubric is one recorded change. Every item keeps its
id, position, hotkey and group, so recorded selections carry over.

## Usage

``` r
rubric_items_set(changes, project = ".", snapshot = TRUE, summary = NULL)
```

## Arguments

- changes:

  A data frame with the columns `question` and `item` (the item's
  current description, matched verbatim) plus `description` (the new
  description), `points` (the new points) or both, where `NA` keeps the
  current value; or the path to a YAML file of such entries.

- project:

  Path to the project directory. Defaults to the working directory.

- snapshot:

  Whether to take an automatic snapshot before the change.

- summary:

  Optional text recorded on the automatic snapshot and in the activity
  log: what the change is and why.

## Value

Invisibly, a data frame of the changes made, with the columns
`question`, `item` (the previous description), `description` and
`points` (the values now stored).

## Details

Every row is checked before anything is written: the question must be in
the template, `item` must match exactly one stored item of it, an item
may be listed only once, and after all the changes no question may hold
two items with the same description (so two items can swap wording in
one call). Rows that change nothing are skipped. If any row fails,
nothing is written.

As a YAML file, `changes` is a list of entries:

    - question: Q1
      item: Hotel-link discovery omits usable hotel pages
      description: Some hotel pages are missed when finding hotel links
    - question: Q2
      item: Plot is missing axis labels
      points: -0.5

## See also

[`rubric_item_set()`](https://rundel.github.io/markermd/reference/rubric_item_set.md),
[`rubric_item_delete()`](https://rundel.github.io/markermd/reference/rubric_item_delete.md),
[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rubric_items_set(
  data.frame(
    question = c("Q1", "Q2"),
    item = c("Calculation error", "No write up"),
    description = c("The standard error divides by n", "The write-up is missing")
  ),
  project = "hw03",
  summary = "Wording review: plainer descriptions"
)
} # }
```
