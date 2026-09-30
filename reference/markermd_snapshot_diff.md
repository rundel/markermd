# Differences between two snapshots

S7 class returned by
[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md):
the rubric, marks, score and template changes between two snapshots, or
between a snapshot and the live state. Its print method groups the
changes by question.

## Usage

``` r
markermd_snapshot_diff(
  from = character(0),
  to = character(0),
  from_label = character(0),
  to_label = character(0),
  rubric = (function (.data = list(), row.names = NULL) 
 {
     if (is.null(row.names))
    {
         list2DF(.data)
     }
     else {
         out <- list2DF(.data,
    length(row.names))
attr(out, "row.names") <- row.names
         out
     }

    })(),
  marks = (function (.data = list(), row.names = NULL) 
 {
     if (is.null(row.names))
    {
         list2DF(.data)
     }
     else {
         out <- list2DF(.data,
    length(row.names))
attr(out, "row.names") <- row.names
         out
     }

    })(),
  scores = (function (.data = list(), row.names = NULL) 
 {
     if (is.null(row.names))
    {
         list2DF(.data)
     }
     else {
         out <- list2DF(.data,
    length(row.names))
attr(out, "row.names") <- row.names
         out
     }

    })(),
  totals = (function (.data = list(), row.names = NULL) 
 {
     if (is.null(row.names))
    {
         list2DF(.data)
     }
     else {
         out <- list2DF(.data,
    length(row.names))
attr(out, "row.names") <- row.names
         out
     }

    })(),
  template = (function (.data = list(), row.names = NULL) 
 {
     if
    (is.null(row.names)) {
         list2DF(.data)
     }
     else {
         out <-
    list2DF(.data, length(row.names))
attr(out, "row.names") <- row.names
      
      out
     }
 })()
)
```

## Arguments

- from:

  Character. The "from" side's snapshot id.

- to:

  Character. The "to" side's snapshot id, or `"current"`.

- from_label:

  Character. A short description of the "from" side.

- to_label:

  Character. A short description of the "to" side.

- rubric:

  Data frame of rubric changes (`question`, `change`, `item`, `detail`):
  items added, removed, reworded, re-pointed or regrouped, group and
  scoring changes, questions added or removed.

- marks:

  Data frame of marks changes (`repo`, `question`, `change`, `before`,
  `after`): pairs whose selections, public comment or private note
  changed.

- scores:

  Data frame of score changes per pair (`repo`, `question`, `before`,
  `after`, `change`).

- totals:

  Data frame of every repository's total (`repo`, `before`, `after`,
  `change`).

- template:

  Data frame of template changes (`question`, `change`, `detail`).

## Value

A `markermd_snapshot_diff` S7 object.

## See also

[`snapshot_diff()`](https://rundel.github.io/markermd/reference/snapshot_diff.md)
