# Rubric Item

S7 class representing a single rubric item with hotkey, points,
description and selection state

## Usage

``` r
markermd_rubric_item(
  hotkey = integer(0),
  points = integer(0),
  description = character(0),
  selected = FALSE,
  group = NA_character_
)
```

## Arguments

- hotkey:

  Integer. Hotkey number (1-10) for keyboard selection, or NA for items
  past the first ten, which have no hotkey and are toggled by clicking.
  A question may hold any number of items; the hotkey follows from an
  item's display position (1-10 for the first ten, NA beyond).

- points:

  Numeric. Point value for this rubric item

- description:

  Character. Description text for the rubric item

- selected:

  Logical. Whether this item is currently selected

- group:

  Character. Id of the
  [markermd_rubric_group](https://rundel.github.io/markermd/reference/markermd_rubric_group.md)
  the item belongs to within its question, or `NA` (the default) when
  the item is ungrouped. Group ids are an app/database detail: in a
  rubric YAML file the grouping is expressed by nesting items under a
  group entry.

## Value

A `markermd_rubric_item` S7 object.

## See also

[markermd_rubric_group](https://rundel.github.io/markermd/reference/markermd_rubric_group.md)
