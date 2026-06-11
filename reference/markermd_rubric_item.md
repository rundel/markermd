# Rubric Item

S7 class representing a single rubric item with hotkey, points,
description and selection state

## Usage

``` r
markermd_rubric_item(
  hotkey = integer(0),
  points = integer(0),
  description = character(0),
  selected = FALSE
)
```

## Arguments

- hotkey:

  Integer. Hotkey number (1-10) for keyboard selection

- points:

  Numeric. Point value for this rubric item

- description:

  Character. Description text for the rubric item

- selected:

  Logical. Whether this item is currently selected
