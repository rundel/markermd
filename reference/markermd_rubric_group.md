# Rubric Item Group

S7 class representing a named group of rubric items within a question
whose combined points are bounded together. A group has an optional
`floor` (the lowest its members' combined points can go; in deduction
mode, a cap on the total deduction) and an optional `ceiling` (the
highest they can go; in additive mode, a cap on the points earned).
Requiring `floor <= 0 <= ceiling` means a group with no selected items
always contributes exactly zero. An `exclusive` group allows at most one
of its items to be selected (severity tiers). Items reference their
group through the `group` property of
[markermd_rubric_item](https://rundel.github.io/markermd/reference/markermd_rubric_item.md).

## Usage

``` r
markermd_rubric_group(
  label = character(0),
  floor = NA_real_,
  ceiling = NA_real_,
  exclusive = FALSE
)
```

## Arguments

- label:

  Character. Group name, unique within its question. Shown to the
  grader, and to students only when
  [`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)
  is asked for group headings.

- floor:

  Numeric. Lower bound on the group's combined points (`<= 0`), or `NA`
  for none.

- ceiling:

  Numeric. Upper bound on the group's combined points (`>= 0`), or `NA`
  for none.

- exclusive:

  Logical. Whether at most one item of the group can be selected at a
  time.

## Value

A `markermd_rubric_group` S7 object.

## See also

[markermd_rubric_item](https://rundel.github.io/markermd/reference/markermd_rubric_item.md)
