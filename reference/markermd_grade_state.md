# Grade State S7 Class

Represents the grading state for a question including current score,
total possible points, and grading configuration options.

## Usage

``` r
markermd_grade_state(
  current_score = 0,
  total_score = 0,
  grading_mode = "positive",
  bound_above_zero = TRUE,
  bound_below_max = TRUE
)
```

## Arguments

- current_score:

  Numeric. Current points awarded

- total_score:

  Numeric. Maximum possible points

- grading_mode:

  Character. Either "positive" or "negative" grading

- bound_above_zero:

  Logical. Whether to enforce score \>= 0

- bound_below_max:

  Logical. Whether to enforce score \<= maximum

## Value

A `markermd_grade_state` S7 object.
