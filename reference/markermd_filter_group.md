# Markermd Filter Group

S7 class representing a group of filter conditions combined with logical
AND. A question's filter groups are combined with logical OR. An empty
group is allowed as transient editing state and is ignored during
evaluation and serialization.

## Usage

``` r
markermd_filter_group(conditions = list(), negate = FALSE)
```

## Arguments

- conditions:

  List of markermd_filter_condition objects

- negate:

  Logical. When TRUE the whole group's predicate is negated (logical NOT
  around the ANDed conditions).

## Value

A `markermd_filter_group` S7 object.
