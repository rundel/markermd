# Create a new markermd filter group

Convenience function to create a new filter group. By default the group
contains a single default condition.

## Usage

``` r
new_markermd_filter_group(conditions = NULL, negate = FALSE)
```

## Arguments

- conditions:

  List of markermd_filter_condition objects (if NULL, a single default
  condition is used)

- negate:

  Logical. Whether the group is negated (defaults to FALSE)

## Value

markermd_filter_group object
