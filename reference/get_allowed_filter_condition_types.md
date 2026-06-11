# Get allowed filter condition types

Returns the list of valid condition types that can be used in question
filters. Each type corresponds to a q2r predicate: "node type" becomes
an `is(<pandoc class>)` test while the others map directly onto the q2r
mask helpers of the same name.

## Usage

``` r
get_allowed_filter_condition_types()
```

## Value

Character vector of allowed filter condition types
