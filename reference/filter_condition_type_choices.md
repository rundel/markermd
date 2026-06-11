# Display choices for the filter condition-type select

Returns the get_allowed_filter_condition_types() values with display
labels annotating each type's matching semantics (exact, regex, glob, or
the option mini-syntax), so the semantics stay visible after a value has
been typed.

## Usage

``` r
filter_condition_type_choices()
```

## Value

Named character vector suitable for shiny select choices
