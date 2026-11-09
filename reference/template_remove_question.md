# Remove a question from a template

Remove a question from a template

## Usage

``` r
template_remove_question(template, id)
```

## Arguments

- template:

  A `markermd_template` object.

- id:

  Integer question ID to remove.

## Value

The `markermd_template` object without the matching question (returned
unchanged, with a warning, when `id` is not found).
