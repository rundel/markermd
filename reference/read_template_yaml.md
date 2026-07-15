# Read a markermd template from a YAML file

Parses a template YAML file and rebuilds the `markermd_template` object,
re-parsing the referenced assignment document to regenerate its AST. All
validation is performed by the S7 constructors as objects are built.

## Usage

``` r
read_template_yaml(path, assignment = NULL, require_ast = FALSE)
```

## Arguments

- path:

  Path to a template `.yaml`/`.yml` file.

- assignment:

  Optional path to the assignment document, overriding the `source.path`
  stored in the file (useful when the template has been moved).

- require_ast:

  When `TRUE`, an assignment document that cannot be located is an error
  (the template editor needs the AST to draw its tree). When `FALSE`
  (the default), grading proceeds with an empty AST.

## Value

A `markermd_template` object.

## Examples

``` r
path = system.file(
  "examples/test_assignment/markermd-template.yaml",
  package = "markermd"
)
read_template_yaml(path)
#> Markermd template with 4 questions
#> Questions:
#>   - Q1 (1 section, 1 rule)
#>   - Q2 (1 section, 2 rules)
#>   - Q3 (1 section, 2 rules)
#>   - Q4 (1 section, 2 rules)
#> 
#> Original AST: 17 nodes 
```
