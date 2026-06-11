# Template Metadata

S7 class representing template creation and modification metadata

## Usage

``` r
markermd_metadata(
  created_at = Sys.time(),
  created_by = Sys.getenv("USER", "unknown"),
  total_nodes = 0L,
  version = markermd_template_version()
)
```

## Arguments

- created_at:

  POSIXct. Template creation timestamp

- created_by:

  Character. User who created the template

- total_nodes:

  Integer. Total number of nodes in original AST

- version:

  Character. Template format version
