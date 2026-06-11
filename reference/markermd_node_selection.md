# Node Selection for Questions

S7 class representing the headings and id'd divs a template question
targets, identified by their q2r/Pandoc ids (header ids or div ids). An
empty vector means no selection (the whole document).

## Usage

``` r
markermd_node_selection(node_ids = character(0))
```

## Arguments

- node_ids:

  Character vector of document-unique node ids (header or div ids)
