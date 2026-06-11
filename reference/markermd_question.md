# Template Question

S7 class representing a single template question with selected nodes and
validation rules

## Usage

``` r
markermd_question(
  id = integer(0),
  name = character(0),
  selected_nodes = markermd_node_selection(),
  rules = list(),
  points = 10,
  filters = list()
)
```

## Arguments

- id:

  Integer. Unique question identifier

- name:

  Character. Question display name

- selected_nodes:

  node_selection. Selected AST nodes for this question

- rules:

  List. Validation rules for this question as markermd_rule objects

- points:

  Numeric. Point value for this question (default 10)

- filters:

  List. Filter groups for this question as markermd_filter_group
  objects. Conditions within a group are ANDed, groups are ORed; filters
  narrow the question's node set before rules evaluate.
