# Markermd Project

S7 class describing an initialized markermd project. Path properties are
stored relative to `root` (resolved to absolute paths at read time);
`root` itself is the absolute project directory and is not serialized to
the config file. Timestamps are stored as strings (matching the database
layer's `get_current_timestamp()` format) so they round-trip through
YAML without timezone/format fragility.

## Usage

``` r
markermd_project(
  root = character(0),
  repos = NA_character_,
  comments = NA_character_,
  database = ".markermd/markermd.sqlite",
  key = NA_character_,
  artifacts = character(0),
  created_at = get_current_timestamp(),
  updated_at = get_current_timestamp(),
  version = markermd_project_version()
)
```

## Arguments

- root:

  Character. Absolute, normalized project root (parent of `.markermd/`).

- repos:

  Character. Root-relative repos directory, or `NA` if absent.

- comments:

  Character. Root-relative comments directory, or `NA` if absent.

- database:

  Character. Root-relative path to the SQLite grading database. The
  grading template is stored inside this database, not in the config.

- key:

  Character. Root-relative key (solution) repository directory, or `NA`
  if unset.

- artifacts:

  Character vector. Root-relative artifact directory names.

- created_at:

  Character. Project creation timestamp.

- updated_at:

  Character. Last config-update timestamp.

- version:

  Character. Config format version.
