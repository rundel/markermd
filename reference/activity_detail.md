# What one activity entry changed

The changes of one entry of
[`project_activity()`](https://rundel.github.io/markermd/reference/project_activity.md):
for a recorded call or an app grading session, the item selections and
the public and private comments it changed, per repository and question
(each against the state just before it); for a call that replaced the
rubric or the template, the rubric and template changes too.

## Usage

``` r
activity_detail(id, project = ".", gap = 15 * 60)
```

## Arguments

- id:

  An entry id from
  [`project_activity()`](https://rundel.github.io/markermd/reference/project_activity.md).

- project:

  Path to the project directory. Defaults to the working directory.

- gap:

  Seconds without activity that end one app grading session.

## Value

A
[markermd_snapshot_diff](https://rundel.github.io/markermd/reference/markermd_snapshot_diff.md)
object whose `rubric`, `marks` and `template` data frames hold the
changes (its score data frames are empty).

## See also

[`project_activity()`](https://rundel.github.io/markermd/reference/project_activity.md)

## Examples

``` r
if (FALSE) { # \dontrun{
latest = project_activity("hw01", app = FALSE)$id[1]
activity_detail(latest, "hw01")
} # }
```
