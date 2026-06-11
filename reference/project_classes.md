# S7 Class Definition for Markermd Projects

S7 class representing a markermd grading project: a directory (typically
produced by
[`ghclass::org_grade_assignment()`](https://rdrr.io/pkg/ghclass/man/org_grade_assignment.html))
whose markermd state lives under a `.markermd/` directory. The object
records the locations of the project's pieces relative to its root so it
stays portable.
