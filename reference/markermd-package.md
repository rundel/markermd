# markermd: Interactive Shiny-Based Grading Interface for Quarto and R Markdown Assignments

Provides a Shiny-based workflow for grading assignments submitted as git
repositories containing Quarto or R Markdown documents. A grading
template of questions and structural validation rules is authored
interactively against the solution (key) repository, and student
submissions are then validated against it and graded question by
question with per-question rubrics, keyboard hotkeys, and
rendered-report previews. All grading state is stored in a per-project
SQLite database, with YAML import and export for templates, rubrics, and
marks, plus bundled Claude Code skills that scaffold templates and
rubrics and record first-pass machine grading for human review.

## See also

Useful links:

- <https://rundel.github.io/markermd/>

- <https://github.com/rundel/markermd>

- Report bugs at <https://github.com/rundel/markermd/issues>

## Author

**Maintainer**: Colin Rundel <rundel@gmail.com>

Authors:

- Colin Rundel <rundel@gmail.com>
