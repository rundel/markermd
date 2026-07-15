# Package index

## Project setup and management

Initialize a grading project and inspect or edit its configuration.

- [`init_project()`](https://rundel.github.io/markermd/reference/init_project.md)
  : Initialize a markermd grading project
- [`project_config()`](https://rundel.github.io/markermd/reference/project_config.md)
  : Read a markermd project's configuration
- [`project_set()`](https://rundel.github.io/markermd/reference/project_set.md)
  : Change a markermd project's configuration
- [`project_sitrep()`](https://rundel.github.io/markermd/reference/project_sitrep.md)
  : Report a markermd project's status
- [`validate_project()`](https://rundel.github.io/markermd/reference/validate_project.md)
  : Validate a project's student repositories against its template
- [`markermd_skills_path()`](https://rundel.github.io/markermd/reference/markermd_skills_path.md)
  : Path to the bundled markermd Claude Code skills

## Grading apps

The interactive template editor and marking interface.

- [`mark()`](https://rundel.github.io/markermd/reference/mark.md) :
  Launch the markermd Marking Application
- [`template()`](https://rundel.github.io/markermd/reference/template.md)
  : Launch the markermd Template Creation Application

## Templates

Author, inspect, and move grading templates in and out of the project
database.

- [`assignment_outline()`](https://rundel.github.io/markermd/reference/assignment_outline.md)
  : Outline the gradable sections of an assignment
- [`template_import()`](https://rundel.github.io/markermd/reference/template_import.md)
  : Import a grading template from YAML into a project's database
- [`template_export()`](https://rundel.github.io/markermd/reference/template_export.md)
  : Export a project's grading template to YAML
- [`add_question()`](https://rundel.github.io/markermd/reference/add_question.md)
  : Add Question to Template
- [`remove_question()`](https://rundel.github.io/markermd/reference/remove_question.md)
  : Remove Question from Template
- [`get_question()`](https://rundel.github.io/markermd/reference/get_question.md)
  : Get Question by ID
- [`template_methods`](https://rundel.github.io/markermd/reference/template_methods.md)
  : Methods for Markermd Template Classes
- [`parse_assignment_document()`](https://rundel.github.io/markermd/reference/parse_assignment_document.md)
  : Parse an assignment document into a Pandoc AST

## Rubrics

Move per-question rubrics between the project database and YAML.

- [`rubric_import()`](https://rundel.github.io/markermd/reference/rubric_import.md)
  : Import a grading rubric from YAML into a project's database
- [`rubric_export()`](https://rundel.github.io/markermd/reference/rubric_export.md)
  : Export a project's grading rubric to YAML

## Marks and score export

Import, export, and set recorded grading, and export scores and
comments.

- [`marks_import()`](https://rundel.github.io/markermd/reference/marks_import.md)
  : Import grading marks from YAML into a project's database
- [`marks_export()`](https://rundel.github.io/markermd/reference/marks_export.md)
  : Export a project's grading marks to YAML
- [`marks_set()`](https://rundel.github.io/markermd/reference/marks_set.md)
  : Record grading marks for one repository/question pair
- [`export_scores()`](https://rundel.github.io/markermd/reference/export_scores.md)
  : Export per-repository scores to a CSV file
- [`export_comments()`](https://rundel.github.io/markermd/reference/export_comments.md)
  : Export student-facing feedback to per-repository markdown files
- [`export_marks()`](https://rundel.github.io/markermd/reference/export_marks.md)
  : Export scores and feedback for a graded project

## YAML file formats

Read, write, and schema-validate the template, rubric, and marks file
formats.

- [`read_template_yaml()`](https://rundel.github.io/markermd/reference/read_template_yaml.md)
  : Read a markermd template from a YAML file
- [`write_template_yaml()`](https://rundel.github.io/markermd/reference/write_template_yaml.md)
  : Write a markermd template to a YAML file
- [`validate_template_file()`](https://rundel.github.io/markermd/reference/validate_template_file.md)
  : Validate a template file against the markermd JSON Schema
- [`read_rubric_yaml()`](https://rundel.github.io/markermd/reference/read_rubric_yaml.md)
  : Read a grading rubric from a YAML file
- [`write_rubric_yaml()`](https://rundel.github.io/markermd/reference/write_rubric_yaml.md)
  : Write a grading rubric to a YAML file
- [`validate_rubric_file()`](https://rundel.github.io/markermd/reference/validate_rubric_file.md)
  : Validate a rubric file against the markermd JSON Schema
- [`read_marks_yaml()`](https://rundel.github.io/markermd/reference/read_marks_yaml.md)
  : Read grading marks from a YAML file
- [`write_marks_yaml()`](https://rundel.github.io/markermd/reference/write_marks_yaml.md)
  : Write grading marks to a YAML file
- [`validate_marks_file()`](https://rundel.github.io/markermd/reference/validate_marks_file.md)
  : Validate a marks file against the markermd JSON Schema

## Classes and vocabularies

S7 classes for templates, questions, rules, filters, and grading state,
plus the allowed rule vocabularies.

- [`markermd_template()`](https://rundel.github.io/markermd/reference/markermd_template.md)
  : Markermd Template
- [`markermd_question()`](https://rundel.github.io/markermd/reference/markermd_question.md)
  : Template Question
- [`markermd_node_selection()`](https://rundel.github.io/markermd/reference/markermd_node_selection.md)
  : Node Selection for Questions
- [`markermd_metadata()`](https://rundel.github.io/markermd/reference/markermd_metadata.md)
  : Template Metadata
- [`markermd_rule()`](https://rundel.github.io/markermd/reference/markermd_rule.md)
  : Markermd Validation Rule
- [`markermd_filter_condition()`](https://rundel.github.io/markermd/reference/markermd_filter_condition.md)
  : Markermd Filter Condition
- [`markermd_filter_group()`](https://rundel.github.io/markermd/reference/markermd_filter_group.md)
  : Markermd Filter Group
- [`markermd_rubric_item()`](https://rundel.github.io/markermd/reference/markermd_rubric_item.md)
  : Rubric Item
- [`markermd_grade_state()`](https://rundel.github.io/markermd/reference/markermd_grade_state.md)
  : Grade State S7 Class
- [`markermd_project()`](https://rundel.github.io/markermd/reference/markermd_project.md)
  : Markermd Project
- [`get_allowed_rule_verbs()`](https://rundel.github.io/markermd/reference/get_allowed_rule_verbs.md)
  : Get allowed verbs for rules
- [`get_allowed_node_types()`](https://rundel.github.io/markermd/reference/get_allowed_node_types.md)
  : Get allowed node types for rules
- [`get_allowed_filter_condition_types()`](https://rundel.github.io/markermd/reference/get_allowed_filter_condition_types.md)
  : Get allowed filter condition types
- [`template_classes`](https://rundel.github.io/markermd/reference/template_classes.md)
  : S7 Class Definitions for Markermd Templates
- [`project_classes`](https://rundel.github.io/markermd/reference/project_classes.md)
  : S7 Class Definition for Markermd Projects
- [`rule_classes`](https://rundel.github.io/markermd/reference/rule_classes.md)
  : S7 Class for Rule Validation
- [`rule_helpers`](https://rundel.github.io/markermd/reference/rule_helpers.md)
  : Helper Functions for Rule Validation
- [`filter_classes`](https://rundel.github.io/markermd/reference/filter_classes.md)
  : S7 Classes for Question Filters
- [`filter_helpers`](https://rundel.github.io/markermd/reference/filter_helpers.md)
  : Helper Functions for Question Filters
