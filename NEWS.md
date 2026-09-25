# markermd 0.1.0

* Initial version.
* Added `parse_project()`, a headless per-repository parse check, and the bundled `markermd-fix-parsing` skill that repairs submissions the strict q2r parser rejects on a `grading-fixes` branch.
* The bundled skills now ship in two sets, for Claude Code (`inst/skills/claude/`, installed to `.claude/skills/`) and Codex (`inst/skills/codex/`, installed to `.agents/skills/`); `init_project()` gained an `agents` argument (default: both) and `markermd_skills_path()` an `agent` argument.
* Rubric items are no longer effectively limited to ten per question. The first ten keep the hotkeys 1-9 and 0; further items are toggled by clicking (their button is blank with a dashed outline). Display order is now stored explicitly in the grading database (`items.position`, schema version 2, migrated in place the first time a project is opened), so reordering items past the tenth survives a restart and export; previously those items always reloaded in insertion order.
* Raw blocks that are nothing but `<!-- ... -->` comments are now their own node kind, `HTML comment`, so rules and filters distinguish `{=html}` write-ups (still `Raw Block`) from skeleton placeholders and repair markers, and `assignment_outline()` no longer counts comments as content.
