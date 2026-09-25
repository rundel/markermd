# markermd 0.1.0

* Initial version.
* Added `parse_project()`, a headless per-repository parse check, and the bundled `markermd-fix-parsing` skill that repairs submissions the strict q2r parser rejects on a `grading-fixes` branch.
* The bundled skills now ship in two sets, for Claude Code (`inst/skills/claude/`, installed to `.claude/skills/`) and Codex (`inst/skills/codex/`, installed to `.agents/skills/`); `init_project()` gained an `agents` argument (default: both) and `markermd_skills_path()` an `agent` argument.
* Raw blocks that are nothing but `<!-- ... -->` comments are now their own node kind, `HTML comment`, so rules and filters distinguish `{=html}` write-ups (still `Raw Block`) from skeleton placeholders and repair markers, and `assignment_outline()` no longer counts comments as content.
