# Scan a project's submissions for prompt injection attempts

Looks through every student repository and rendered report for text
aimed at an automated grader rather than at the instructor: instructions
to ignore the rubric or award full marks, text addressed to an AI model,
chat role markers, fabricated grading output, and above all text a human
reader would not see (HTML comments, elements hidden by style or class,
invisible Unicode characters, code in chunks that are not shown, base64,
attribute and meta text in the rendered HTML, look-alike letters from
other scripts). In rendered HTML it also reports active content: scripts
that reach out of the report's frame or carry injection text, scripts
loaded from an unusual host, event-handler attributes and outside
frames. It also reports agent instruction files (`CLAUDE.md`,
`AGENTS.md`, `.cursorrules`, ...) shipped in a repository and
`<!-- markermd: ... -->` repair markers that were not added by the
bundled `markermd-fix-parsing` skill.

## Usage

``` r
scan_project(project = ".", artifacts = TRUE)
```

## Arguments

- project:

  Path to an initialized markermd project directory.

- artifacts:

  Logical. Also scan the rendered HTML reports found under the project's
  configured artifacts directories (`TRUE`, the default).

## Value

A data frame with one row per finding, ordered by repository and
severity, with columns `repo`, `source` (`"repo"` or `"artifact"`),
`file` (relative to the project root), `line` (`NA` for rendered HTML
and file-level findings), `severity`, `detector`, `matched` and
`excerpt`.

`detector` says where the text sits: `visible-text`, `html-comment`,
`hidden-style`, `hidden-chunk`, `hidden-element`, `html-attribute`,
`invisible-unicode`, `bidi-control`, `look-alike-text`, `encoded-text`,
`active-content`, `markermd-marker`, `agent-file` or `file-name`.
`matched` lists the content patterns it hit (`instruction-override`,
`grader-addressed`, `grade-demand`, `role-marker`, `grading-output`,
`score-line`, `rubric-text`), or `NA`. `severity` is `"high"` for a
content match in a concealed place (and for smuggled Unicode, look-alike
spelling and scripts that reach out of the report), `"medium"` for
visible text matching two different content categories, an agent file,
an event handler, a script from an unusual host or an unverified repair
marker, and `"low"` for visible text matching one category, concealment
with no content match, or an outside frame. `excerpt` is a single line,
truncated, with invisible characters shown as `<U+XXXX>`.

## Details

The scan is deterministic pattern matching and reads no file as
instructions. It is meant to catch obvious attempts and to give a human
(or the bundled `markermd-check-injection` skill) a short list to judge;
it changes nothing in the project, and a finding is a candidate, not a
verdict. Text inside images, persuasion written as ordinary prose, and
heavier obfuscation are not detected.

Findings that also occur in the key repository are dropped, as are low
and medium findings shared by more than half of the repositories (when
there are at least four), since both are starter text rather than a
student's doing.

## Examples

``` r
if (FALSE) { # \dontrun{
res = scan_project("hw01")
res[res$severity == "high", ]
} # }
```
