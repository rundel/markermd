---
name: markermd-fix-parsing
description: Repair student repositories whose assignment documents fail markermd's strict q2r (Quarto) parse: finds them with parse_project(), makes the smallest edit that makes each document parseable, commits on a grading-fixes branch so the original submission stays intact, and asks before any substantive change. Use when asked to fix or resolve parse errors or unparseable submissions in a markermd project.
---

# Repair parse errors in student repositories

## Overview

markermd parses every assignment document with the q2r package, a strict
Quarto parser. Small markdown slips that Quarto 1.x and pandoc tolerate (a
bare `$` in prose, a quoted `"*"`, an apostrophe before a space, a word like
`_word_score`) fail the parse outright, and a repository whose document does
not parse cannot be validated against the template or graded in `mark()`. In
practice most of a class can be affected: on one real assignment 8 of 10
submissions (and the key) failed, every one on a one or two character
escaping issue in prose. This skill makes those documents parseable with the
smallest possible edits, without changing the student's work, and keeps every
edit on a separate git branch so the original submission is preserved.

It works on an **initialized markermd project** (`markermd::init_project()`,
which sets up a `ghclass::org_grade_assignment()` layout) and needs
`paths.repos` (and, to check the key, `paths.key`) from
`<root>/.markermd/config.yml`. It is typically run right after the
`markermd-scaffold-template` skill, whose validation step reports the
repositories that failed to parse, but it needs no stored template.

The flow is: read the config -> check the key parses -> find the failing
repositories -> for each one, read the diagnostic, prepare the
`grading-fixes` branch, make the smallest fix, re-parse until clean, commit
-> verify across the project -> report.

Hard rules:

- **Change nothing beyond what parsing requires.** The goal is a parseable
  document, not a better one. Never fix the student's code, prose, spelling,
  or formatting (a typo that causes the error is escaped, not corrected),
  never edit heading text or levels so template anchors match (that is the
  rubric's business), and never touch anything the parser did not complain
  about. Only prose lines are ever edited: text inside a fenced chunk cannot
  cause a parse error and is left alone even when it contains the same
  pattern.
- **Every edit lives on a `grading-fixes` branch** in the student's
  repository, created from the checked-out commit and left checked out
  (`mark()` and `validate_project()` read the working tree, so grading has to
  happen with `grading-fixes` checked out; switching back to the original
  branch reintroduces the parse error). The original branch is never
  modified and nothing is ever pushed.
- **Minimal fixes** (the escape-or-close edits in step 5) proceed without
  asking. **Substantive fixes** (anything that removes, rewrites, moves, or
  adds content, including moving a chunk fence) need the user's explicit
  permission, per change, and are flagged in the document with a
  `<!-- markermd: ... -->` comment saying the change is an automated grading
  fix and not part of the student's work.
- Never edit a repository with uncommitted changes, and never stash, reset,
  or discard anything.
- The key (solution) repository is the instructor's document: it gets no
  branch, and it is edited only with the user's permission (step 2).

This skill requires the `markermd` R package to be installed. If the commands
below fail with a package-not-found error, ask the user to install it first.

## Steps

### 1. Confirm the project is initialized and read its config

The user provides the project directory (the `init_project()` root). Read
`<root>/.markermd/config.yml`. If it does not exist, the directory is not a
markermd project: tell the user to run `markermd::init_project("<root>")` first,
then stop. For a human-readable overview you can also run:

```
Rscript -e 'markermd::project_sitrep("<root>")'
```

This skill needs `paths.repos` (the student repositories). If it is
missing/null, ask the user where the student repositories live relative to the
project root and record it:

```
Rscript -e 'markermd::project_set("<root>", repos = "<repos-dir>")'
```

### 2. Check that the key parses

A key that does not parse blocks every later step: the template's
`source.path` points at it and it is re-parsed whenever the template is built
or loaded. If `paths.key` is set, run:

```
Rscript -e 'invisible(markermd::assignment_outline("<root>/<paths.key>")); cat("key OK\n")'
```

If it aborts with a parser diagnostic (the same format as step 3), the key
itself is at fault, often in instructor-only material such as grading notes
(a real example: `herds' moves`, an apostrophe before a space, `[Q-2-10]`).
Show the user the diagnostic and the minimal fix from step 5's table, and ask
before editing: the key is the instructor's own repository, so it gets no
`grading-fixes` branch and whether to commit the edit is their call. Report
the edit (and that it is uncommitted) in step 8. If `paths.key` is not set,
skip this step and say so.

### 3. Find the repositories that fail to parse

Run the headless parse check. It locates each repository's assignment document
(matched by the stored template's `source.path` basename when a template
exists, otherwise the repository's `.qmd` file) and parses it exactly as
grading would:

```
Rscript -e 'res <- markermd::parse_project("<root>"); print(res[, c("repo", "status")], row.names = FALSE); for (i in which(res$status != "ok")) cat("\n==", res$repo[i], "[", res$status[i], "]", res$file[i], "\n", res$detail[i], "\n")'
```

It returns one row per repository with `repo`, `file` (the document that was
parsed), `status`, and `detail` (the parser's diagnostic verbatim):

- `ok`: nothing to do.
- `warning`: the document parsed; the diagnostics are informational (most
  often `Q-2-9`, an HTML element passed through as raw HTML). Nothing to do,
  and do not "fix" warnings.
- `error`: the document did not parse. These repositories are this skill's
  work.
- `missing`: no `.qmd` document anywhere in the repository. Nothing to
  repair; list them in the report so the user can chase the submission.

If the user arrived here from `validate_project()` or `mark()`, their `error`
rows are the `error` and `missing` rows here. Parse errors are never template
problems: do not try to fix them by changing the template.

**Reading a diagnostic.** It carries a bracketed code and title
(`[Q-2-12] Unclosed Star Emphasis`) or the generic `Parse error ... unexpected
character or token here`, followed by a source excerpt with a caret under the
reported position. Three pitfalls:

- **Line numbers are offset.** They refer to the text after markermd's knitr
  chunk-header normalization, which rewrites `{r name}` and
  `{r name, echo=FALSE}` headers into `{r}` plus `#| label:` / option lines.
  The reported line exceeds the file line by the number of such chunks above
  it (17 in one real key). Locate the problem by grepping for the quoted
  text, never by line number, and verify a line with `sed -n '<n>p'` before
  editing it.
- **The column can point at the end of the line.** For an unclosed construct
  (a `$` opening inline math, a `_` opening emphasis, a quote) the error is
  reported where the paragraph ends, not where the offending character is.
  Search the quoted line for `$`, `_`, `*`, and quote marks.
- **Only the first fatal error is reported**, and the AST comes back empty.
  A document often has several problems, so expect to fix, re-parse, and
  repeat (one real repository needed four rounds).

To shorten the rounds, pre-scan a document's prose for the characters that
cause nearly all failures. This tracks fence state so chunks are skipped, and
prints each hit with its file line and surrounding text:

```
perl -CSD -ne 'BEGIN{$in=0} if (/^```/) {$in=!$in; next} next if $in;
  while (/(\$|"\*"|\*{3,}|(?:^|\s)_[A-Za-z0-9]|[A-Za-z]\x27\s)/g) { print "$.: ", substr($_, pos($_)-45 < 0 ? 0 : pos($_)-45, 90) }' <file>
```

Treat the output as a shortlist, not a fix list: many hits are benign (a
balanced `'v'`, `.Machine$integer.max` inside backticks, `_repr_` as valid
emphasis). Only what the parser actually rejects gets edited.

Do not edit anything yet: work through steps 4-6 one repository at a time.

### 4. Prepare the repository's `grading-fixes` branch

Before touching a repository, check its git state:

```
git -C <repo> status --porcelain
git -C <repo> branch --show-current
```

- **Not a git repository** (`git status` fails): there is no branch to keep
  the original on. Report it and ask; skip by default, and only edit in place
  if the user explicitly accepts, after copying the untouched document to
  `<file>.orig` alongside it.
- **Uncommitted changes** (non-empty porcelain output): skip the repository,
  report it, and ask the user how to proceed. Never stash or discard.
- **Record the original branch** (usually `main`; on a detached HEAD record
  the commit from `git -C <repo> rev-parse --short HEAD`). The report and the
  marker comments refer to it.
- **`grading-fixes` already exists** (`git -C <repo> show-ref --verify --quiet
  refs/heads/grading-fixes` succeeds): a previous run repaired this
  repository. Check it out (`git -C <repo> checkout grading-fixes`) and
  continue on it. If the original branch has moved on since (`git -C <repo>
  log --oneline grading-fixes..<original-branch>` prints commits), the
  submission changed after the repairs; ask the user before proceeding (they
  may prefer to delete the branch and repair the new submission from scratch).
- Otherwise create it from the checked-out commit:

  ```
  git -C <repo> checkout -b grading-fixes
  ```

### 5. Make the smallest fix

Read the document around the diagnostic and identify what the parser tripped
on. Then choose the smallest edit, in one of two tiers.

**Minimal fixes** are made without asking. They leave the student's visible
content unchanged (every backslash escape below renders as the literal
character under pandoc, so the rendered document is identical) or preserve it
unambiguously:

| Diagnostic | Typical cause in prose | Fix |
|---|---|---|
| Parse error at or after a `$`, or `Q-2-23` Unclosed Inline Math | an R accessor outside backticks (`.Machine$integer.max`, `py$x`), a dollar amount | `\$` |
| `Q-2-11` Unclosed Double Quote where the quoted text is a star | `"*"`: the `*` opens emphasis and the closing quote is lost | escape the star, not the quote: `"\*"` |
| `Q-2-10` Closed Quote Without Matching Open Quote | an apostrophe before a space (`herds' moves`), or a typo such as `'.''` | `herds\' moves`, `'.'\'` (the typo stays) |
| `Q-2-7`, `Q-2-11` unclosed quote, or a parse error inside a quoted span | a double-quoted span containing double quotes (`" if (x == "a") "`) | escape the outer quotes: `\"` |
| Parse error at `_`, or `Q-2-5` / `Q-2-14` unclosed emphasis | a word with a leading and an intraword `_` (`_word_score`) | escape every underscore in the word: `\_word\_score` |
| Parse error at a run of stars, or `Q-2-12` / `Q-2-13` | `*****` as a separator, a lone `*` (`p*q`, `5 * 3`) | escape every star: `\*` |
| `Q-2-32` triple star | `***text***` | `**_text_**` (renders the same) |
| Parse error at `{` or `}`, `Q-2-41`, `Q-2-50`, `Q-2-38`, `Q-2-2` | literal braces in prose, a malformed `{.class key=val}` | `\{`, `\}`; fix a specifier only when the intent is evident |
| `Q-2-24` unclosed code span, or a parse error at a backtick | a stray backtick | `` \` `` |
| `Q-2-1` Unclosed Span | a lone `[` | `\[` |
| `Q-2-16`, `Q-2-17`, `Q-2-18` | `^` or `~` used literally (`x^2`, `y ~ x`) | `\^`, `\~` |
| `Q-2-45`, `Q-2-46` reference-style links | `[text][ref]` plus `[ref]: url` | inline the link: `[text](url)`; escape the brackets when no definition exists |
| `Q-0-99` YAML front matter | wrong indentation, an unquoted `:` in a title | the one evident repair: fix the indentation or quote the value |
| Invalid UTF-8 (a `readLines` / encoding error) | a file saved in another encoding | re-encode the file to UTF-8 (`iconv`); the text itself is unchanged |
| No diagnostic, but the document is truncated (step 6) | an unclosed code fence or `:::` div | add the closing fence where the code or div evidently ends |

Two diagnostics need **no edit at all**: `Q-2-36` (knitr-style chunk options
such as `{r label, echo=FALSE}`), which markermd rewrites itself before
parsing, and `Q-2-6` / `Q-2-9` (HTML elements), which are warnings. Leave
both alone.

Prefer escaping a lone mark over inventing its closing partner: the escape
keeps the student's text exactly as written and is invisible when rendered,
whereas an invented closer can silently italicize or quote a run of text.
Insert a closer only when its place is unmistakable (a quote that plainly ends
with the sentence, a code fence whose code plainly ends at the blank line
before the next paragraph or heading).

Make each edit as an exact-string replacement that refuses to run unless the
old text occurs exactly the expected number of times (an editor tool that
rejects a non-unique match does this). From the shell, use a guard like this
one, which passes the strings through environment variables so `$`, `\`, and
quotes need no shell or perl escaping:

```
# usage: fix.sh <file> <old> <new> [expected_line_matches]
f="$1"; old="$2"; new="$3"; exp="${4:-1}"
n=$(grep -cF -- "$old" "$f")
if [ "$n" -ne "$exp" ]; then echo "REFUSING: '$old' matches $n lines (expected $exp)"; exit 1; fi
OLD="$old" NEW="$new" perl -pi -e 's/\Q$ENV{OLD}\E/$ENV{NEW}/g' "$f"
```

The guard matters: on one real repository the offending phrase also appeared
inside a chunk, where it was harmless and had to stay untouched.

**Substantive fixes** need permission and a marker. They are any edit that
removes, rewrites, moves, or adds content: moving a chunk fence, deleting a
broken line, table, or chunk; rewriting a malformed table (`Q-2-39` grid
tables); untangling nested or mis-indented fences; changing YAML front matter
beyond a one-token repair; and any case where the intended text is not
evident. For each such change, show the user the diagnostic, the exact lines
involved, and the proposed edit, and ask. If they decline, leave the
repository unrepaired (revert any partial edit with
`git -C <repo> checkout -- <file>`) and note it in the report. If they
approve, apply the edit and put a marker comment on its own line, with a
blank line on each side, immediately before the changed spot:

```
<!-- markermd: automated grading fix, not part of the students' work. Moved this chunk's closing fence from after the skeleton stub to after the fizzbuzz definition, which was otherwise outside the chunk; the original is on the main branch. -->
```

Marker conventions: it starts with `markermd: automated grading fix, not part
of the students' work.`, then states what changed, why (the diagnostic code
when there is one), and where the original is (the original branch from step
4). The comment becomes an `HTML comment` node in the document's section,
which no `Markdown`, `Raw Block`, or `Chunk` rule counts, and the blank lines
keep the following paragraph a normal paragraph. Never put a marker
inside a prose line, and never add markers for minimal fixes: the commit
records those.

One substantive case recurs and deserves its own treatment: **code pasted
outside its chunk**. A student writes their function after the skeleton
chunk's closing fence, so the parser reads R or Python as prose and reports a
cascade of quote, `$`, and `_` errors from it. There are two ways out, and the
choice is the user's, so present both when asking:

- move the chunk's closing fence to after the pasted code, with a marker
  above the chunk (as in the example marker), so the code becomes a chunk
  again; this changes the document's structure, and the student's document
  as submitted would not have executed that code; or
- escape the offending characters and leave the code as prose, in which case
  a `Chunk has at least 1` rule will fail for that question, which may be the
  grading outcome the user wants.

Structural edits are done against original line numbers in a single pass,
checking the content of every line about to be touched before applying (a
hand count of a listing is easily off by one; `sed -n '<n>p'` each line
first).

A document can carry several problems, and the parser reports only the first
fatal one per pass. After each edit re-parse (step 6) and repeat until the
document is clean. Every repair, first or fifth, follows the same tiers.

### 6. Re-parse, check for truncation, and commit

After each edit, re-parse the document exactly as grading does:

```
Rscript -e 'invisible(markermd::parse_assignment_document("<file>")); cat("OK\n")'
```

Repeat step 5 until it prints `OK` (warnings are fine). If a repository
cannot be brought to `OK` within these rules, revert its edits
(`git -C <repo> checkout -- <file>`), leave it on the original branch
(`git -C <repo> checkout <original-branch>` and delete the empty branch with
`git -C <repo> branch -D grading-fixes` if this run created it), and report
it as unrepaired with the remaining diagnostic.

Then check that a clean parse is also a complete one. An unclosed code fence
or `:::` div produces no diagnostic: everything after it is swallowed into
that block, and the sections below silently disappear. Print the sections the
parser sees, with their chunk counts:

```
Rscript -e 'print(markermd::assignment_outline("<file>")[, c("type", "id", "level", "title", "n_chunk")], row.names = FALSE)'
```

Compare against the headings actually present in the file (the `#` lines
outside code blocks) and, when the key was outlined in step 2, against the
key's anchors: every heading in the file should appear, with sensible chunk
counts. Headings that are in the file but absent from the outline sit below
an unclosed fence or div; find it, close it (a minimal fix when the end of the
code is evident, otherwise substantive), and re-parse.

Commit only the assignment document, once per repository, with a one-line
message that says what was done and no attribution trailers:

```
git -C <repo> add <file>
git -C <repo> commit -m "Escape markdown characters that break parsing"
```

Use a message that fits the repairs made (for example `Fix chunk fences and
escape quotes that break parsing` when a fence moved). Never `git add -A`:
anything else in the working tree (rendered output, data files) stays out of
the commit. If git refuses to commit because no `user.name` / `user.email` is
configured, ask the user which identity to use and pass it with
`git -c user.name=... -c user.email=... commit ...`. Leave `grading-fixes`
checked out.

### 7. Verify across the project

Re-run the parse check from step 3. Every repository repaired in this run
should now be `ok` or `warning`; anything still `error` is listed in the
report with its diagnostic.

If the project has a stored template, also run the structural validation:

```
Rscript -e 'res <- markermd::validate_project("<root>"); print(res, row.names = FALSE)'
```

Repaired repositories should no longer have `error` rows. Their per-question
`pass` / `fail` results are the template's and rubric's business, not this
skill's: do not edit a document to make a rule pass. The one exception is a
repaired repository where every question below some point fails with
`No nodes selected` (its section was not found): that is the truncation
signature from step 6, so go back and look for the unclosed fence.

### 8. Report

Summarize for the user, per repository: the document, the original branch,
each fix with its line and tier, and the commit. Then the repositories that
were skipped (uncommitted changes, not a git repository, the user declined a
substantive fix) or could not be repaired, each with its diagnostic, the
repositories with no document at all, and any edit made to the key (and that
it is uncommitted). Explain:

- `mark()` and `validate_project()` read the working tree, so the repaired
  documents on `grading-fixes` are what gets validated and graded, and
  grading must happen with `grading-fixes` checked out. The student's
  untouched submission is on the original branch, and
  `git -C <repo> diff <original-branch>..grading-fixes` shows every change.
- To undo a repository's repairs: `git -C <repo> checkout <original-branch>`
  then `git -C <repo> branch -D grading-fixes`.
- Substantive fixes are flagged in the document with `<!-- markermd: ... -->`
  comments; graders and the `markermd-scaffold-rubric` /
  `markermd-apply-rubric` skills treat those markers as not the student's
  work.
- A parse failure here is not evidence that the student's document fails to
  render: markermd's parser is stricter than Quarto 1.x. Judge rendering from
  the rendered artifact, never from the fact that a repair was needed. In
  particular a repair is never a document-level problem for a document-scope
  question to record.

Then point the user at the next step: the `markermd-scaffold-template` skill's
validation if the template is not yet settled, otherwise
`markermd::mark("<root>")` or the rubric skills.

Shell notes for macOS: `cat -A` does not exist (use `cat -vet` to see
invisible characters), and R scripts that need regexes with `\s` or escape
sequences are easier to run from a file than from `Rscript -e`.
