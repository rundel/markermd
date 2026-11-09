# Prompt injection scan
#
# Deterministic detectors for text in a submission that is aimed at an
# automated grader rather than at the instructor. scan_project() runs them
# over every student repository and rendered report and returns candidates;
# judging them is left to a human (or to the markermd-check-injection skill,
# which only ever sees the neutralized excerpts produced here).
#
# A finding has a `detector` (where the text sits: visible, or one of the
# concealment kinds) and `matched` (which content patterns it hit). Severity
# combines the two: content in a concealed place is high, content in visible
# text is medium when two different categories agree and low on one alone
# (ordinary coursework prose hits one category all the time: "ignore all NA
# values and follow the rules", "Note to the grader: I ran out of time"),
# concealment with no content match is low.

# Content patterns by category, as case-insensitive multi-line PCRE. The
# line-anchored role markers ("System:" at the start of a line, a "System
# prompt" heading) are only meaningful in a markdown source; in a YAML or data
# file every `system:` key would hit them.
#
# markdown: Whether the text comes from a markdown document

injection_content_patterns = function(markdown = TRUE) {
  model = "ai|llm|large language model|language model|claude|chatgpt|gpt|codex|copilot|gemini"
  role_lines = c(
    "^\\s*(system|assistant|human)\\s*:\\s*\\S",
    "^#{1,6}\\s*(system|instructions?)\\s*(prompt|message)?\\s*:?\\s*$"
  )
  list(
    "instruction-override" = c(
      paste0(
        "\\b(ignore|disregard|forget|override|bypass)\\b[^.\\n]{0,40}",
        "\\b(previous|prior|above|earlier|preceding|all|any|your|the|system)\\s+(?:[\\w'-]+\\s+){0,2}",
        "(instructions?|prompts?|rules|rubric|guidelines|directions|criteria)\\b"
      ),
      "\\bnew (instructions?|task|rules)\\s*:",
      "\\b(system|developer)\\s+(prompt|message|override|instructions?)\\b",
      "\\bdo not (follow|apply|use)\\b[^.\\n]{0,30}\\b(rubric|instructions|grading)\\b"
    ),
    "grader-addressed" = c(
      paste0(
        "\\b(you are|you're|as) an? (ai( (assistant|grader|model|agent))?|llm|",
        "(large )?language model|automated grader|helpful assistant)\\b"
      ),
      glue::glue(
        "\\b(dear|attention|note to|message (to|for)|instructions? (to|for)|hey|hello)[,:]?\\s+",
        "(the\\s+|any\\s+)?(<<model>>|grader|grading (assistant|agent|model|system)|",
        "ai (assistant|grader|model|agent))\\b",
        .open = "<<", .close = ">>"
      ),
      glue::glue(
        "\\bif you are (an? )?(<<model>>|automated|bot|(ai )?(grader|assistant|agent))\\b",
        .open = "<<", .close = ">>"
      )
    ),
    "grade-demand" = c(
      paste0(
        "\\b(give|award|assign|grant)\\b[^.\\n]{0,40}",
        "\\b(full|maximum|max|perfect|100\\s*%|top|highest)\\s+(marks?|credits?|points?|scores?|grades?)\\b"
      ),
      paste0(
        "\\b(grade|mark|score)\\s+(this|it|me|my|the (answer|submission|response|work))\\b[^.\\n]{0,40}",
        "\\b(full|maximum|max|perfect|100\\s*%|top|highest)\\s+(marks?|credits?|points?|scores?|grades?)\\b"
      ),
      "\\b(do not|don't|never)\\s+(deduct|penali[sz]e|take (off|away)|subtract|mark (it |this )?down)\\b",
      paste0(
        "\\b(this|the)\\s+(submission|answer|response|assignment|work|student|solution)\\s+",
        "(should|must|deserves?|shall|merits|has earned)\\b[^.\\n]{0,40}",
        "\\b(full|maximum|perfect|100\\s*%|highest|an? A\\+?)\\b"
      ),
      "\\b(positive|favou?rable)\\s+(review|evaluation|feedback|assessment)\\s+only\\b"
    ),
    "role-marker" = c(
      if (markdown) role_lines,
      "</?(system|instructions?|system[-_]prompt|assistant|human)>",
      "<\\|(im_start|im_end|system|user|assistant|endoftext)\\|>",
      "\\[/?INST\\]"
    ),
    "score-line" = paste0(
      "^\\s*[-*>]?\\s*(final\\s+|total\\s+)?(score|grade|marks?|points)\\s*(awarded|given|earned)?",
      "\\s*[:=]\\s*\\d+(\\.\\d+)?\\s*(/|out of)\\s*\\d+"
    ),
    "grading-output" = c(
      paste0(
        "^\\s*[-*>]?\\s*(private_comment|selected (rubric )?items?|rubric items?|",
        "grader('s)? (notes?|comments?|feedback)|grading (notes?|results?|decision))\\s*:"
      )
    )
  )
}

# Content categories that carry no weight on their own in visible text: a
# "Score: 8/10" line is ordinary program output (a game, a test summary) far
# more often than it is part of a fabricated grading record.

injection_weak_categories = function() {
  "score-line"
}

# The content categories one string matches. Rubric item descriptions quoted
# verbatim count as the "rubric-text" category.
#
# text: Character scalar (may span lines)
# rubric: Character vector of rubric item descriptions to look for
# markdown: Whether the text comes from a markdown document (see
#   injection_content_patterns())

match_injection_content = function(text, rubric = character(0), markdown = TRUE, fold = TRUE) {
  match_injection_lines(text, rubric, markdown, fold)[[1]]
}

# match_injection_content() for each of a set of strings, as a list of
# category vectors. Each pattern runs once over the whole vector. The patterns
# run against fold_for_matching() of the text, so a phrase split by invisible
# characters or spelled with look-alike letters still matches.
#
# lines: Character vector
# rubric: Character vector of rubric item descriptions to look for
# markdown: Whether the text comes from a markdown document
# fold: Whether to fold the text first (FALSE matches the text as written)

match_injection_lines = function(lines, rubric = character(0), markdown = TRUE, fold = TRUE) {
  if (fold) {
    lines = fold_for_matching(lines)
  }
  patterns = injection_content_patterns(markdown)
  hit = vapply(patterns, function(p) {
    Reduce(`|`, lapply(p, function(re) grepl(paste0("(?im)", re), lines, perl = TRUE)))
  }, logical(length(lines)))
  hit = matrix(hit, nrow = length(lines), dimnames = list(NULL, names(patterns)))

  if (length(rubric) > 0) {
    lower = tolower(lines)
    quoted = Reduce(`|`, lapply(tolower(rubric), grepl, x = lower, fixed = TRUE))
    hit = cbind(hit, "rubric-text" = quoted)
  }
  lapply(seq_along(lines), function(i) colnames(hit)[hit[i, ]])
}

# Code points that render as nothing, by kind:
#   zero-width: the zero-width space, non-joiner and joiner, the word joiner,
#     the byte order mark used mid-text and the Mongolian vowel separator
#   ignorable: the other Default_Ignorable characters, which are legitimate in
#     some scripts (soft hyphen, the left-to-right and right-to-left marks, the
#     invisible math operators, the Hangul fillers) but also split a phrase
#   bidi: the bidirectional embedding, override and isolate controls
#   tag: the Unicode tag block, which spells ASCII text that displays as
#     nothing
#   vs: the variation selectors and their supplement, which carry bytes in
#     one smuggling scheme and are otherwise ordinary in emoji
# ZWJ is listed so runs are counted, but a lone one is ordinary in emoji
# sequences and is not reported by itself.
#
# cps: Integer vector of code points

invisible_kind = function(cps) {
  kind = rep(NA_character_, length(cps))
  kind[cps %in% c(0x200B, 0x200C, 0x200D, 0x2060, 0xFEFF, 0x180E)] = "zero-width"
  kind[cps %in% c(0x00AD, 0x034F, 0x061C, 0x115F, 0x1160, 0x17B4, 0x17B5, 0x200E, 0x200F, 0x3164, 0xFFA0) |
    (cps >= 0x180B & cps <= 0x180D) | (cps >= 0x2061 & cps <= 0x2064)] = "ignorable"
  kind[(cps >= 0x202A & cps <= 0x202E) | (cps >= 0x2066 & cps <= 0x2069)] = "bidi"
  kind[cps >= 0xE0000 & cps <= 0xE007F] = "tag"
  kind[(cps >= 0xFE00 & cps <= 0xFE0F) | (cps >= 0xE0100 & cps <= 0xE01EF)] = "vs"
  kind
}

# Positions of tag characters that are part of a well-formed emoji tag
# sequence (a black flag, tag letters, the cancel tag): the subdivision flags
# of England, Scotland and Wales, which are ordinary text.
#
# cps: Integer vector of code points

emoji_tag_positions = function(cps) {
  out = integer(0)
  for (start in which(cps == 0x1F3F4)) {
    i = start + 1
    while (i <= length(cps) && cps[i] >= 0xE0020 && cps[i] <= 0xE007E) {
      i = i + 1
    }
    if (i > start + 1 && i <= length(cps) && cps[i] == 0xE007F) {
      out = c(out, (start + 1):i)
    }
  }
  out
}

# Runs of variation selectors long enough to be a smuggled payload rather than
# an emoji presentation selector: four or more when any comes from the
# supplement block, eight or more otherwise. Returns a list of index vectors.
#
# cps: Integer vector of code points
# kind: invisible_kind(cps)

vs_payload_runs = function(cps, kind) {
  is_vs = !is.na(kind) & kind == "vs"
  if (!any(is_vs)) {
    return(list())
  }
  r = rle(is_vs)
  ends = cumsum(r$lengths)
  starts = ends - r$lengths + 1
  runs = lapply(which(r$values), function(k) starts[k]:ends[k])
  Filter(function(run) {
    length(run) >= 8 || (length(run) >= 4 && any(cps[run] >= 0xE0100))
  }, runs)
}

# The bytes a run of variation selectors encodes (U+FE00 to U+FE0F are 0 to
# 15, U+E0100 to U+E01EF are 16 to 255), as readable text.
#
# cps: Integer vector of code points, all variation selectors

decode_vs_payload = function(cps) {
  bytes = ifelse(cps >= 0xE0100, cps - 0xE0100 + 16, cps - 0xFE00)
  printable = bytes %in% c(9, 10, 13, 32:126)
  if (!any(printable)) {
    return("")
  }
  rawToChar(as.raw(bytes[printable]))
}

has_non_ascii = function(x) {
  grepl("[^\\x01-\\x7F]", x, perl = TRUE, useBytes = TRUE)
}

# Code points of a string, with invalid UTF-8 read byte by byte (utf8ToInt()
# would return NA for the whole string).
#
# x: Character scalar

safe_code_points = function(x) {
  if (!validUTF8(x)) {
    x = iconv(x, "UTF-8", "UTF-8", sub = "byte")
  }
  cps = utf8ToInt(x)
  if (anyNA(cps)) integer(0) else cps
}

# Replace invisible code points with a visible <U+XXXX> form so an excerpt
# can be read safely. Variation selectors are left alone unless they form a
# payload run, so emoji keep their presentation selector.
#
# x: Character scalar

show_invisible = function(x) {
  if (!has_non_ascii(x)) {
    return(x)
  }
  cps = safe_code_points(x)
  kind = invisible_kind(cps)
  flagged = !is.na(kind) & kind != "vs"
  flagged[unlist(vs_payload_runs(cps, kind))] = TRUE
  flagged[emoji_tag_positions(cps)] = FALSE
  if (!any(flagged)) {
    return(x)
  }
  chars = vapply(cps, intToUtf8, character(1))
  chars[flagged] = paste0("<U+", toupper(format(as.hexmode(cps[flagged]), width = 4)), ">")
  paste(chars, collapse = "")
}

# Drop invisible code points, so a phrase split by them still matches.
#
# x: Character vector

strip_invisible = function(x) {
  x = valid_utf8(x)
  gsub(invisible_class_regex(), "", x, perl = TRUE)
}

# A PCRE character class of every code point invisible_kind() classifies, so
# whole vectors can be tested and stripped in one call. The (*UTF) prefix puts
# PCRE in UTF mode even for an all-ASCII vector, where R would otherwise
# compile the pattern bytewise and reject the wide code points.

invisible_class_regex = function() {
  paste0(
    "(*UTF)[\\x{200B}-\\x{200F}\\x{2060}-\\x{2064}\\x{FEFF}\\x{180B}-\\x{180E}\\x{00AD}\\x{034F}\\x{061C}",
    "\\x{115F}\\x{1160}\\x{17B4}\\x{17B5}\\x{3164}\\x{FFA0}\\x{202A}-\\x{202E}\\x{2066}-\\x{2069}",
    "\\x{E0000}-\\x{E007F}\\x{FE00}-\\x{FE0F}\\x{E0100}-\\x{E01EF}]"
  )
}

# Strings with invalid UTF-8 read byte by byte, so the PCRE calls below never
# error on them.
#
# x: Character vector

valid_utf8 = function(x) {
  bad = !validUTF8(x)
  if (any(bad)) {
    x[bad] = iconv(x[bad], "UTF-8", "UTF-8", sub = "byte")
  }
  x
}

# Letters from other scripts that look like Latin ones, folded to the Latin
# letter so "Ignоre" with a Cyrillic o still matches. Fullwidth ASCII is folded
# arithmetically.

confusable_map = function() {
  from = c(
    "а", "е", "о", "р", "с", "у", "х", "і", "ј", "ѕ",
    "һ", "ԁ", "ԛ", "ԝ", "А", "В", "Е", "К", "М", "Н",
    "О", "Р", "С", "Т", "Х", "ο", "ν", "α", "ε", "ρ",
    "τ", "υ", "ι", "κ", "Α", "Β", "Ε", "Ζ", "Η", "Ι",
    "Κ", "Μ", "Ν", "Ο", "Ρ", "Τ", "Υ", "Χ"
  )
  to = c(
    "a", "e", "o", "p", "c", "y", "x", "i", "j", "s",
    "h", "d", "q", "w", "A", "B", "E", "K", "M", "H",
    "O", "P", "C", "T", "X", "o", "v", "a", "e", "p",
    "t", "u", "i", "k", "A", "B", "E", "Z", "H", "I",
    "K", "M", "N", "O", "P", "T", "Y", "X"
  )
  list(from = paste(from, collapse = ""), to = paste(to, collapse = ""))
}

# The copy of a text the content patterns run against: invisible characters
# dropped, fullwidth ASCII and look-alike letters folded to Latin.
#
# x: Character vector

fold_for_matching = function(x) {
  x = strip_invisible(x)
  idx = which(has_non_ascii(x))
  if (length(idx) == 0) {
    return(x)
  }
  map = confusable_map()
  x[idx] = chartr(
    paste0(map$from, intToUtf8(0xFF01:0xFF5E)), paste0(map$to, intToUtf8(0x21:0x7E)), x[idx]
  )
  x
}

# A single-line, length-capped excerpt with invisible characters made
# visible.
#
# x: Character scalar
# at: Character position the excerpt should include
# width: Maximum number of characters

make_excerpt = function(x, at = 1, width = 240) {
  if (nchar(x) > width) {
    start = max(1, at - 60)
    x = paste0(
      if (start > 1) "...",
      substr(x, start, start + width - 1),
      if (start + width - 1 < nchar(x)) "..."
    )
  }
  show_invisible(trimws(gsub("\\s+", " ", x)))
}

# Character position of the first content match in a text, so a long line's
# excerpt is taken around the match rather than from its start (a paragraph
# that ends in an injection would otherwise show only its benign opening).
#
# text: Character scalar
# matched: The content categories the text hit (see match_injection_content())
# rubric: Rubric item descriptions, for the "rubric-text" category

content_match_position = function(text, matched, rubric = character(0)) {
  if (length(matched) == 0 || !nzchar(text)) {
    return(1L)
  }
  text = fold_for_matching(text)
  patterns = injection_content_patterns()
  regexes = unlist(patterns[intersect(matched, names(patterns))], use.names = FALSE)
  positions = vapply(regexes, function(re) {
    regexpr(paste0("(?im)", re), text, perl = TRUE)[1]
  }, integer(1))
  if ("rubric-text" %in% matched && length(rubric) > 0) {
    lower = tolower(text)
    positions = c(positions, vapply(tolower(rubric), function(r) regexpr(r, lower, fixed = TRUE)[1], integer(1)))
  }
  positions = positions[positions > 0]
  if (length(positions) == 0) 1L else min(positions)
}

# make_excerpt() centered on the first content match
#
# text: Character scalar
# matched: The content categories the text hit
# rubric: Rubric item descriptions

match_excerpt = function(text, matched, rubric = character(0)) {
  make_excerpt(text, at = content_match_position(text, matched, rubric))
}

empty_findings = function() {
  data.frame(
    line = integer(0), severity = character(0), detector = character(0),
    matched = character(0), excerpt = character(0), stringsAsFactors = FALSE
  )
}

# One finding, or several when the arguments are vectors of the same length
# (`matched` is then a list of category vectors).
#
# line: Integer line numbers (NA for rendered text)
# severity, detector, excerpt: Character vectors
# matched: Character vector of categories for one finding, or a list of them

finding = function(line, severity, detector, matched, excerpt) {
  if (!is.list(matched)) {
    matched = list(matched)
  }
  data.frame(
    line = as.integer(line), severity = severity, detector = detector,
    matched = vapply(matched, function(m) if (length(m) == 0) NA_character_ else paste(m, collapse = ", "), character(1)),
    excerpt = excerpt, stringsAsFactors = FALSE
  )
}

bind_findings = function(rows) {
  rows = Filter(function(x) !is.null(x) && nrow(x) > 0, rows)
  if (length(rows) == 0) {
    return(empty_findings())
  }
  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}

# Severity of a concealed region: any content match makes it high.
#
# matched: Character vector of content categories

concealed_severity = function(matched) {
  if (length(matched) > 0) "high" else "low"
}

# Severity of a content match in visible text: medium when two different
# categories agree (an override beside a grade demand, text addressed to a
# model that also tells it what to do), low on one category alone.
#
# matched: Character vector of content categories

visible_severity = function(matched) {
  strong = setdiff(matched, injection_weak_categories())
  if (length(strong) >= 2) "medium" else "low"
}

# Invisible-character findings for a set of lines, built from per-line
# counts rather than one data frame per line (a data file of accented names
# has tens of thousands of non-ASCII lines).
#
# lines: Character vector
# line_numbers: Whether findings carry line numbers (FALSE for rendered text)
# rubric: Rubric item descriptions
# line_matches: match_injection_lines() of the raw lines, when the caller has
#   them, so a phrase the invisible characters split can be told apart from
#   one that matched anyway

scan_invisible = function(lines, line_numbers = TRUE, rubric = character(0), line_matches = NULL) {
  lines = valid_utf8(lines)
  idx = which(grepl(invisible_class_regex(), lines, perl = TRUE))
  if (length(idx) == 0) {
    return(empty_findings())
  }
  line_of = function(i) if (line_numbers) i else NA_integer_

  per_line = lapply(idx, function(i) {
    cps = safe_code_points(lines[i])
    if (i == 1 && length(cps) > 0 && cps[1] == 0xFEFF) {
      cps = cps[-1]
    }
    kind = invisible_kind(cps)
    kind[emoji_tag_positions(cps)] = NA_character_
    is_kind = function(k) !is.na(kind) & kind == k
    list(
      cps = cps, kind = kind,
      tag = cps[is_kind("tag")],
      vs_runs = vs_payload_runs(cps, kind),
      n_run = sum(is_kind("zero-width") | is_kind("ignorable")),
      zw_at = which(is_kind("zero-width") & cps != 0x200D & !(cps == 0x200C & any(cps >= 0x0590 & cps <= 0x08FF)))[1],
      ignorable_at = which(is_kind("ignorable"))[1],
      bidi_at = which(is_kind("bidi"))[1],
      rtl = any(cps >= 0x0590 & cps <= 0x08FF),
      visible = intToUtf8(cps[is.na(kind) | is_kind("vs")])
    )
  })

  rows = list()
  for (k in seq_along(idx)) {
    i = idx[k]
    p = per_line[[k]]
    line = line_of(i)
    whole = intToUtf8(p$cps)

    if (length(p$tag) > 0) {
      payload = intToUtf8(p$tag[p$tag >= 0xE0020 & p$tag <= 0xE007E] - 0xE0000)
      rows[[length(rows) + 1]] = finding(
        line, "high", "invisible-unicode", match_injection_content(payload, rubric),
        make_excerpt(glue::glue("{length(p$tag)} tag characters decoding to: {payload} | in: {p$visible}"))
      )
    }
    for (run in p$vs_runs) {
      payload = decode_vs_payload(p$cps[run])
      rows[[length(rows) + 1]] = finding(
        line, "high", "invisible-unicode", match_injection_content(payload, rubric),
        make_excerpt(glue::glue("{length(run)} variation selectors decoding to: {payload} | in: {p$visible}"))
      )
    }

    split_match = character(0)
    if (p$n_run > 0) {
      raw_matched = if (is.null(line_matches)) match_injection_content(lines[i], rubric, fold = FALSE) else line_matches[[i]]
      stripped = match_injection_content(strip_invisible(lines[i]), rubric)
      split_match = setdiff(stripped, raw_matched)
    }
    if (length(split_match) > 0) {
      rows[[length(rows) + 1]] = finding(
        line, "high", "invisible-unicode", stripped,
        make_excerpt(whole, at = min(p$zw_at, p$ignorable_at, na.rm = TRUE))
      )
    } else if (p$n_run >= 8) {
      rows[[length(rows) + 1]] = finding(
        line, "high", "invisible-unicode", character(0),
        make_excerpt(glue::glue("{p$n_run} invisible characters (possibly an encoded payload) in: {p$visible}"))
      )
    } else if (!is.na(p$zw_at)) {
      rows[[length(rows) + 1]] = finding(
        line, "low", "invisible-unicode", character(0), make_excerpt(whole, at = p$zw_at)
      )
    }

    if (!is.na(p$bidi_at)) {
      rows[[length(rows) + 1]] = finding(
        line, if (p$rtl) "low" else "medium", "bidi-control", character(0), make_excerpt(whole, at = p$bidi_at)
      )
    }
  }
  bind_findings(rows)
}

# CSS declarations that hide text from a reader of the rendered page.

hidden_style_regex = function() {
  paste0("(?i)(", paste(c(
    "display\\s*:\\s*none",
    "visibility\\s*:\\s*(hidden|collapse)",
    "(?<![-\\w])opacity\\s*:\\s*(?:0+(?:\\.0+)?|\\.0+)(?![\\d.])",
    "font-size\\s*:\\s*(?:0+(?:\\.0+)?(?![\\d.])|[0-3](?:\\.\\d+)?\\s*(?:px|pt)|0?\\.[0-2]\\d*\\s*r?em)",
    paste0(
      "(?<![-\\w])color\\s*:\\s*(?:white\\b|#fff\\b|#ffffff\\b|transparent\\b|",
      "rgba?\\(\\s*255\\s*,\\s*255\\s*,\\s*255|rgba\\([^)]*,\\s*0*(?:\\.0+)?\\s*\\))"
    ),
    "(?:left|top|text-indent|margin-left)\\s*:\\s*-\\d{3,}",
    "clip-path\\s*:\\s*inset\\(\\s*(?:[5-9]\\d|100)\\s*%",
    "(?<![-\\w])clip\\s*:\\s*rect\\(\\s*0",
    "(?<![-\\w])(?:max-)?(?:height|width)\\s*:\\s*0(?:px)?\\s*(?:;|!important)[^\"'{}]*overflow\\s*:\\s*hidden",
    "overflow\\s*:\\s*hidden[^\"'{}]*(?<![-\\w])(?:max-)?(?:height|width)\\s*:\\s*0(?:px)?(?![\\d.])"
  ), collapse = "|"), ")")
}

# Class names a document's own style sheets hide (a rule whose selectors are
# plain class selectors and whose declarations match hidden_style_regex()),
# so `.q { display: none }` hides `<p class="q">` as surely as an inline
# style would.
#
# css: Character scalar, the text of the document's style elements

css_hidden_classes = function(css) {
  css = gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
  rules = regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css, perl = TRUE))[[1]]
  if (length(rules) == 0) {
    return(character(0))
  }
  selector = trimws(sub("\\{.*$", "", rules))
  body = sub("^[^{]*\\{", "", rules)
  hiding = grepl(hidden_style_regex(), body, perl = TRUE) &
    grepl("^\\s*\\.[-\\w]+\\s*(,\\s*\\.[-\\w]+\\s*)*$", selector, perl = TRUE)
  classes = unlist(regmatches(selector[hiding], gregexpr("(?<=\\.)[-\\w]+", selector[hiding], perl = TRUE)))
  setdiff(unique(classes), c("hidden", "d-none", "visually-hidden", "sr-only", "invisible", "content-hidden"))
}

# Script elements, event-handler attributes and embedded frames that are not
# a rendered report's own machinery. A self-contained Quarto report inlines
# its libraries (Bootstrap, Popper, clipboard.js, its own page script), which
# use fetch() and walk DOM parents, so a script is reported only when it
# reaches out of its frame (the grading app's window, its Shiny session, the
# cookies) or its text matches a content pattern, or when it loads code from a
# host other than the usual CDNs. Event-handler attributes never appear in
# Quarto output; an outside iframe, object or embed is noted at low.
#
# doc: xml2 document, before scripts are removed
# rubric: Rubric item descriptions

scan_active_content = function(doc, rubric = character(0)) {
  reach = paste0(
    "(?<![.\\w$])(?:window\\.)?(?:parent|top|opener)\\.(?:Shiny|postMessage|document|location|frames)\\b|",
    "\\bShiny\\.(?:setInputValue|onInputChange)\\b|document\\.cookie|navigator\\.sendBeacon"
  )
  cdn = "^(https?:)?//(cdn\\.jsdelivr\\.net|cdnjs\\.cloudflare\\.com|unpkg\\.com|code\\.jquery\\.com|cdn\\.plot\\.ly|cdn\\.datatables\\.net|polyfill\\.io)/"
  external = "^(https?:)?//"
  rows = list()

  scripts = xml2::xml_find_all(doc, "//script")
  src = xml2::xml_attr(scripts, "src")
  for (k in seq_along(scripts)) {
    if (!is.na(src[k])) {
      if (grepl(external, src[k], perl = TRUE) && !grepl(cdn, src[k], perl = TRUE)) {
        rows[[length(rows) + 1]] = finding(NA, "medium", "active-content", character(0), make_excerpt(paste("script loaded from", src[k])))
      }
      next
    }
    code = xml2::xml_text(scripts[[k]])
    matched = match_injection_content(code, rubric, markdown = FALSE)
    reaches = grepl(reach, code, perl = TRUE)
    if (length(matched) == 0 && !reaches) {
      next
    }
    at = if (reaches) regexpr(reach, code, perl = TRUE)[1] else content_match_position(strip_invisible(code), matched, rubric)
    label = if (reaches) "script reaching out of the report: " else "script: "
    rows[[length(rows) + 1]] = finding(
      NA, "high", "active-content", matched, paste0(label, make_excerpt(strip_invisible(code), at = at))
    )
  }

  handlers = xml2::xml_find_all(doc, "//@*[starts-with(name(), 'on')]")
  for (h in handlers) {
    value = xml2::xml_text(h)
    matched = match_injection_content(value, rubric, markdown = FALSE)
    rows[[length(rows) + 1]] = finding(
      NA, if (length(matched) > 0 || grepl(reach, value, perl = TRUE)) "high" else "medium", "active-content", matched,
      make_excerpt(paste0(xml2::xml_name(h), "=\"", value, "\""))
    )
  }

  frames = xml2::xml_find_all(doc, "//iframe[@src] | //object[@data] | //embed[@src]")
  for (fr in frames) {
    target = xml2::xml_attr(fr, if (xml2::xml_name(fr) == "object") "data" else "src")
    if (grepl(external, target, perl = TRUE)) {
      rows[[length(rows) + 1]] = finding(
        NA, "low", "active-content", character(0), make_excerpt(paste0("<", xml2::xml_name(fr), "> loading ", target))
      )
    }
  }

  css = paste(xml2::xml_text(xml2::xml_find_all(doc, "//style")), collapse = "\n")
  generated = regmatches(css, gregexpr("content\\s*:\\s*([\"'])(?:(?!\\1).){12,}\\1", css, perl = TRUE))[[1]]
  for (g in unique(generated)) {
    text = sub("^content\\s*:\\s*[\"']", "", sub("[\"']$", "", g))
    matched = match_injection_content(text, rubric, markdown = FALSE)
    if (length(matched) > 0) {
      rows[[length(rows) + 1]] = finding(NA, "high", "hidden-style", matched, make_excerpt(paste("CSS generated text:", text)))
    }
  }

  bind_findings(rows)
}

hidden_class_names = function() {
  "hidden|d-none|visually-hidden|sr-only|invisible|content-hidden"
}

# Whether each source line carries markup that hides its content: an inline
# style, a hiding class in a Pandoc attribute or an HTML class attribute, the
# HTML hidden attribute, or white LaTeX text.
#
# lines: Character vector

is_hidden_markup = function(lines) {
  classes = hidden_class_names()
  (grepl("style\\s*=", lines, perl = TRUE) & grepl(hidden_style_regex(), lines, perl = TRUE)) |
    grepl(glue::glue("\\{[^}]*\\.(<<classes>>)(?![-\\w])[^}]*\\}", .open = "<<", .close = ">>"), lines, perl = TRUE) |
    grepl(
      glue::glue("class\\s*=\\s*[\"'][^\"']*(?<![-\\w])(<<classes>>)(?![-\\w])", .open = "<<", .close = ">>"),
      lines, perl = TRUE
    ) |
    grepl("<[a-zA-Z]+[^>]*\\shidden(\\s|>|=)", lines, perl = TRUE) |
    grepl("\\\\(text)?color\\{white\\}", lines, perl = TRUE)
}

# The last line of the region a hiding line opens: the closing fence of a
# fenced div, the closing tag of an unclosed HTML element, otherwise the line
# itself. Capped so a missing closer cannot swallow the document.
#
# lines: Character vector
# i: Index of the hiding line

hidden_region_end = function(lines, i, cap = 60) {
  last = min(length(lines), i + cap)
  rest = if (i < last) (i + 1):last else integer(0)

  if (grepl("^\\s*:::+\\s*\\{", lines[i])) {
    depth = 1
    for (j in rest) {
      if (grepl("^\\s*:::+\\s*\\{", lines[j])) {
        depth = depth + 1
      } else if (grepl("^\\s*:::+\\s*$", lines[j])) {
        depth = depth - 1
      }
      if (depth == 0) {
        return(j)
      }
    }
    return(last)
  }

  tag = regmatches(lines[i], regexpr("<(div|span|p|section|details)\\b", lines[i], perl = TRUE))
  if (length(tag) == 1) {
    closer = paste0("</", sub("<", "", tag, fixed = TRUE), ">")
    if (!grepl(closer, lines[i], fixed = TRUE)) {
      closed = rest[grepl(closer, lines[rest], fixed = TRUE)]
      return(if (length(closed) > 0) closed[1] else last)
    }
  }
  i
}

# Executable chunks whose code is not shown in the rendered document
# (include or echo off, in the header or as a `#|` option), as a data frame
# of start and end lines.
#
# lines: Character vector

hidden_chunks = function(lines) {
  open = grep("^\\s*```+\\s*\\{", lines)
  close = grep("^\\s*```+\\s*$", lines)
  off = "(include|echo)\\s*[=:]\\s*(FALSE|F|false)\\b"

  rows = lapply(open, function(i) {
    end = close[close > i]
    end = if (length(end) > 0) end[1] else length(lines)
    body = lines[i:end]
    options = c(body[1], grep("^\\s*#\\|", body, value = TRUE))
    if (any(grepl(off, options, perl = TRUE))) data.frame(start = i, end = end) else NULL
  })
  rows = Filter(Negate(is.null), rows)
  if (length(rows) == 0) data.frame(start = integer(0), end = integer(0)) else do.call(rbind, rows)
}

# Base64 runs that decode to readable text matching a content pattern.
#
# lines: Character vector
# rubric: Rubric item descriptions

scan_encoded = function(lines, rubric = character(0)) {
  decode = purrr::possibly(jsonlite::base64_dec, otherwise = raw(0))
  idx = grep("[A-Za-z0-9+/]{40,}", lines, perl = TRUE)

  bind_findings(lapply(idx, function(i) {
    runs = regmatches(lines[i], gregexpr("[A-Za-z0-9+/]{40,}={0,2}", lines[i], perl = TRUE))[[1]]
    bind_findings(lapply(runs, function(run) {
      bytes = decode(run)
      printable = bytes %in% as.raw(c(9, 10, 13, 32:126))
      if (length(bytes) == 0 || mean(printable) < 0.95) {
        return(NULL)
      }
      text = rawToChar(bytes[printable])
      matched = match_injection_content(text, rubric)
      if (length(matched) == 0) {
        return(NULL)
      }
      finding(i, "high", "encoded-text", matched, make_excerpt(paste("base64 decoding to:", text)))
    }))
  }))
}

# Findings for one source text.
#
# lines: Character vector, the file's lines
# structural: Whether the file is a markdown document, so the concealment
#   detectors (comments, hiding markup, unshown chunks) apply
# rubric: Rubric item descriptions
# verified_markers: Text of `markermd:` comments known to come from the
#   fix-parsing skill (see verified_repair_markers())
# markdown: Whether the line-anchored role markers apply (see
#   injection_content_patterns()); the visible text of a rendered report
#   counts as markdown though it is not structural

scan_text_lines = function(lines, structural = FALSE, rubric = character(0), verified_markers = character(0),
                           markdown = structural) {
  if (length(lines) == 0) {
    return(empty_findings())
  }
  clean = strip_invisible(lines)
  line_matches = match_injection_lines(clean, rubric, markdown)
  covered = rep(FALSE, length(lines))
  rows = list(scan_invisible(lines, rubric = rubric), scan_encoded(clean, rubric))

  # A phrase that matches only once look-alike letters are folded to Latin is
  # spelled to slip past a reader or a filter, which is concealment.
  folded = which(has_non_ascii(clean) & lengths(line_matches) > 0)
  if (length(folded) > 0) {
    as_written = match_injection_lines(clean[folded], rubric, markdown, fold = FALSE)
    for (k in seq_along(folded)) {
      i = folded[k]
      if (length(setdiff(line_matches[[i]], as_written[[k]])) > 0) {
        covered[i] = TRUE
        rows[[length(rows) + 1]] = finding(
          i, "high", "look-alike-text", line_matches[[i]], match_excerpt(clean[i], line_matches[[i]], rubric)
        )
      }
    }
  }

  if (structural) {
    text = paste(clean, collapse = "\n")
    line_starts = cumsum(c(1, nchar(clean) + 1))
    m = gregexpr("(?s)<!--.*?(?:-->|\\z)", text, perl = TRUE)[[1]]

    if (m[1] != -1) {
      comments = regmatches(text, list(m))[[1]]
      first = findInterval(as.integer(m), line_starts)
      last = findInterval(as.integer(m) + attr(m, "match.length") - 1, line_starts)

      bodies = trimws(gsub("^<!--|-->$", "", comments))
      body_matches = match_injection_lines(bodies, rubric, markdown)
      marker = grepl("^markermd:", bodies)
      keep = nzchar(bodies) & !(marker & gsub("\\s+", " ", bodies) %in% verified_markers)
      any_match = lengths(body_matches) > 0
      severity = ifelse(marker, ifelse(any_match, "high", "medium"), ifelse(any_match, "high", "low"))
      excerpts = vapply(which(keep), function(k) {
        if (any_match[k]) match_excerpt(bodies[k], body_matches[[k]], rubric) else make_excerpt(bodies[k])
      }, character(1))
      if (any(keep)) {
        rows[[length(rows) + 1]] = finding(
          first[keep], severity[keep], ifelse(marker[keep], "markermd-marker", "html-comment"),
          body_matches[keep], excerpts
        )
      }
      covered[unlist(Map(seq, first, last))] = TRUE
    }

    for (i in which(is_hidden_markup(clean) & !covered)) {
      if (covered[i]) {
        next
      }
      end = hidden_region_end(clean, i)
      region = paste(clean[i:end], collapse = "\n")
      matched = match_injection_content(region, rubric, markdown)
      covered[i:end] = TRUE
      rows[[length(rows) + 1]] = finding(i, concealed_severity(matched), "hidden-style", matched, match_excerpt(region, matched, rubric))
    }

    chunks = hidden_chunks(clean)
    for (k in seq_len(nrow(chunks))) {
      span = chunks$start[k]:chunks$end[k]
      hits = span[lengths(line_matches[span]) > 0 & !covered[span]]
      covered[hits] = TRUE
      rows = c(rows, lapply(hits, function(i) {
        finding(i, "high", "hidden-chunk", line_matches[[i]], match_excerpt(clean[i], line_matches[[i]], rubric))
      }))
    }
  }

  rows = c(rows, lapply(which(!covered & lengths(line_matches) > 0), function(i) {
    finding(i, visible_severity(line_matches[[i]]), "visible-text", line_matches[[i]], match_excerpt(clean[i], line_matches[[i]], rubric))
  }))

  resolve_weak_findings(bind_findings(rows))
}

# Score lines in visible text mean something only beside a grader-style
# record ("Grader notes:", "Selected items:") in the same file, where
# together they read as a fabricated grading demonstration: they are then
# raised to medium, and otherwise dropped as program output. A visible line
# with one other category stays low (see visible_severity()).
#
# findings: Findings data frame for one file

resolve_weak_findings = function(findings) {
  matched = findings$matched
  matched[is.na(matched)] = ""
  categories = strsplit(matched, ", ", fixed = TRUE)
  weak_only = vapply(categories, function(m) length(m) > 0 && all(m %in% injection_weak_categories()), logical(1))
  weak = findings$detector == "visible-text" & weak_only
  record = findings$detector == "visible-text" & grepl("grading-output", findings$matched, fixed = TRUE)
  if (any(record)) {
    findings$severity[weak | record] = "medium"
    return(findings)
  }
  findings[!weak, , drop = FALSE]
}

# Findings for one HTML document (a rendered report, or an HTML file in a
# repository). Looks at what a reader of the page would not see (comments,
# elements hidden by style or class, attribute text, meta tags) and at the
# visible text of the main content. Findings carry no line numbers.
#
# path: Path to the HTML file
# rubric: Rubric item descriptions

scan_html_file = function(path, rubric = character(0)) {
  doc = xml2::read_html(path)
  main = xml2::xml_find_first(doc, "//main | //*[@id='quarto-document-content']")
  if (inherits(main, "xml_missing")) {
    main = xml2::xml_find_first(doc, "//body")
  }
  if (inherits(main, "xml_missing")) {
    main = doc
  }
  rows = list()

  attrs = xml2::xml_text(xml2::xml_find_all(
    doc, "//meta/@content | //@alt | //@title | //@aria-label"
  ))
  rows = c(rows, lapply(unique(attrs), function(value) {
    value = strip_invisible(value)
    matched = match_injection_content(value, rubric)
    if (length(matched) == 0) {
      return(NULL)
    }
    finding(NA, "high", "html-attribute", matched, match_excerpt(value, matched, rubric))
  }))

  rows[[length(rows) + 1]] = scan_active_content(doc, rubric)
  page_classes = css_hidden_classes(paste(xml2::xml_text(xml2::xml_find_all(doc, "//style")), collapse = "\n"))

  # Text the page never shows while scripts run (the report frame runs them).
  unshown = xml2::xml_find_all(main, ".//noscript | .//template")
  rows = c(rows, lapply(unique(xml2::xml_text(unshown)), function(value) {
    value = strip_invisible(trimws(value))
    matched = match_injection_content(value, rubric)
    if (length(matched) == 0) {
      return(NULL)
    }
    finding(NA, "high", "hidden-element", matched, match_excerpt(value, matched, rubric))
  }))
  xml2::xml_remove(xml2::xml_find_all(doc, "//script | //style | //noscript | //template"))

  comments = xml2::xml_find_all(doc, "//comment()")
  in_main = xml2::xml_path(comments) %in% xml2::xml_path(xml2::xml_find_all(main, ".//comment()"))
  rows = c(rows, lapply(seq_along(comments), function(k) {
    body = strip_invisible(trimws(xml2::xml_text(comments[[k]])))
    matched = match_injection_content(body, rubric)
    if (grepl("^markermd:", body)) {
      return(finding(NA, if (length(matched) > 0) "high" else "medium", "markermd-marker", matched, match_excerpt(body, matched, rubric)))
    }
    if (length(matched) == 0 && !(in_main[k] && nchar(body) >= 20)) {
      return(NULL)
    }
    finding(NA, concealed_severity(matched), "html-comment", matched, match_excerpt(body, matched, rubric))
  }))

  candidates = xml2::xml_find_all(main, ".//*[@style or @hidden or @class]")
  style = xml2::xml_attr(candidates, "style")
  class = xml2::xml_attr(candidates, "class")
  id = xml2::xml_attr(candidates, "id")
  hidden = (!is.na(style) & grepl(hidden_style_regex(), style, perl = TRUE)) |
    !is.na(xml2::xml_attr(candidates, "hidden")) |
    (!is.na(class) & grepl(paste0("(^|\\s)(", hidden_class_names(), ")(\\s|$)"), class, perl = TRUE)) |
    (!is.na(class) & vapply(strsplit(ifelse(is.na(class), "", class), "\\s+"), function(cl) any(cl %in% page_classes), logical(1)))
  chrome = grepl("quarto-|anchorjs|glightbox", paste(class, id))
  hidden_nodes = candidates[hidden & !chrome]

  rows = c(rows, lapply(unique(xml2::xml_text(hidden_nodes)), function(value) {
    value = strip_invisible(trimws(value))
    matched = match_injection_content(value, rubric)
    if (length(matched) == 0 && nchar(gsub("\\s", "", value)) < 12) {
      return(NULL)
    }
    finding(NA, concealed_severity(matched), "hidden-element", matched, match_excerpt(value, matched, rubric))
  }))
  xml2::xml_remove(hidden_nodes)

  visible = strsplit(xml2::xml_text(main), "\n", fixed = TRUE)[[1]]
  visible = visible[nzchar(trimws(visible))]
  text = scan_text_lines(visible, structural = FALSE, rubric = rubric, markdown = TRUE)
  text$line = rep(NA_integer_, nrow(text))

  out = bind_findings(c(rows, list(text)))
  out[!duplicated(out[c("detector", "excerpt")]), , drop = FALSE]
}

# Text of the `<!-- markermd: ... -->` comments the fix-parsing skill added
# to a repository: lines added by commits that are on the grading-fixes
# branch and on no other local branch, and whose author is the git identity
# configured on this machine (the grader, who ran the skill). The branch alone
# is not enough: a student can push a grading-fixes branch of their own, and
# checking it out creates the local branch from theirs. A marker anywhere
# else was not put there by markermd. Empty for a directory that is not a git
# repository, has no grading-fixes branch, or when no git identity is
# configured.
#
# repo_dir: Repository directory

verified_repair_markers = function(repo_dir) {
  lookup = purrr::possibly(function() {
    branches = gert::git_branch_list(local = TRUE, repo = repo_dir)$name
    if (!"grading-fixes" %in% branches) {
      return(character(0))
    }
    grader = email_of(gert::git_signature_default(repo = repo_dir))
    log = gert::git_log(ref = "grading-fixes", max = 1000, repo = repo_dir)
    fixes = log$commit[email_of(log$author) == grader]
    others = unlist(lapply(setdiff(branches, "grading-fixes"), function(b) {
      gert::git_log(ref = b, max = 1000, repo = repo_dir)$commit
    }))
    patches = unlist(lapply(setdiff(fixes, others), function(commit) {
      strsplit(gert::git_diff(ref = commit, repo = repo_dir)$patch, "\n", fixed = TRUE)
    }))
    added = grep("^\\+\\s*<!--\\s*markermd:", patches, value = TRUE)
    gsub("\\s+", " ", trimws(gsub("^\\+\\s*<!--|-->\\s*$", "", added)))
  }, otherwise = character(0))

  if (!is_git_repo(repo_dir)) character(0) else lookup()
}

# The email address of git signatures ("Name <email>"), lower-cased.
#
# signature: Character vector

email_of = function(signature) {
  tolower(sub("^.*<([^>]*)>.*$", "\\1", signature))
}

# Files in a directory worth scanning, skipping version-control metadata,
# dependency and cache directories, and rendering resource directories.
#
# dir: Directory to walk

list_scan_files = function(dir) {
  entries = fs::dir_ls(dir, all = TRUE, recurse = FALSE)
  is_dir = fs::is_dir(entries)
  name = fs::path_file(entries)
  skip = name %in% c(
    ".git", "renv", "node_modules", ".quarto", ".Rproj.user", "site_libs", "libs",
    "__pycache__", ".venv", "venv", ".ipynb_checkpoints"
  ) | grepl("_(files|cache)$", name)

  c(
    as.character(entries[!is_dir & !fs::is_link(entries)]),
    unlist(lapply(entries[is_dir & !skip & !fs::is_link(entries)], list_scan_files), use.names = FALSE)
  )
}

# Whether a relative path is an agent instructions file or sits in an agent
# configuration directory. A submission has no reason to ship one.
#
# rel: Relative paths

is_agent_file = function(rel) {
  name = tolower(fs::path_file(rel))
  parts = fs::path_split(rel)
  in_agent_dir = vapply(parts, function(p) {
    any(p[-length(p)] %in% c(".claude", ".cursor", ".agents", ".codex", ".gemini", ".windsurf"))
  }, logical(1))

  in_agent_dir | name %in% c(
    "claude.md", "agents.md", "gemini.md", ".cursorrules", ".windsurfrules", ".clinerules",
    "copilot-instructions.md", "llms.txt", "skill.md"
  )
}

# Read a file's lines as valid UTF-8, or NULL when it is not a text file
# worth scanning (an unrecognized extension, too large, or binary).
#
# path: File path

read_scan_lines = function(path, max_size = 5e6) {
  text_ext = c(
    "qmd", "rmd", "md", "markdown", "r", "py", "jl", "sql", "sh", "txt", "yml", "yaml",
    "json", "csv", "tsv", "tex", "bib", "css", "js", "toml", "ipynb", "stan", "cpp", "c", "h"
  )
  ext = tolower(fs::path_ext(path))
  size = file.size(path)
  if (!(ext %in% text_ext || ext == "") || is.na(size) || size == 0 || size > max_size) {
    return(NULL)
  }
  if (ext == "" && (size > 2e5 || any(readBin(path, "raw", n = 8000) == as.raw(0)))) {
    return(NULL)
  }
  iconv(readLines(path, warn = FALSE, encoding = "UTF-8"), "UTF-8", "UTF-8", sub = "byte")
}

# Findings for one directory (a student repository or the key), with file
# paths relative to it.
#
# dir: Directory to scan
# rubric: Rubric item descriptions

scan_directory = function(dir, rubric = character(0)) {
  files = list_scan_files(dir)
  rel = as.character(fs::path_rel(files, dir))
  markers = verified_repair_markers(dir)

  rows = lapply(seq_along(files), function(k) {
    ext = tolower(fs::path_ext(files[k]))
    name_matched = match_injection_content(gsub("[-_./]+", " ", rel[k]))

    found = bind_findings(list(
      if (is_agent_file(rel[k])) {
        finding(NA, "medium", "agent-file", character(0), make_excerpt(rel[k]))
      },
      if (length(name_matched) > 0) {
        finding(NA, "medium", "file-name", name_matched, make_excerpt(rel[k]))
      },
      if (ext %in% c("html", "htm")) {
        scan_html_file(files[k], rubric)
      } else {
        scan_text_lines(
          read_scan_lines(files[k]),
          structural = ext %in% c("qmd", "rmd", "md", "markdown"),
          rubric = rubric, verified_markers = markers
        )
      }
    ))
    cbind(file = rep(rel[k], nrow(found)), found, stringsAsFactors = FALSE)
  })

  rows = Filter(function(x) nrow(x) > 0, rows)
  if (length(rows) == 0) {
    return(cbind(file = character(0), empty_findings(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

# Descriptions of the project's stored rubric items long enough to be
# distinctive when quoted in a submission. Empty when no rubric is stored.
#
# root: Absolute project root

project_rubric_descriptions = function(root) {
  names = project_question_names(root)$names
  if (length(names) == 0) {
    return(character(0))
  }
  rubric = collect_rubric_data(root, names)
  descriptions = unlist(lapply(rubric$questions, function(q) {
    vapply(q$items, function(item) item@description, character(1))
  }))
  unique(descriptions[nchar(descriptions) >= 30])
}

# Drop findings that are not the student's doing: anything the key also
# contains, and low or medium findings shared by more than half of at least
# four repositories (starter text every submission inherited).
#
# findings: Data frame with repo, detector, excerpt and severity columns
# key_findings: Findings from the key directory
# n_repos: Number of student repositories

drop_baseline_findings = function(findings, key_findings, n_repos) {
  id = paste(findings$detector, findings$excerpt, sep = "\r")
  in_key = id %in% paste(key_findings$detector, key_findings$excerpt, sep = "\r")

  spread = tapply(findings$repo, id, function(x) length(unique(x)))
  shared = n_repos >= 4 & as.vector(spread[id]) > n_repos / 2 & findings$severity != "high"

  findings[!in_key & !shared, , drop = FALSE]
}

#' Scan a project's submissions for prompt injection attempts
#'
#' Looks through every student repository and rendered report for text aimed
#' at an automated grader rather than at the instructor: instructions to
#' ignore the rubric or award full marks, text addressed to an AI model, chat
#' role markers, fabricated grading output, and above all text a human reader
#' would not see (HTML comments, elements hidden by style or class, invisible
#' Unicode characters, code in chunks that are not shown, base64, attribute
#' and meta text in the rendered HTML, look-alike letters from other
#' scripts). In rendered HTML it also reports active content: scripts that
#' reach out of the report's frame or carry injection text, scripts loaded
#' from an unusual host, event-handler attributes and outside frames. It also
#' reports agent instruction
#' files (`CLAUDE.md`, `AGENTS.md`, `.cursorrules`, ...) shipped in a
#' repository and `<!-- markermd: ... -->` repair markers that were not added
#' by the bundled `markermd-fix-parsing` skill.
#'
#' The scan is deterministic pattern matching and reads no file as
#' instructions. It is meant to catch obvious attempts and to give a human
#' (or the bundled `markermd-check-injection` skill) a short list to judge;
#' it changes nothing in the project, and a finding is a candidate, not a
#' verdict. Text inside images, persuasion written as ordinary prose, and
#' heavier obfuscation are not detected.
#'
#' Findings that also occur in the key repository are dropped, as are low and
#' medium findings shared by more than half of the repositories (when there
#' are at least four), since both are starter text rather than a student's
#' doing.
#'
#' @param path Path to an initialized markermd project directory.
#' @param artifacts Logical. Also scan the rendered HTML reports found under
#'   the project's configured artifacts directories (`TRUE`, the default).
#' @return A data frame with one row per finding, ordered by repository and
#'   severity, with columns `repo`, `source` (`"repo"` or `"artifact"`),
#'   `file` (relative to the project root), `line` (`NA` for rendered HTML
#'   and file-level findings), `severity`, `detector`, `matched` and
#'   `excerpt`.
#'
#'   `detector` says where the text sits: `visible-text`, `html-comment`,
#'   `hidden-style`, `hidden-chunk`, `hidden-element`, `html-attribute`,
#'   `invisible-unicode`, `bidi-control`, `look-alike-text`, `encoded-text`,
#'   `active-content`, `markermd-marker`, `agent-file` or `file-name`. `matched` lists the content patterns it
#'   hit (`instruction-override`, `grader-addressed`, `grade-demand`,
#'   `role-marker`, `grading-output`, `score-line`, `rubric-text`), or `NA`. `severity` is
#'   `"high"` for a content match in a concealed place (and for smuggled
#'   Unicode, look-alike spelling and scripts that reach out of the report),
#'   `"medium"` for visible text matching two different content categories,
#'   an agent file, an event handler, a script from an unusual host or an
#'   unverified repair marker, and `"low"` for visible text matching one
#'   category, concealment with no content match, or an outside frame. `excerpt` is a single line, truncated, with invisible
#'   characters shown as `<U+XXXX>`.
#' @examples
#' \dontrun{
#' res = scan_project("hw01")
#' res[res$severity == "high", ]
#' }
#' @export
scan_project = function(path = ".", artifacts = TRUE) {
  if (!rlang::is_bool(artifacts)) {
    cli::cli_abort("{.arg artifacts} must be {.code TRUE} or {.code FALSE}.")
  }
  project = project_config(path)
  repos_dir = project_repos_dir(project)
  repos = project_repo_names(project)
  rubric = project_rubric_descriptions(project@root)
  root = fs::path_real(project@root)

  label_findings = function(found, repo, source, prefix) {
    if (nrow(found) == 0) {
      return(NULL)
    }
    data.frame(
      repo = repo, source = source, file = as.character(fs::path(prefix, found$file)),
      found[c("line", "severity", "detector", "matched", "excerpt")], stringsAsFactors = FALSE
    )
  }

  rows = lapply(repos, function(repo) {
    repo_dir = fs::path(repos_dir, repo)
    label_findings(scan_directory(repo_dir, rubric), repo, "repo", fs::path_rel(fs::path_real(repo_dir), root))
  })

  if (artifacts && length(repos) > 0) {
    reports = attr(suppressWarnings(resolve_repo_artifacts(project, repos)), "candidates")
    rows = c(rows, lapply(repos, function(repo) {
      do.call(rbind, lapply(reports[[repo]], function(report) {
        found = scan_html_file(report, rubric)
        found = cbind(file = rep(fs::path_file(report), nrow(found)), found, stringsAsFactors = FALSE)
        label_findings(found, repo, "artifact", fs::path_rel(fs::path_dir(report), root))
      }))
    }))
  }

  rows = Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      repo = character(0), source = character(0), file = character(0), line = integer(0),
      severity = character(0), detector = character(0), matched = character(0),
      excerpt = character(0), stringsAsFactors = FALSE
    ))
  }
  out = do.call(rbind, rows)

  key_findings = if (!is.na(project@key) && fs::dir_exists(fs::path(project@root, project@key))) {
    scan_directory(fs::path(project@root, project@key), rubric)
  } else {
    empty_findings()
  }
  out = drop_baseline_findings(out, key_findings, length(repos))

  out = out[order(out$repo, match(out$severity, c("high", "medium", "low")), out$file, out$line), , drop = FALSE]
  rownames(out) = NULL
  out
}
