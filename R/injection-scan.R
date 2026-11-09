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
# text is medium, concealment with no content match is low.

# Content patterns by category, as case-insensitive multi-line PCRE.

injection_content_patterns = function() {
  model = "ai|llm|large language model|language model|claude|chatgpt|gpt|codex|copilot|gemini"
  list(
    "instruction-override" = c(
      paste0(
        "\\b(ignore|disregard|forget|override|bypass)\\b[^.\\n]{0,40}",
        "\\b(previous|prior|above|earlier|preceding|all|any|your|the|system)\\b[^.\\n]{0,30}",
        "\\b(instructions?|prompts?|rules|rubric|guidelines|directions|criteria)\\b"
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
      "^\\s*(system|assistant|human)\\s*:\\s*\\S",
      "</?(system|instructions?|system[-_]prompt|assistant|human)>",
      "<\\|(im_start|im_end|system|user|assistant|endoftext)\\|>",
      "\\[/?INST\\]",
      "^#{1,6}\\s*(system|instructions?)\\s*(prompt|message)?\\s*:?\\s*$"
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

match_injection_content = function(text, rubric = character(0)) {
  match_injection_lines(text, rubric)[[1]]
}

# match_injection_content() for each of a set of strings, as a list of
# category vectors. Each pattern runs once over the whole vector.
#
# lines: Character vector
# rubric: Character vector of rubric item descriptions to look for

match_injection_lines = function(lines, rubric = character(0)) {
  patterns = injection_content_patterns()
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

# Code points that render as nothing: the Unicode tag block and variation
# selector supplement (both used to smuggle ASCII), zero-width characters, and
# the bidirectional controls. ZWJ is listed so runs are counted, but a lone
# one is ordinary in emoji sequences and is not reported by itself.
#
# cps: Integer vector of code points

invisible_kind = function(cps) {
  kind = rep(NA_character_, length(cps))
  kind[cps %in% c(0x200B, 0x200C, 0x200D, 0x2060, 0xFEFF, 0x180E)] = "zero-width"
  kind[(cps >= 0x202A & cps <= 0x202E) | (cps >= 0x2066 & cps <= 0x2069)] = "bidi"
  kind[(cps >= 0xE0000 & cps <= 0xE007F) | (cps >= 0xE0100 & cps <= 0xE01EF)] = "tag"
  kind
}

has_non_ascii = function(x) {
  grepl("[^\\x01-\\x7F]", x, perl = TRUE, useBytes = TRUE)
}

# Replace invisible code points with a visible <U+XXXX> form so an excerpt
# can be read safely.
#
# x: Character scalar

show_invisible = function(x) {
  if (!has_non_ascii(x)) {
    return(x)
  }
  cps = utf8ToInt(x)
  chars = strsplit(x, "")[[1]]
  flagged = !is.na(invisible_kind(cps))
  chars[flagged] = paste0("<U+", toupper(format(as.hexmode(cps[flagged]), width = 4)), ">")
  paste(chars, collapse = "")
}

# Drop invisible code points, so a phrase split by them still matches.
#
# x: Character scalar

strip_invisible = function(x) {
  if (!has_non_ascii(x)) {
    return(x)
  }
  cps = utf8ToInt(x)
  intToUtf8(cps[is.na(invisible_kind(cps))])
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

finding = function(line, severity, detector, matched, excerpt) {
  data.frame(
    line = as.integer(line), severity = severity, detector = detector,
    matched = if (length(matched) == 0) NA_character_ else paste(matched, collapse = ", "),
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

# Severity of a content match in visible text.
#
# matched: Character vector of content categories

visible_severity = function(matched) {
  if (all(matched %in% injection_weak_categories())) "low" else "medium"
}

# Invisible-character findings for a set of lines.
#
# lines: Character vector
# line_numbers: Whether findings carry line numbers (FALSE for rendered text)
# rubric: Rubric item descriptions

scan_invisible = function(lines, line_numbers = TRUE, rubric = character(0)) {
  idx = which(has_non_ascii(lines))
  bind_findings(lapply(idx, function(i) {
    cps = utf8ToInt(lines[i])
    if (i == 1 && length(cps) > 0 && cps[1] == 0xFEFF) {
      cps = cps[-1]
    }
    kind = invisible_kind(cps)
    line = if (line_numbers) i else NA_integer_
    visible = intToUtf8(cps[is.na(kind)])

    tag = cps[!is.na(kind) & kind == "tag" & cps <= 0xE007F]
    n_tag = sum(kind == "tag", na.rm = TRUE)
    n_zw = sum(kind == "zero-width" & cps != 0x200D, na.rm = TRUE)
    n_run = sum(kind == "zero-width", na.rm = TRUE)

    bind_findings(list(
      if (n_tag > 0) {
        payload = intToUtf8(tag[tag >= 0xE0020 & tag <= 0xE007E] - 0xE0000)
        finding(
          line, "high", "invisible-unicode", match_injection_content(payload, rubric),
          make_excerpt(glue::glue("{n_tag} tag characters decoding to: {payload} | in: {visible}"))
        )
      },
      if (n_run >= 8) {
        finding(
          line, "high", "invisible-unicode", character(0),
          make_excerpt(glue::glue("{n_run} zero-width characters (possibly an encoded payload) in: {visible}"))
        )
      } else if (n_zw > 0) {
        finding(
          line, "low", "invisible-unicode", character(0),
          make_excerpt(intToUtf8(cps), at = which(kind == "zero-width" & cps != 0x200D)[1])
        )
      },
      if (any(kind == "bidi", na.rm = TRUE)) {
        finding(
          line, "medium", "bidi-control", character(0),
          make_excerpt(intToUtf8(cps), at = which(kind == "bidi")[1])
        )
      }
    ))
  }))
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
    "(?:left|top|text-indent|margin-left)\\s*:\\s*-\\d{3,}"
  ), collapse = "|"), ")")
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

scan_text_lines = function(lines, structural = FALSE, rubric = character(0), verified_markers = character(0)) {
  if (length(lines) == 0) {
    return(empty_findings())
  }
  clean = vapply(lines, strip_invisible, character(1), USE.NAMES = FALSE)
  line_matches = match_injection_lines(clean, rubric)
  covered = rep(FALSE, length(lines))
  rows = list(scan_invisible(lines, rubric = rubric), scan_encoded(clean, rubric))

  if (structural) {
    text = paste(clean, collapse = "\n")
    line_starts = cumsum(c(1, nchar(clean) + 1))
    m = gregexpr("(?s)<!--.*?(?:-->|\\z)", text, perl = TRUE)[[1]]

    if (m[1] != -1) {
      comments = regmatches(text, list(m))[[1]]
      first = findInterval(as.integer(m), line_starts)
      last = findInterval(as.integer(m) + attr(m, "match.length") - 1, line_starts)

      rows = c(rows, lapply(seq_along(comments), function(k) {
        body = trimws(gsub("^<!--|-->$", "", comments[k]))
        matched = match_injection_content(body, rubric)

        if (grepl("^markermd:", body)) {
          if (gsub("\\s+", " ", body) %in% verified_markers) {
            return(NULL)
          }
          return(finding(
            first[k], if (length(matched) > 0) "high" else "medium", "markermd-marker", matched,
            match_excerpt(body, matched, rubric)
          ))
        }
        if (!nzchar(body)) {
          return(NULL)
        }
        finding(first[k], concealed_severity(matched), "html-comment", matched, match_excerpt(body, matched, rubric))
      }))
      for (k in seq_along(comments)) {
        covered[first[k]:last[k]] = TRUE
      }
    }

    for (i in which(is_hidden_markup(clean) & !covered)) {
      if (covered[i]) {
        next
      }
      end = hidden_region_end(clean, i)
      region = paste(clean[i:end], collapse = "\n")
      matched = match_injection_content(region, rubric)
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
# raised to medium, and otherwise dropped as program output.
#
# findings: Findings data frame for one file

resolve_weak_findings = function(findings) {
  weak = findings$detector == "visible-text" & findings$severity == "low"
  if (any(grepl("grading-output", findings$matched, fixed = TRUE))) {
    findings$severity[weak] = "medium"
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

  xml2::xml_remove(xml2::xml_find_all(doc, "//script | //style | //noscript"))

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
    (!is.na(class) & grepl(paste0("(^|\\s)(", hidden_class_names(), ")(\\s|$)"), class, perl = TRUE))
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
  text = scan_text_lines(visible, structural = FALSE, rubric = rubric)
  text$line = rep(NA_integer_, nrow(text))

  out = bind_findings(c(rows, list(text)))
  out[!duplicated(out[c("detector", "excerpt")]), , drop = FALSE]
}

# Text of the `<!-- markermd: ... -->` comments the fix-parsing skill added
# to a repository: lines added by commits that are on the grading-fixes
# branch and on no other local branch. A marker anywhere else was not put
# there by markermd. Empty for a directory that is not a git repository or
# has no grading-fixes branch.
#
# repo_dir: Repository directory

verified_repair_markers = function(repo_dir) {
  lookup = purrr::possibly(function() {
    branches = gert::git_branch_list(local = TRUE, repo = repo_dir)$name
    if (!"grading-fixes" %in% branches) {
      return(character(0))
    }
    fixes = gert::git_log(ref = "grading-fixes", max = 1000, repo = repo_dir)$commit
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
#' and meta text in the rendered HTML). It also reports agent instruction
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
#'   `invisible-unicode`, `bidi-control`, `encoded-text`, `markermd-marker`,
#'   `agent-file` or `file-name`. `matched` lists the content patterns it
#'   hit (`instruction-override`, `grader-addressed`, `grade-demand`,
#'   `role-marker`, `grading-output`, `score-line`, `rubric-text`), or `NA`. `severity` is
#'   `"high"` for a content match in a concealed place (and for smuggled
#'   Unicode), `"medium"` for a content match in visible text, an agent file
#'   or an unverified repair marker, and `"low"` for concealment with no
#'   content match. `excerpt` is a single line, truncated, with invisible
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
