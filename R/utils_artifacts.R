# Local artifact resolution helpers for the marking app
#
# The marking app displays a rendered HTML report ("artifact") for each repo.
# Reports are read from the project's configured `artifacts:` directories
# rather than downloaded; see resolve_repo_artifacts().

# Resolve each repo to a local rendered HTML report under the project's
# configured artifacts directories.
#
# Discovery is permissive: every HTML file that belongs to the repo is a
# candidate, where "belongs" means it sits in an artifacts directory as
# "<repo>.html" or "<repo><delimiter>*.html", or anywhere (recursively) under a
# subdirectory named "<repo>" or "<repo><delimiter>*". The delimiter must be
# non-alphanumeric, so repo "hw1" never claims "hw10.html" or "hw10_report/".
# A single candidate is accepted wherever it sits. Several candidates are
# ranked by artifact_candidates()'s heuristic, the best one is used, and one
# consolidated warning names each affected repo with its choice.
#
# project: markermd_project object (provides @root and @artifacts)
# repo_list: Character vector of repository names
#
# Returns a named character vector mapping each repo to the absolute path of
# its HTML report, or NA when no candidate is found, carrying a "candidates"
# attribute with every candidate per repo in ranked order (the app's
# ambiguity notice, artifact_ambiguity_note(), reads it).

resolve_repo_artifacts = function(project, repo_list) {
  artifact_dirs = fs::path(project@root, project@artifacts)
  artifact_dirs = artifact_dirs[fs::dir_exists(artifact_dirs)]

  ranked = lapply(repo_list, function(repo) {
    found = lapply(seq_along(artifact_dirs), function(i) {
      df = artifact_candidates(artifact_dirs[[i]], repo)
      if (!is.null(df)) df$dir_index = i
      df
    })
    found = do.call(rbind, found)
    if (is.null(found)) {
      return(character(0))
    }
    found = found[
      order(found$dir_index, found$tier, found$resource, found$decorated, found$depth, found$path),
      , drop = FALSE
    ]
    as.character(fs::path_real(found$path))
  })
  names(ranked) = repo_list

  paths = vapply(ranked, function(x) if (length(x) > 0) x[[1]] else NA_character_, character(1))
  attr(paths, "candidates") = ranked

  warn_ambiguous_artifacts(ranked, project@root)
  paths
}

# Keep the paths whose file name is repo, or repo followed by a
# non-alphanumeric delimiter
#
# paths: Character vector of paths
# repo: Repository name

prefixed_by_repo = function(paths, repo) {
  name = fs::path_file(paths)
  remainder = substring(name, nchar(repo) + 1L)
  paths[startsWith(name, repo) & grepl("^([^[:alnum:]]|$)", remainder)]
}

# Every HTML file in one artifacts directory that belongs to repo (see
# resolve_repo_artifacts()), with the keys its candidates are ranked on. The
# ranking is modeled on how reports actually arrive:
#
#   tier 1  "<dir>/<repo>.html", a report named for the repo
#   tier 2  a file directly inside "<dir>/<repo>/"
#   tier 3  "<dir>/<repo><delimiter>*.html", a decorated file name
#   tier 4  anything deeper. ghclass's action_artifact_download() (called by
#           org_grade_assignment()) extracts each GitHub Actions artifact into
#           "<dir>/<repo>_<artifact-name>/" by default, or
#           "<dir>/<repo>/<artifact-name>/" with nest = TRUE, so the report
#           sits one or more levels below a repo-named directory
#
# Within a tier, files under a rendering's resource directories ("*_files/",
# "libs/", "site_libs/") rank last since they are never the report, then an
# exact "<repo>/" directory beats a decorated sibling, shallower beats deeper,
# and the path breaks ties.
#
# dir: One artifacts directory
# repo: Repository name
#
# Returns a data frame (path, tier, resource, decorated, depth) or NULL when
# nothing belongs to the repo

artifact_candidates = function(dir, repo) {
  entries = prefixed_by_repo(fs::dir_ls(dir, recurse = FALSE), repo)
  files = entries[fs::is_file(entries) & fs::path_ext(entries) == "html"]
  dirs = entries[fs::is_dir(entries)]
  nested = if (length(dirs) > 0) {
    fs::dir_ls(dirs, recurse = TRUE, type = "file", glob = "*.html")
  } else {
    character(0)
  }
  paths = as.character(c(files, nested))
  if (length(paths) == 0) {
    return(NULL)
  }

  parts = fs::path_split(fs::path_rel(paths, dir))
  depth = lengths(parts)
  in_exact_dir = vapply(parts, function(p) length(p) > 1 && p[[1]] == repo, logical(1))
  resource = vapply(parts, function(p) {
    inner = p[-length(p)]
    any(grepl("_files$", inner) | inner %in% c("libs", "site_libs"))
  }, logical(1))

  tier = ifelse(
    depth == 1 & fs::path_file(paths) == paste0(repo, ".html"), 1L,
    ifelse(depth == 2 & in_exact_dir, 2L, ifelse(depth == 1, 3L, 4L))
  )

  data.frame(
    path = paths,
    tier = tier,
    resource = resource,
    decorated = depth > 1 & !in_exact_dir,
    depth = depth,
    stringsAsFactors = FALSE
  )
}

# One consolidated warning naming each repo that had more than one candidate
# report, the one chosen and how many others were passed over
#
# ranked: Named list of ranked candidate paths per repo
# root: Project root, so the message can show root-relative paths

warn_ambiguous_artifacts = function(ranked, root) {
  ambiguous = ranked[lengths(ranked) > 1]
  if (length(ambiguous) == 0) {
    return(invisible(NULL))
  }

  root = fs::path_real(root)
  bullets = vapply(names(ambiguous), function(repo) {
    chosen = fs::path_rel(ambiguous[[repo]][[1]], root)
    n_other = length(ambiguous[[repo]]) - 1L
    cli::format_inline("{.val {repo}}: using {.file {chosen}} ({n_other} other HTML file{?s} found)")
  }, character(1))
  # format_inline() output is final text; double any braces so cli_warn() does
  # not re-interpolate a path
  bullets = gsub("([{}])", "\\1\\1", bullets)

  cli::cli_warn(c(
    "Several HTML files were found for {length(ambiguous)} repositor{?y/ies}; the most likely report was chosen for each.",
    stats::setNames(bullets, rep("*", length(bullets))),
    "i" = "If the wrong report is shown, remove or rename the extra files under the project's artifacts directories."
  ))
}

# A small warning banner for the app when a repo had more than one candidate
# report (see resolve_repo_artifacts()), or NULL when there is nothing to say
#
# artifact_paths: Named character vector from resolve_repo_artifacts()
# repo: Repository name

artifact_ambiguity_note = function(artifact_paths, repo) {
  candidates = attr(artifact_paths, "candidates")[[repo]]
  if (length(candidates) <= 1) {
    return(NULL)
  }
  # Show the candidates relative to what they share, so same-directory
  # alternatives read as bare file names and cross-directory ones keep their
  # distinguishing prefix
  shown = as.character(fs::path_rel(candidates, fs::path_common(candidates)))

  shiny::div(
    class = "alert alert-warning py-1 px-2 mb-1 small",
    role = "alert",
    fa_icon("triangle-exclamation", class = "me-1"),
    glue::glue(
      "Several HTML files were found for this repository; showing \"{shown[[1]]}\". ",
      "Also found: {paste0('\"', shown[-1], '\"', collapse = ', ')}. ",
      "If this is the wrong report, remove or rename the extra files."
    )
  )
}

# Register each repo's artifact directory as a Shiny static resource path and
# return the URL each report is served under.
#
# Serving the reports (instead of injecting their text into the page) lets a
# non-self-contained report's sibling resources ("<name>_files/" figures, libs
# CSS/JS) resolve. The reports are student-produced, so they are shown in
# sandboxed iframes without allow-same-origin (see artifact_iframe()): their
# scripts run, but in an opaque origin that cannot reach the app's page,
# inputs or session. The grading pane's question highlight therefore runs
# inside the frame, from a script injected into a served copy of the report
# (artifact_frame_url()), driven by postMessage.
#
# artifact_paths: Named character vector from resolve_repo_artifacts()
#
# Returns a named character vector mapping each repo to its served URL, or NA
# when the repo has no artifact.

register_artifact_resources = function(artifact_paths) {
  urls = stats::setNames(rep(NA_character_, length(artifact_paths)), names(artifact_paths))
  for (i in seq_along(artifact_paths)) {
    path = artifact_paths[[i]]
    if (is.na(path)) next
    # Derive the resource prefix from the serving directory rather than the loop
    # position so a second mark() launch in the same R session re-registers each
    # prefix to the same directory instead of silently rebinding it.
    prefix = paste0("markermd_artifact_", substr(rlang::hash(dirname(path)), 1, 16))
    shiny::addResourcePath(prefix, dirname(path))
    urls[[i]] = paste0(prefix, "/", utils::URLencode(basename(path), reserved = TRUE))
  }
  urls
}

# Open a folder in the system file manager (cross-platform)
#
# folder_path: Path to folder to open

open_folder = function(folder_path) {
  if (!dir.exists(folder_path)) {
    return(FALSE)
  }

  cmd = switch(
    Sys.info()[["sysname"]],
    "Darwin" = "open",
    "Windows" = "explorer",
    "xdg-open"
  )
  status = suppressWarnings(
    system2(cmd, shQuote(folder_path), stdout = FALSE, stderr = FALSE)
  )
  # explorer.exe exits non-zero even when it successfully opens the folder, so
  # its status is not authoritative; every other launcher reports 0 on success.
  cmd == "explorer" || identical(as.integer(status), 0L)
}

# The sandbox attribute every report frame carries: scripts run (htmlwidgets,
# MathJax, Quarto's own scripts), links may open new tabs and forms work,
# but without allow-same-origin the frame is cross-origin to the app, so a
# script in a report cannot reach the grading page, its inputs or its
# session

artifact_sandbox = "allow-scripts allow-popups allow-forms allow-downloads"

# A sandboxed iframe showing a served report
#
# src: The report's URL (register_artifact_resources() or artifact_frame_url())
# ...: Further attributes (id, class, style, onload)

artifact_iframe = function(src, ...) {
  shiny::tags$iframe(src = src, sandbox = artifact_sandbox, referrerpolicy = "no-referrer", ...)
}

# The directory the grading pane's report copies are served from (one per R
# session, registered under the markermd_artifact_frames prefix)

artifact_frame_dir = function() {
  dir = file.path(tempdir(), "markermd_artifact_frames")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    shiny::addResourcePath("markermd_artifact_frames", dir)
  }
  dir
}

# The URL of the grading pane's copy of a report: the HTML file with a
# <base> pointing at the served original directory (so its relative
# resources still resolve) and the highlight script (artifact_frame_script())
# injected into its head. The copy is keyed by the file's path and
# modification time, so a re-rendered report gets a fresh copy and an
# unchanged one is written once per session.
#
# path: The report's local path
# url: The URL the original is served under (register_artifact_resources())
# Returns: The copy's URL

artifact_frame_url = function(path, url) {
  dir = artifact_frame_dir()
  key = substr(rlang::hash(list(normalizePath(path, winslash = "/"), file.mtime(path))), 1, 16)
  copy = file.path(dir, paste0(key, ".html"))
  if (!file.exists(copy)) {
    html = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # The copy lives one directory down from the original's prefix, so the
    # base is a sibling path; a root-relative one would break under a proxy
    # that serves the app below a path
    injected = paste0(
      "<base href=\"../", dirname(url), "/\">\n",
      "<script>", artifact_frame_script(), "</script>\n"
    )
    head = regexpr("<head[^>]*>", html, ignore.case = TRUE)
    html = if (head > 0) {
      at = head + attr(head, "match.length")
      paste0(substr(html, 1, at - 1), "\n", injected, substr(html, at, nchar(html)))
    } else {
      paste0(injected, html)
    }
    writeLines(enc2utf8(html), copy, useBytes = TRUE)
  }
  paste0("markermd_artifact_frames/", basename(copy))
}

# The script injected into the grading pane's copy of a report: on a
# markermd-highlight message from the embedding page it clears the previous
# highlight, scrolls to the first target and wraps every target (a heading
# with its section, or an id'd div) in a highlighted block. Targets are found
# by data-anchor-id, then element id, then heading text. The copy carries a
# <base> for its resources, which would send the report's own "#section"
# links (its table of contents) to the base URL, so those are scrolled to
# in place instead. Nothing here reads the embedding page: the frame is
# sandboxed and cross-origin.

artifact_frame_script = function() {
  "(function() {
    var STYLE = '.section-highlight-wrapper { background-color: rgba(255, 235, 59, 0.15); border-left: 3px solid rgba(255, 193, 7, 0.8); padding: 8px; padding-left: 12px; margin: 8px 0; }' +
      '.section-highlight-wrapper > * { margin-top: 0; }' +
      '.section-highlight-wrapper > *:not(:last-child) { margin-bottom: 1rem; }' +
      '.section-highlight-wrapper > *:last-child { margin-bottom: 0; }';
    function ensureStyle() {
      if (document.getElementById('markermd-highlight-style')) return;
      var st = document.createElement('style');
      st.id = 'markermd-highlight-style';
      st.textContent = STYLE;
      (document.head || document.documentElement).appendChild(st);
    }
    function clearHighlights() {
      document.querySelectorAll('.section-highlight-wrapper').forEach(function(wrapper) {
        var parent = wrapper.parentNode;
        while (wrapper.firstChild) parent.insertBefore(wrapper.firstChild, wrapper);
        parent.removeChild(wrapper);
      });
    }
    function findTarget(target) {
      var el = document.querySelector('[data-anchor-id=\"' + target.id + '\"]') || document.getElementById(target.id);
      if (!el && target.text) {
        var headings = document.querySelectorAll('h1, h2, h3, h4, h5, h6');
        for (var i = 0; i < headings.length; i++) {
          if (headings[i].textContent.trim().toLowerCase() === target.text.toLowerCase()) { el = headings[i]; break; }
        }
      }
      return el;
    }
    function highlight(targets) {
      ensureStyle();
      clearHighlights();
      var first = targets.length > 0 ? findTarget(targets[0]) : null;
      if (first) {
        var rect = first.getBoundingClientRect();
        window.scrollTo({ top: window.scrollY + rect.top - 20, behavior: 'smooth' });
      }
      targets.forEach(function(target) {
        var el = findTarget(target);
        if (!el) return;
        // A heading selects its whole section (its siblings until the next
        // same-or-higher heading); a rendered div already contains its
        // children, so wrap just the div element
        var toWrap = [el];
        if (el.tagName && el.tagName.match(/^H[1-6]$/)) {
          var level = parseInt(el.tagName.charAt(1));
          var cur = el.nextElementSibling;
          while (cur) {
            if (cur.tagName && cur.tagName.match(/^H[1-6]$/) && parseInt(cur.tagName.charAt(1)) <= level) break;
            toWrap.push(cur);
            cur = cur.nextElementSibling;
          }
        }
        var wrapper = document.createElement('div');
        wrapper.className = 'section-highlight-wrapper';
        el.parentNode.insertBefore(wrapper, el);
        toWrap.forEach(function(node) { wrapper.appendChild(node); });
      });
    }
    document.addEventListener('click', function(e) {
      var link = e.target.closest && e.target.closest('a[href^=\"#\"]');
      if (!link) return;
      var id = decodeURIComponent(link.getAttribute('href').slice(1));
      if (!id) return;
      var el = document.getElementById(id) || document.querySelector('[name=\"' + id + '\"]');
      if (!el) return;
      e.preventDefault();
      el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    });
    var pending = null;
    window.addEventListener('message', function(e) {
      var msg = e.data;
      if (!msg || msg.type !== 'markermd-highlight' || !Array.isArray(msg.targets)) return;
      if (document.readyState === 'loading') { pending = msg.targets; return; }
      highlight(msg.targets);
    });
    document.addEventListener('DOMContentLoaded', function() {
      if (pending) { highlight(pending); pending = null; }
    });
  })();"
}
