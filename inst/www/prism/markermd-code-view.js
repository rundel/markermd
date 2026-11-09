/*
 * markermd's read-only code view, on Prism.
 *
 * Every element with the class markermd-code-view holds a <pre><code> with
 * the file's plain text (readable as is when this script is missing). This
 * script highlights it with Prism in the language named by data-language,
 * then splits the highlighted HTML into one .mm-line element per source
 * line. Line numbers are drawn on those elements and a question's highlight
 * is a class on them (.mm-hl), so the band always sits exactly on its lines,
 * wrapped lines included. data-highlight holds the ranges as JSON,
 * [{"start": 3, "end": 5}, ...]; the first highlighted line is scrolled near
 * the top of the view.
 *
 * Views are rendered as they enter the page (a MutationObserver covers
 * renderUI output and modals), and the markermd_code_view_highlight message
 * ({id, ranges}) replaces a rendered view's highlight in place.
 */
(function () {
  "use strict";

  // Longer texts are shown plain: Prism highlights synchronously
  var MAX_HIGHLIGHT_CHARS = 400000;

  function escapeHtml(text) {
    return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  // Split highlighted HTML into lines, closing the open token spans at each
  // line break and reopening them on the next line, so every line is valid
  // HTML on its own
  function splitLines(html) {
    var lines = [];
    var current = "";
    var open = [];
    var re = /(<span[^>]*>)|(<\/span>)|([^<]+)/g;
    var m;
    while ((m = re.exec(html)) !== null) {
      if (m[1]) {
        open.push(m[1]);
        current += m[1];
      } else if (m[2]) {
        open.pop();
        current += m[2];
      } else {
        var parts = m[3].split("\n");
        for (var i = 0; i < parts.length; i++) {
          if (i > 0) {
            for (var j = 0; j < open.length; j++) current += "</span>";
            lines.push(current);
            current = open.join("");
          }
          current += parts[i];
        }
      }
    }
    lines.push(current);
    return lines;
  }

  function parseRanges(value) {
    if (!value) return [];
    try {
      var ranges = JSON.parse(value);
      return Array.isArray(ranges) ? ranges : [];
    } catch (e) {
      return [];
    }
  }

  function lineHeight(view) {
    var line = view.querySelector(".mm-line");
    return line ? line.getBoundingClientRect().height || 18 : 18;
  }

  // Mark the lines in `ranges` and scroll the first one near the top
  function applyHighlight(view, ranges, behavior) {
    var lines = view.querySelectorAll(".mm-line");
    for (var i = 0; i < lines.length; i++) lines[i].classList.remove("mm-hl");
    var first = null;
    ranges.forEach(function (range) {
      for (var n = range.start; n <= range.end; n++) {
        var line = lines[n - 1];
        if (!line) continue;
        line.classList.add("mm-hl");
        if (!first || n < Number(first.getAttribute("data-n"))) first = line;
      }
    });
    var top = first ? Math.max(0, first.offsetTop - 2 * lineHeight(view)) : 0;
    if (typeof view.scrollTo === "function") {
      view.scrollTo({ top: top, behavior: behavior || "auto" });
    } else {
      view.scrollTop = top;
    }
  }

  function render(view) {
    var code = view.querySelector("code");
    if (!code) return;
    var text = code.textContent;
    var language = view.getAttribute("data-language") || "none";
    var Prism = window.Prism;
    var grammar = Prism && Prism.languages && Prism.languages[language];
    var html = grammar && text.length <= MAX_HIGHLIGHT_CHARS ?
      Prism.highlight(text, grammar, language) :
      escapeHtml(text);
    var lines = splitLines(html);
    view.style.setProperty("--mm-gutter", (String(lines.length).length + 1) + "ch");
    code.innerHTML = lines.map(function (line, i) {
      return '<span class="mm-line" data-n="' + (i + 1) + '">' + line + "</span>";
    }).join("");
    view.setAttribute("data-rendered", "true");
    applyHighlight(view, parseRanges(view.getAttribute("data-highlight")), "auto");
  }

  function renderWithin(root) {
    if (!root || !root.querySelectorAll) return;
    if (root.matches && root.matches(".markermd-code-view:not([data-rendered])")) render(root);
    var views = root.querySelectorAll(".markermd-code-view:not([data-rendered])");
    for (var i = 0; i < views.length; i++) render(views[i]);
  }

  function start() {
    renderWithin(document.body);
    new MutationObserver(function (mutations) {
      mutations.forEach(function (mutation) {
        mutation.addedNodes.forEach(function (node) {
          if (node.nodeType === 1) renderWithin(node);
        });
      });
    }).observe(document.body, { childList: true, subtree: true });

    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler("markermd_code_view_highlight", function (msg) {
        var view = document.getElementById(msg.id);
        if (!view) return;
        var ranges = msg.ranges || [];
        view.setAttribute("data-highlight", JSON.stringify(ranges));
        if (view.getAttribute("data-rendered") === "true") applyHighlight(view, ranges, "smooth");
      });
    }
  }

  window.markermdCodeView = { render: render, splitLines: splitLines, applyHighlight: applyHighlight };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", start);
  } else {
    start();
  }
})();
