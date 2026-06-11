# Convert highlight line ranges into Monaco decoration objects
#
# Whole-line yellow highlight plus an overview-ruler marker so highlighted
# ranges stay findable in the scrollbar after scrolling away. The
# .highlight-line / .highlight-margin classes are styled in the marking app's
# stylesheet.
#
# highlight_ranges: List of list(start =, end =) line ranges (or NULL)

monaco_line_decorations = function(highlight_ranges) {
  if (is.null(highlight_ranges) || length(highlight_ranges) == 0) {
    return(list())
  }

  lapply(highlight_ranges, function(range) {
    list(
      range = list(
        startLineNumber = range$start,
        startColumn = 1,
        endLineNumber = range$end,
        endColumn = 1
      ),
      options = list(
        isWholeLine = TRUE,
        className = "highlight-line",
        marginClassName = "highlight-margin",
        # 7 = monaco.editor.OverviewRulerLane.Full
        overviewRuler = list(color = "#ffc107", position = 7)
      )
    )
  })
}

# Build the JavaScript used to initialize a read-only Monaco editor
#
# This produces the single editor-initialization script shared by every Monaco
# surface (template/mark preview modals, the mark source modal, and the rubric
# Source view). The generated script:
#  - loads Monaco from the CDN if necessary; if that fails (offline, blocking
#    proxy) it falls back to rendering the content as a plain monospace block
#    so the text is always readable,
#  - disposes any previously registered editor for the same id (containers
#    created by renderUI/modals are replaced wholesale, so without this each
#    render would leak an editor instance),
#  - registers the editor in window.monacoEditors so later decoration updates
#    can find it, and
#  - applies the supplied line decorations, revealing the first highlighted
#    line.
#
# editor_id: Character. The id of the container div that will host the editor
# content: Character. The text content to display in the editor
# language: Character. The Monaco editor language identifier (e.g. "r", "yaml")
# font_size: Numeric. Editor font size in px
# decorations: List of Monaco decoration objects (see monaco_line_decorations())

monaco_editor_config = function(editor_id, content, language, font_size = 12, decorations = list()) {
  content_json = jsonlite::toJSON(content, auto_unbox = TRUE)
  decorations_json = jsonlite::toJSON(decorations, auto_unbox = TRUE)

  glue::glue(
    "
    (function() {
      function showFallback() {
        var container = document.getElementById('<<editor_id>>');
        if (!container) return;
        var pre = document.createElement('pre');
        pre.className = 'font-monospace small bg-light m-0 p-2 overflow-auto';
        pre.style.height = '100%';
        pre.textContent = <<content_json>>;
        container.replaceChildren(pre);
      }

      function ensureMonaco(cb) {
        if (typeof monaco !== 'undefined') { cb(); return; }
        var script = document.createElement('script');
        script.src = 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs/loader.js';
        script.onload = function() {
          require.config({ paths: { vs: 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs' } });
          require(['vs/editor/editor.main'], cb, showFallback);
        };
        script.onerror = showFallback;
        document.head.appendChild(script);
      }

      ensureMonaco(function() {
        var container = document.getElementById('<<editor_id>>');
        if (!container) return;

        if (!window.monacoEditors) window.monacoEditors = {};

        var editor = container.editor;
        if (editor) {
          // Same container re-rendered: swap content/language in place
          editor.setValue(<<content_json>>);
          monaco.editor.setModelLanguage(editor.getModel(), '<<language>>');
        } else {
          // A previous editor with this id lives in a detached container
          // (renderUI/modal replaced it); dispose it before creating anew
          var prev = window.monacoEditors['<<editor_id>>'];
          if (prev) { try { prev.dispose(); } catch (e) {} }

          editor = monaco.editor.create(container, {
            value: <<content_json>>,
            language: '<<language>>',
            theme: 'vs',
            readOnly: true,
            wordWrap: 'on',
            wrappingIndent: 'same',
            fontSize: <<font_size>>,
            lineNumbers: 'on',
            minimap: { enabled: false },
            scrollBeyondLastLine: false,
            automaticLayout: true,
            contextmenu: false,
            selectOnLineNumbers: false
          });
          container.editor = editor;
          window.monacoEditors['<<editor_id>>'] = editor;
        }

        var decorations = <<decorations_json>>;
        editor._currentDecorationIds = editor.deltaDecorations(editor._currentDecorationIds || [], decorations);
        if (decorations.length > 0) {
          editor.revealLineNearTop(decorations[0].range.startLineNumber, monaco.editor.ScrollType.Immediate);
        } else {
          editor.setScrollTop(0);
          editor.revealLine(1);
        }
      });
    })();
    ",
    .open = "<<",
    .close = ">>"
  )
}

# Render a read-only Monaco editor into an existing container div
#
# Runs the editor-initialization JavaScript produced by monaco_editor_config()
# in the client browser via shinyjs.
#
# editor_id: Character. The id of the container div that will host the editor
# content: Character. The text content to display in the editor
# language: Character. The Monaco editor language identifier (e.g. "r", "yaml")
# font_size: Numeric. Editor font size in px
# decorations: List of Monaco decoration objects (see monaco_line_decorations())

render_monaco_editor = function(editor_id, content, language, font_size = 12, decorations = list()) {
  shinyjs::runjs(monaco_editor_config(editor_id, content, language, font_size = font_size, decorations = decorations))
}

# Build the JavaScript that replaces the line decorations on an already
# registered Monaco editor, retrying briefly while the editor is still being
# created, and revealing the first highlighted line.
#
# editor_id: Character. The id the editor was registered under
# decorations: List of Monaco decoration objects (see monaco_line_decorations())

monaco_update_decorations_js = function(editor_id, decorations) {
  decorations_json = jsonlite::toJSON(decorations, auto_unbox = TRUE)

  glue::glue(
    "
    (function update(retries) {
      retries = retries || 0;
      var editor = window.monacoEditors && window.monacoEditors['<<editor_id>>'];
      if (!editor) {
        if (retries < 25) setTimeout(function() { update(retries + 1); }, 200);
        return;
      }
      var decorations = <<decorations_json>>;
      editor._currentDecorationIds = editor.deltaDecorations(editor._currentDecorationIds || [], decorations);
      if (decorations.length > 0) {
        editor.revealLineNearTop(decorations[0].range.startLineNumber, monaco.editor.ScrollType.Smooth);
      }
    })(0);
    ",
    .open = "<<",
    .close = ">>"
  )
}
