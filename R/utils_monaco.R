# Build the JavaScript used to initialize a read-only Monaco editor
#
# This produces the editor-initialization script shared by the marking and
# validation modules. The generated script loads Monaco Editor from a CDN if
# necessary, disposes of any existing editor in the target container, and
# creates a new read-only editor with the supplied content and language.
#
# editor_id: Character. The id of the container div that will host the editor
# content: Character. The text content to display in the editor
# language: Character. The Monaco editor language identifier (e.g. "r", "yaml")

monaco_editor_config = function(editor_id, content, language) {
  content_json = jsonlite::toJSON(content, auto_unbox = TRUE)

  glue::glue(
    "
    (function() {
      // Load Monaco Editor if not already loaded
      if (typeof monaco === 'undefined') {
        var script = document.createElement('script');
        script.src = 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs/loader.js';
        script.onload = function() {
          require.config({ paths: { vs: 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs' } });
          require(['vs/editor/editor.main'], function() {
            createEditor();
          });
        };
        document.head.appendChild(script);
      } else {
        createEditor();
      }

      function createEditor() {
        // Clean up any existing editor
        var existingContainer = document.getElementById('<<editor_id>>');
        if (existingContainer && existingContainer.editor) {
          existingContainer.editor.dispose();
        }

        // Create the editor
        var editor = monaco.editor.create(document.getElementById('<<editor_id>>'), {
          value: <<content_json>>,
          language: '<<language>>',
          theme: 'vs',
          readOnly: true,
          wordWrap: 'on',
          wrappingIndent: 'indent',
          fontSize: 12,
          lineNumbers: 'on',
          minimap: { enabled: false },
          scrollBeyondLastLine: false,
          automaticLayout: true,
          contextmenu: false,
          selectOnLineNumbers: false
        });

        // Store reference for cleanup
        document.getElementById('<<editor_id>>').editor = editor;
      }
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

render_monaco_editor = function(editor_id, content, language) {
  shinyjs::runjs(monaco_editor_config(editor_id, content, language))
}
