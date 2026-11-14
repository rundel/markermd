# Prism (vendored)

markermd's read-only code views (the template editor's file view and node previews, and `mark()`'s Source view, source modal and file view) use [Prism](https://prismjs.com) 1.30.0, vendored here so the apps work offline. Prism is MIT licensed (see `LICENSE`).

| File                     | Origin                                                                                   |
|--------------------------|------------------------------------------------------------------------------------------|
| `prism-core.min.js`      | Prism 1.30.0, `components/prism-core.min.js`                                             |
| `components/*.min.js`    | Prism 1.30.0 language components                                                         |
| `LICENSE`                | Prism's MIT license                                                                      |
| `prism-config.js`        | markermd: sets `Prism.manual` before the core loads so Prism does not scan the page      |
| `markermd-qmd.js`        | markermd: the `qmd` grammar (alias `rmd`) for Quarto and R Markdown documents             |
| `markermd-code-view.js`  | markermd: renders `.markermd-code-view` elements, line numbers and question highlights   |
| `markermd-code-view.css` | markermd: layout, line numbers, the highlight band and the token colors                  |

`code_view_dependency()` in `R/utils_code_view.R` serves these files, loading the components in the order of `prism_components()`. A component must come after the components it extends (for example `clike` before `javascript`, `yaml` before `markdown`, `markup` before `markdown`).

## Updating Prism

1. Set the new version in `prism_version()` in `R/utils_code_view.R`.
2. Download `components/prism-core.min.js` and each component in `prism_components()` from `https://cdnjs.cloudflare.com/ajax/libs/prism/<version>/components/`, replacing the files here, and the release's `LICENSE` from the Prism repository.
3. To add a language, add its component name to `prism_components()` (after anything it depends on, per Prism's `components.json`) and map file extensions or chunk engines to it in `file_code_language()` / `code_language_for_engine()`, and in `ENGINES` in `markermd-qmd.js`.
4. Run the code view tests: `test-code-view.R` (the grammar, through V8) and the browser tests that show code (`test-mark-coverage.R`, `test-template-app_files.R`).
