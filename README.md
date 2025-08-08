# markermd

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/rundel/markermd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rundel/markermd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> **Interactive Shiny-Based Grading Interface for R Markdown Assignments**

markermd provides a comprehensive Shiny-based interface for grading assignments submitted as git repositories containing R Markdown or Quarto documents. It features template creation with validation rules, interactive AST tree visualization, question-based grading workflows, and automated validation systems.

## ⚠️ Experimental Status

This package is currently in **experimental** development. APIs may change significantly, and some features may not be fully stable. Use with caution in production environments.

## Key Features

- 📝 **Template Creation**: Interactive interface for creating grading templates with validation rules
- 🌳 **AST Visualization**: Explore document structure through interactive abstract syntax trees
- ✅ **Automated Validation**: Define and apply validation rules to check assignment requirements  
- 📊 **Question-Based Grading**: Organize grading around specific questions with customizable rubrics
- ⌨️ **Hotkey Support**: Fast grading workflows with keyboard shortcuts
- 🔗 **GitHub Integration**: Support for both local directories and remote GitHub repositories
- 🔄 **Auto Sync**: Automatic syncing of GitHub classroom artifacts
- 📋 **Export Options**: Export grades and feedback in various formats

## Installation

You can install the development version of markermd from GitHub:

```r
# Install from GitHub
# install.packages("pak")
pak::pak("rundel/markermd")

# Or using remotes
# install.packages("remotes")
remotes::install_github("rundel/markermd")
```

### Dependencies

markermd relies on the development version of [parsermd](https://github.com/rundel/parsermd) for parsing R Markdown and Quarto documents:

```r
pak::pak("rundel/parsermd")
```

## Quick Start

### Template Creation

Create grading templates with validation rules:

```r
library(markermd)

# Launch template creation interface
template()
```

### Grading Interface  

Grade a collection of assignment repositories:

```r
# Grade assignments in a local directory
mark("/path/to/assignments/")

# Grade with a pre-built template
mark("/path/to/assignments/", template = "my_template.rds")

# Grade Rmd files instead of qmd (default is qmd)
mark("/path/to/assignments/", use_qmd = FALSE)
```

## Core Workflows

### 1. Template Creation (`template()`)

The template creation interface allows you to:

- Load and explore assignment documents through interactive AST trees
- Select specific nodes (headings, code chunks, etc.) for each question
- Define validation rules (content checks, node counts, etc.)
- Save reusable templates for consistent grading

### 2. Assignment Grading (`mark()`)

The grading interface provides:

- **Validation Tab**: Automated checking of assignment requirements
- **Rubric Tab**: Question-by-question grading with customizable point values
- **Marking Tab**: Detailed feedback and scoring workflows

## Repository Structure

markermd expects assignments to be organized as:

```
assignments/
├── student1-repo/
│   ├── assignment.qmd  # or .Rmd
│   └── ...
├── student2-repo/
│   ├── assignment.qmd
│   └── ...
└── ...
```

For GitHub Classroom integration, the package can automatically:
- Clone repositories from GitHub
- Download and cache rendered artifacts  
- Sync updates from remote repositories

## Configuration

### File Type Selection

Choose between Quarto (.qmd) and R Markdown (.Rmd):

```r
# Parse .qmd files (default)
mark("/path/to/assignments/", use_qmd = TRUE)

# Parse .Rmd files  
mark("/path/to/assignments/", use_qmd = FALSE)
```

### Template Validation

Templates define validation rules that can check:

- **Node counts**: Minimum/maximum number of headings, code chunks, etc.
- **Content presence**: Required text patterns or keywords
- **Content absence**: Forbidden text or patterns  
- **Node naming**: Specific names for code chunks or sections

## Examples

### Basic Template Creation

```r
# Start with a sample assignment
template("/path/to/sample/assignment.qmd")

# 1. Explore the document structure in the AST tree
# 2. Select nodes for each question
# 3. Add validation rules
# 4. Save template for reuse
```

### Automated Grading Workflow

```r
# Load template and grade collection
template_obj <- readRDS("grading_template.rds")
mark("/path/to/student/repos/", template = template_obj)

# View validation results and grade interactively
```

## Screenshots

### Template Creation Interface

![Template Creation Interface](man/figures/template_screenshot.png)

The template interface shows:
- Document AST tree for node selection
- Question management panel  
- Validation rule configuration
- Template save/load functionality

### Grading Interface

#### Validation Tab
![Grading Validation Interface](man/figures/mark_validation_screenshot.png)

#### Rubric Tab  
![Grading Rubric Interface](man/figures/mark_rubric_screenshot.png)

The grading interface provides:
- Repository overview with validation status
- Question-based rubric grading
- Content viewing with highlighting
- Progress tracking across assignments

## Contributing

markermd is in active development. Contributions, bug reports, and feature requests are welcome!

## License

GPL (>= 3)