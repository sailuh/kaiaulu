#!/usr/local/bin/Rscript

require(kaiaulu, quietly = TRUE)
require(XML, quietly = TRUE)
require(stringi, quietly = TRUE)
require(data.table, quietly = TRUE)

# For building, should I create a separate executable? Or put it in as an option? Currently have it as a separate usage.
doc <- "
USAGE:
  understand.R help
  understand.R build <project_filepath> <code_language> <output_dir>
  understand.R parse <build_filepath> [--class | --file]

DESCRIPTION:
  Builds then analyzes a project using Scitool's Understand for dependencies between either classes or files.

OPTIONS:
  --class   parses class-level dependencies
  --file    parses file-level dependencies
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')

# Currently unsure how variables would work

project_path <- ""
understand_folder <- ""
code_language <- ""

if (arguments[["build"]]) {
  project_path <- arguments[["<project_filepath>"]]
  understand_folder <- arguments[["<output_dir>"]]
  code_language <- arguments[["<code_language>"]]
} else if (arguments[["parse"]]) {
  understand_folder <- arguments[["<build_filepath>"]]
}

# Ensuring directory exists for output
if (!dir.exists(understand_folder) & (arguments[["build"]] | arguments[["parse"]])) {
  dir.create(understand_folder, recursive = TRUE)
}

# Determine which function to run and save output
if (arguments[["build"]]) {
  build_understand_project(project_path = project_path, language = code_language, output_dir = understand_folder)
} else if (arguments[["parse"]]) {
  if (arguments[["--file"]]) {
    result <- parse_understand_dependencies(understand_dir = understand_folder, parse_type = "file")
  } else if (arguments[["--class"]]) {
    result <- parse_understand_dependencies(understand_dir = understand_folder, parse_type = "classs")
  }
} else {
  stop("No/invalid option(s) provided.")
}

# Unsure how to close up the executable at the moment, but result is a data.table
