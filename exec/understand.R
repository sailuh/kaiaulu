#!/usr/local/bin/Rscript

require(kaiaulu, quietly = TRUE)
require(cli, quietly = TRUE)
require(XML, quietly = TRUE)
require(stringi, quietly = TRUE)
require(data.table, quietly = TRUE)

# For building, should I create a separate executable? Or put it in as an option? Currently have it as a separate usage.
doc <- "
USAGE:
  understand.R (-h | -help)
  understand.R --version
  understand.R build help
  understand.R build <config_filepath>
  understand.R parse help
  understand.R parse <config_filepath> <save_path> [--class | --file]

DESCRIPTION:
  Builds then analyzes a project using Scitool's Understand for dependencies between either classes or files.

OPTIONS:
  --class   parses class-level dependencies
  --file    parses file-level dependencies
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

# Currently unsure how variables would work

if (!arguments[["help"]]) {
  conf <- parse_config(arguments[["<config_filepath>"]])
  project_path <- get_understand_project_path(conf)
  understand_folder <- get_understand_output_path(conf)
  code_language <- get_understand_code_language(conf)
}

# Ensuring directory exists for output
if (!dir.exists(understand_folder) & (arguments[["build"]] | arguments[["parse"]])) {
  dir.create(understand_folder, recursive = TRUE)
}

# Determine which function to run and save output
if (arguments[["build"]] & arguments[["help"]]) {
  cli::cli_alert_info("Builds an analysis of the project in the designated project_path set in the <config_filepath> using build_understand_project()")
} else if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("From the built analysis, parses the dependency types of either files or classes (dependening on supplied flag) using parse_understand_dependencies() and saves it at <save_path>")
} else if (arguments[["build"]]) {
  build_understand_project(project_path = project_path, language = code_language, output_dir = understand_folder)
  cli::cli_alert_success("Project sucessfully built.")
} else if (arguments[["parse"]]) {
  if (arguments[["--file"]]) {
    result <- parse_understand_dependencies(understand_dir = understand_folder, parse_type = "file")
  } else if (arguments[["--class"]]) {
    result <- parse_understand_dependencies(understand_dir = understand_folder, parse_type = "class")
  }
  data.table::fwrite(result, save_path)
  cli::cli_alert_success(paste0("Dependencies table was saved at: ", save_path))
} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}

# Unsure how to close up the executable at the moment, but result is a data.table
