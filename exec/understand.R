#!/usr/local/bin/Rscript

require(kaiaulu, quietly = TRUE)
require(cli, quietly = TRUE)
require(XML, quietly = TRUE)
require(stringi, quietly = TRUE)
require(data.table, quietly = TRUE)

doc <- "
USAGE:
  understand.R (-h | -help)
  understand.R --version
  understand.R build help
  understand.R build <config_filepath.yml> <tool_filepath.yml>
  understand.R export help
  understand.R export <config_filepath.yml> <tool_filepath.yml> <save_path> [--class | --file]
  understand.R parse help
  understand.R parse <config_filepath.yml> <tool_filepath.yml> <save_path> [--class | --file]

DESCRIPTION:
  Builds then analyzes a project using Scitool's Understand for dependencies between either classes or files.

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
  --class       parses class-level dependencies
  --file        parses file-level dependencies
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

if (!arguments[["help"]]) {
  tool <- parse_config(arguments[["<tool_filepath.yml>"]])
  scitools_path <- get_tool_project("scitools", tool)

  conf <- parse_config(arguments[["<config_filepath.yml>"]])
  keep_dependencies_type <- get_understand_keep_dependencies_type(conf)
  project_path <- get_understand_project_path(conf)

  # Scitools
  understand_folder <- get_understand_output_path(conf)
  code_language <- get_understand_code_language(conf)

  db_path <- stringi::stri_c(understand_folder,"Understand.und")

  file_dependencies_path <- stringi::stri_c(understand_folder,"file_dependencies.xml")
  class_dependencies_path <- stringi::stri_c(understand_folder,"class_dependencies.xml")

  save_path <- arguments[["<save_path>"]]
  node_file <- paste0(save_path, "/node_list.csv")
  edge_file <- paste0(save_path, "/edge_list.csv")
}

# Determine which function to run and save output
if (arguments[["build"]] & arguments[["help"]]) {
  cli::cli_alert_info("Builds an analysis of the project in the designated project_path set in the <config_filepath.yml> using build_understand_project()")
} else if (arguments[["export"]] & arguments[["help"]]) {
  cli::cli_alert_info("From the built analysis, exports the dependency types of either files or classes (dependening on supplied flag) using export_understand_dependencies() and saves two xmls at the specified folder: <save_path>")
} else if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("From the built analysis, parses the dependency types of either files or classes (dependening on supplied flag) using parse_understand_dependencies() and saves two tables at the specified folder: <save_path>")
} else if (arguments[["build"]]) {
  db_path <- build_understand_project(scitools_path = scitools_path, project_path = project_path, language = code_language, output_dir = understand_folder)
  cli::cli_alert_success("Project sucessfully built.")
} else if (arguments[["export"]]) {
  if (arguments[["--file"]]) {
    result <- export_understand_dependencies(scitools_path = scitools_path, db_filepath = db_path, parse_type = "file", output_filepath = file_dependencies_path)
  } else if (arguments[["--class"]]) {
    result <- export_understand_dependencies(scitools_path = scitools_path, db_filepath = db_path, parse_type = "class", output_filepath = class_dependencies_path)
  } else {
    stop("No/invalid option(s) provided.")
  }
  cli::cli_alert_success(paste0("Dependencies xml was saved at: ", save_path))
} else if (arguments[["parse"]]) {
  if (arguments[["--file"]]) {
    result <- parse_understand_dependencies(dependencies_path = file_dependencies_path)
  } else if (arguments[["--class"]]) {
    result <- parse_understand_dependencies(dependencies_path = class_dependencies_path)
  } else {
    stop("No/invalid option(s) provided.")
  }
  data.table::fwrite(result$node_list, node_file)
  data.table::fwrite(result$edge_list, edge_file)
  cli::cli_alert_success(paste0("Dependencies table was saved at: ", save_path))
} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}
