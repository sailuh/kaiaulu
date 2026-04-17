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
  understand.R build <tools.yml> <project_conf.yml>
  understand.R export help
  understand.R export <tools.yml> <project_conf.yml> <save_path> (--class | --file)
  understand.R parse help
  understand.R parse <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path> (--class | --file)

DESCRIPTION:
  Builds then analyzes a project using Scitool's Understand for dependencies between either classes or files. Please see Kaiaulu's README.md for instructions on how to create <tool.yml> and <project_conf.yml>.

COMMANDS:
  build                         Builds an analysis of the project in the designated project_path set in the <project_conf.yml>
  export                        Exports the dependencies to an xml
  parse.                        Parses the dependencies and saves them to a node csv and an edge csv

ARGUMENTS:
  <tools.yml>                   path to tools.yml file
  <project_conf.yml>            path to configuration file for project you want to analyze
  <save_path>                   file path where output will be saved
  <node_save_file_name_path>    file path where csv of nodes of the network will be saved
  <edge_save_file_name_path>    file path where csv of edges of the network will be saved

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
  --class       parses/exports class-level dependencies
  --file        parses/exports file-level dependencies
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

if (!arguments[["help"]]) {
  tool <- parse_config(arguments[["<tools.yml>"]])
  scitools_path <- get_tool_project("scitools", tool)

  conf <- parse_config(arguments[["<project_conf.yml>"]])
  keep_dependencies_type <- get_understand_keep_dependencies_type(conf)
  project_path <- get_understand_project_path(conf)

  # Scitools
  understand_folder <- get_understand_output_path(conf)
  code_language <- get_understand_code_language(conf)

  db_path <- stringi::stri_c(understand_folder,"Understand.und")

  file_dependencies_path <- stringi::stri_c(understand_folder,"file_dependencies.xml")
  class_dependencies_path <- stringi::stri_c(understand_folder,"class_dependencies.xml")

  save_path <- arguments[["<save_path>"]]

  node_save_path <- arguments[["<node_save_file_name_path>"]]
  edge_save_path <- arguments[["<edge_save_file_name_path>"]]


}

# Determine which function to run and save output
if (arguments[["build"]] & arguments[["help"]]) {
  cli::cli_alert_info("Builds an analysis of the project in the designated project_path set in the <project_conf.yml> using build_understand_project()")
} else if (arguments[["export"]] & arguments[["help"]]) {
  cli::cli_alert_info("From the built analysis, exports the dependency types of either files or classes (dependening on supplied flag) using export_understand_dependencies() and saves an xml of the dependencies at the specified file path: <save_path>")
} else if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("From the built analysis, parses the dependency types of either files or classes (dependening on supplied flag) using parse_understand_dependencies() and saves two csvs at the specified file paths: <node_save_file_name_path> <edge_save_file_name_path>")
} else if (arguments[["build"]]) {
  db_path <- build_understand_project(scitools_path = scitools_path,
                                      project_path = project_path,
                                      language = code_language,
                                      output_dir = understand_folder)
  cli::cli_alert_success("Project sucessfully built.")
} else if (arguments[["export"]]) {
  if (arguments[["--file"]]) {
    result <- export_understand_dependencies(scitools_path = scitools_path,
                                             db_filepath = db_path,
                                             parse_type = "file",
                                             output_filepath = save_path)
  } else if (arguments[["--class"]]) {
    result <- export_understand_dependencies(scitools_path = scitools_path,
                                             db_filepath = db_path,
                                             parse_type = "class",
                                             output_filepath = save_path)
  } else {
    stop("No/invalid option(s) provided.")
  }
  cli::cli_alert_success(paste0("Dependencies xml was saved at: ", save_path))
} else if (arguments[["parse"]]) {
  if (arguments[["--file"]]) {
    result <- parse_understand_dependencies(dependencies_path = file_dependencies_path)

    data.table::fwrite(result$node_list, node_save_path)
    data.table::fwrite(result$edge_list, edge_save_path)

  } else if (arguments[["--class"]]) {
    result <- parse_understand_dependencies(dependencies_path = class_dependencies_path)

    data.table::fwrite(result$node_list, node_save_path)
    data.table::fwrite(result$edge_list, edge_save_path)
  } else {
    stop("No/invalid option(s) provided.")
  }

  cli::cli_alert_success(paste0("Dependencies node table was saved at: ", node_save_path))
  cli::cli_alert_success(paste0("Dependencies edge table was saved at: ", edge_save_path))

} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}
