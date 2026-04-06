#!/usr/local/bin/Rscript

require(kaiaulu, quietly = TRUE)
require(data.table, quietly = TRUE)
require(stringi, quietly = TRUE)
require(gh, quietly = TRUE)
require(yaml, quietly = TRUE)
require(magrittr, quietly = TRUE)
require(knitr, quietly = TRUE)
require(cli, quietly = TRUE)

doc <- "
USAGE:
  depends.R (-h | -help)
  depends.R --version
  depends.R parse help
  depends.R parse <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path>

DESCRIPTION:
  Parses file dependencies using the Depends tool.

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

# Determine which function to run and save output
if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("Parses a given GitHub project supplied by <project_conf.yml> and  saves two tables at the specified folders <node_save_file_name_path> <edge_save_file_name_path> using Depends")
} else if (arguments[["parse"]]) {

  tool <- parse_config(arguments[["<tools.yml>"]])
  conf <- parse_config(arguments[["<project_conf.yml>"]])
  git_repo_path <- get_git_repo_path(conf)

  # Depends parameters
  depends_jar_path <- get_tool_project("depends", tool)
  language <- get_depends_code_language(conf)
  keep_dependencies_type <- get_depends_keep_dependencies_type(conf)

  # Filters
  file_extensions <- get_file_extensions(conf)
  substring_filepath <- get_substring_filepath(conf)

  # Output
  node_save_path <- arguments[["<node_save_file_name_path>"]]
  edge_save_path <- arguments[["<edge_save_file_name_path>"]]

  # Parse Dependencies
  result <- parse_dependencies(depends_jar_path,git_repo_path,language=language)

  # Filter Parsed Dependencies
  result[["nodes"]] <- result[["nodes"]]  %>%
    filter_by_file_extension(file_extensions,"filepath")  %>%
    filter_by_filepath_substring(substring_filepath,"filepath")

  result[["edgelist"]] <- result[["edgelist"]]  %>%
    filter_by_file_extension(file_extensions,"src_filepath")  %>%
    filter_by_file_extension(file_extensions,"dest_filepath")  %>%
    filter_by_filepath_substring(substring_filepath,"src_filepath") %>%
    filter_by_filepath_substring(substring_filepath,"dest_filepath")

  # Write to Files
  data.table::fwrite(result$nodes, node_save_path)
  data.table::fwrite(result$edgelist, edge_save_path)
  cli::cli_alert_success(paste0("Dependencies node table was saved at: ", node_save_path))
  cli::cli_alert_success(paste0("Dependencies edge table was saved at: ", edge_save_path))

} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}

