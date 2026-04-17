#!/usr/local/bin/Rscript

require(kaiaulu, quietly = TRUE)
require(data.table, quietly = TRUE)
require(magrittr, quietly = TRUE)
require(knitr, quietly = TRUE)
require(cli, quietly = TRUE)

doc <- "
USAGE:
  linemetrics.R (-h | -help)
  linemetrics.R --version
  linemetrics.R parse help
  linemetrics.R parse <tools.yml> <project_conf.yml> <save_path>

DESCRIPTION:
  Analyze line metrics using the SCC tool. Please see Kaiaulu's README.md for instructions on how to create <tool.yml> and <project_conf.yml>.

ARGUMENTS:
  <tools.yml>                   path to tools.yml file
  <project_conf.yml>            path to configuration file for project you want to analyze
  <save_path>                   file path where output will be saved

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

# Determine which function to run and save output
if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("Parses a project specified in the <project_conf.yml> config file and returns a csv file designated by <save_path> using SCC")
} else if (arguments[["parse"]]) {
  tool <- parse_config(arguments[["<tools.yml>"]])
  conf <- parse_config(arguments[["<project_conf.yml>"]])
  scc_path <- get_tool_project("scc", tool)

  git_repo_path <- get_git_repo_path(conf)
  git_branch <- get_git_branches(conf)[1]

  # Filters
  file_extensions <- get_file_extensions(conf)
  substring_filepath <- get_substring_filepath(conf)

  # Output path
  save_path <- arguments[["<save_path>"]]

  git_checkout(git_branch,git_repo_path)

  # Parse Dependencies
  result <- parse_line_metrics(scc_path,git_repo_path)

  # Filter Parsed Dependencies
  result <- result  %>%
    filter_by_file_extension(file_extensions,"Provider")  %>%
    filter_by_filepath_substring(substring_filepath,"Provider")

  data.table::fwrite(result, save_path)
  cli::cli_alert_success(paste0("Line metrics table was saved at: ", save_path))
} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}
