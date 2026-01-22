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
  linemetrics.R analyze help
  linemetrics.R analyze <config_filepath.yml> <tool_filepath.yml> <save_path>

DESCRIPTION:
  Analyze line metrics using the SCC tool.

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

# Determine which function to run and save output
if (arguments[["analyze"]] & arguments[["help"]]) {
  cli::cli_alert_info("Analyzes a project specified in the <config_filepath.yml> config file and returns a csv file designated by <save_path> using SCC")
} else if (arguments[["parse"]]) {
  tool <- parse_config(arguments[["<tool_filepath.yml>"]])
  conf <- parse_config(arguments[["<config_filepath.yml>"]])
  scc_path <- get_tool_project("scc", tool)

  git_repo_path <- get_git_repo_path(conf)
  git_branch <- get_git_branches(conf)[1]

  # Filters
  file_extensions <- get_file_extensions(conf)
  substring_filepath <- get_substring_filepath(conf)

  # Output
  save_path <- arguments[["<save_path>"]]

  git_checkout(git_branch,git_repo_path)
  result <- parse_line_metrics(scc_path,git_repo_path) # Parse Dependencies

  data.table::fwrite(result, save_path)
  cli::cli_alert_success(paste0("Line metrics table was saved at: ", save_path))
} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}
