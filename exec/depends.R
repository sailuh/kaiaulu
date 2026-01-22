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
  depends.R parse <config_filepath.yml> <tool_filepath.yml> <project_github_url> <save_path>

DESCRIPTION:
  Parses file dependencies using the Depends tool.

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

# Determine which function to run and save output
if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("Parses a given GitHub project supplied by <project_github_url> and  saves two tables at the specified folder <save_path> using Depends")
} else if (arguments[["parse"]]) {
  tool <- parse_config(arguments[["<tool_filepath.yml>"]])
  conf <- parse_config(arguments[["<config_filepath.yml>"]])
  project_github_url <- arguments[["<project_github_url>"]]
  git_repo_path <- get_git_repo_path(conf)

  # Depends parameters
  depends_jar_path <- get_tool_project("depends", tool)
  language <- get_depends_code_language(conf)
  keep_dependencies_type <- get_depends_keep_dependencies_type(conf)

  # Filters
  file_extensions <- get_file_extensions(conf)
  substring_filepath <- get_substring_filepath(conf)

  # Output
  save_path <- arguments[["<save_path>"]]
  node_file <- paste0(save_path, "/node.csv")
  edge_file <- paste0(save_path, "/edge.csv")

  # Construct File Network
  project_github_url <- stri_split_regex(project_github_url,pattern="/")[[1]]
  owner <- project_github_url[length(project_github_url)-1]
  repo <- project_github_url[length(project_github_url)]
  language_distribution_byte <- unlist(gh("GET /repos/:owner/:repo/languages",owner=owner,repo=repo))
  language_distribution_byte <- language_distribution_byte/sum(language_distribution_byte)
  format(round(language_distribution_byte, 2), nsmall = 2)

  result <- parse_dependencies(depends_jar_path,git_repo_path,language=language) # Parse Dependencies

  data.table::fwrite(result$nodes, node_file)
  data.table::fwrite(result$edgelist, edge_file)
  cli::cli_alert_success(paste0("Dependencies table was saved at: ", save_path))
} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}
