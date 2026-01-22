#!/usr/local/bin/Rscript

require(kaiaulu, quietly = TRUE)
require(cli, quietly = TRUE)
require(XML, quietly = TRUE)
require(stringi, quietly = TRUE)
require(data.table, quietly = TRUE)

doc <- "
USAGE:
  rdependencies.R (-h | -help)
  rdependencies.R --version
  rdependencies.R parse help
  rdependencies.R parse <folder_path> <save_path>

DESCRIPTION:
  Analyzes a folder with R project files to return dependencies
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

# Currently unsure how variables would work

if (!arguments[["help"]]) {
  folder_path <- arguments[["<folder_path>"]]
  save_path <- arguments[["<save_path>"]]
}

# Determine which function to run and save output
if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("Analyzes dependencies using parse_r_dependencies() and saves it at the csv file specified in <save_path>")
} else if (arguments[["parse"]]) {
  result <- parse_r_dependencies(folder_path)
  data.table::fwrite(result, save_path)
  cli::cli_alert_success(paste0("Dependencies table was saved at: ", save_path))
} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}
