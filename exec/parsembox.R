#!/usr/local/bin/Rscript

# Load required libraries
require(yaml, quietly = TRUE)
require(cli, quietly = TRUE)
require(docopt, quietly = TRUE)
require(kaiaulu, quietly = TRUE)
require(data.table, quietly = TRUE)
require(stringi, quietly = TRUE)

# Define the usage pattern for docopt
doc <- "
USAGE:
  parsembox.R parse help
  parsembox.R parse <tools.yml> <mbox_file_path> <save_file_name_path>
  parsembox.R (-h | --help)
  parsembox.R --version

DESCRIPTION:
  Provides a function to parse .mbox files using Perceval.
  Please see Kaiaulu's README.md for instructions on how to create <tools.yml>.

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

# Parse the arguments using docopt
arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')

# check for help and version flags using isTRUE
if (isTRUE(arguments[["--help"]]) || isTRUE(arguments[["-h"]])) {
  cli_alert_info("This script parses .mbox files using Perceval.")
  quit(status = 0)
} else if (isTRUE(arguments[["--version"]])) {
  cli_alert_info("Kaiaulu version 0.0.0.9600")
  quit(status = 0)
}

# Main parsing functionality for the "parse" command
if (arguments[["parse"]]) {
  # Extract arguments
  # Path to YAML configuration file
  tools_path <- arguments[["<tools.yml>"]]
  # Path to .mbox file
  mbox_path <- arguments[["<mbox_file_path>"]]
  # Path to save parsed file
  save_file <- arguments[["<save_file_name_path>"]]

  # Read YAML config for Perceval path
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- path.expand(tool[["perceval"]])

  # Call the parse_mbox function
  parsed_mbox <- parse_mbox(perceval_path, mbox_path)

  # Save the parsed .mbox content to a CSV file
  data.table::fwrite(parsed_mbox, save_file)

  cli_alert_success(paste0("Parsed mbox file '", basename(mbox_path), "' and saved output at: ", save_file))
}
