#!/usr/local/bin/Rscript

# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

require(yaml, quietly = TRUE)
require(cli, quietly = TRUE)
require(docopt, quietly = TRUE)
require(kaiaulu, quietly = TRUE)
require(data.table, quietly = TRUE)
require(jsonlite, quietly = TRUE)
source("R/config.R")

doc <- "
USAGE:
  github_events.R download help
  github_events.R download <config_path> --token_path=<path>
  github_events.R parse help
  github_events.R parse <config_path> <output_file>
  github_events.R (-h | --help)
  github_events.R --version

DESCRIPTION:
  Download and parse GitHub event data via CLI.

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

# Download command logic
if (arguments[["download"]] & arguments[["help"]]) {
  cli::cli_alert_info("Downloads Github Events using R/github.R")
} else if (arguments[["download"]]) {
  # owner <- get_github_owner()
  # repo <- arguments[["<repo>"]]
  # save_path <- arguments[["<output_folder>"]]

  config_path = arguments[["config_path"]]
  conf <- parse_config(config_path)

  owner <- get_github_owner(conf, "project_key_1")
  repo <- get_github_repo(conf, "project_key_1")
  save_path <- get_github_issue_event_path(conf, "project_key_1")

  token_path <- arguments[["--token_path"]]
  token <- scan(token_path, what="character", quiet = TRUE)

  if (any(sapply(list(owner, repo, save_path, token_path, token), is.null))) {
    cli::cli_abort("Error: Missing required arguments. Please provide: <config_path> and --token_path.")
  }

  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }

  cli::cli_alert_info("Downloading GitHub Project Issue Events using R/github.R")

  # Download Logic
  gh_response <- github_api_project_issue_events(owner, repo, token)
  github_api_iterate_pages(token, gh_response, save_path, prefix="issue_event")

  cli::cli_alert_success("GitHub issue events saved to {save_path}")

}

# Parse command logic
if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("Parses GitHub event data from JSON files in input folder and saves it as a CSV")
} else if (arguments[["parse"]]) {

  config_path = arguments[["config_path"]]
  output_file = arguments[["output_file"]]
  conf <- parse_config(config_path)

  input_dir <- get_github_issue_event_path(conf, "project_key_1")

  if (any(sapply(list(input_dir, output_file), is.null)) || !file.exists(input_dir)) {
    cli::cli_abort("Error: Missing required arguments. Please provide: <config_path> and <output_file>.")
  }

  cli::cli_alert_info("Parsing Github issue events from {input_dir}")

  all_issue_event <- lapply(list.files(input_dir, full.names = TRUE), read_json)
  all_issue_event <- lapply(all_issue_event, github_parse_project_issue_events)
  all_issue_event <- rbindlist(all_issue_event, fill = TRUE)
  all_issue_event[, issue_body := NULL]

  # Write data
  write.csv(all_issue_event, output_file, row.names = FALSE)

  cli::cli_alert_info("Parsed data saved to {output_file}")
}



