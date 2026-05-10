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

doc <- "
USAGE:
  mailinglist.R parse help
  mailinglist.R parse <tools.yml> <mbox_file_path> <save_path>
  mailinglist.R refresh_modmbox help
  mailinglist.R refresh_modmbox <project_conf.yml> <save_path> <start_year_month>
  mailinglist.R refresh_pipermail help
  mailinglist.R refresh_pipermail <project_conf.yml> <save_path> <start_year_month>
  mailinglist.R (-h | --help)
  mailinglist.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Mailing Lists. Please see
  Kaiaulu's README.md for instructions on how to create <tools.yml>
  and <project_conf.yml>.

COMMANDS:
  parse                         parses an inputted mbox file using parse_mbox()
  refresh_modmbox               refreshes modmbox mail from inputted start year and month
  refresh_pipermail             refreshes pipermail mail from inputted start year and month

ARGUMENTS:
  <tools.yml>                   path to tools.yml file
  <project_conf.yml>            path to configuration file for project you want to analyze
  <save_path>                   file path where output will be saved
  <mbox_file_path>              path to an mbox file you would like to parse
  <start_year_month>            start date to refresh all mail after. Must follow format of YYYYMM e.g. 202410

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("Parses an mbox file using parse_mbox().")
} else if (arguments[["parse"]]) {

  tools_path <- arguments[["<tools.yml>"]]
  save_path <- arguments[["<save_path>"]]
  mbox_file_path <- arguments[["<mbox_file_path>"]]

  tools <- yaml::read_yaml(tools_path)
  perceval_path <- get_tool_project("perceval", tools)

  cli::cli_alert_info(paste0("Parsing mbox file: ", mbox_file_path))
  parsed_mbox <- parse_mbox(
    perceval_path = perceval_path,
    mbox_file_path = mbox_file_path
  )

  data.table::fwrite(parsed_mbox, save_path)
  cli::cli_alert_success(paste0("Parsed mbox file was saved at: ", save_path))

} else if (arguments[["refresh_modmbox"]] & arguments[["help"]]) {
  cli::cli_alert_info("Refreshes mailing list archives from mod_mbox using refresh_mod_mbox().")

} else if (arguments[["refresh_modmbox"]]) {

  conf_path <- arguments[["<project_conf.yml>"]]
  start_year_month <- arguments[["<start_year_month>"]]
  save_path <- arguments[["<save_path>"]]

  conf <- yaml::read_yaml(conf_path)

  mbox_mailing_list <- get_mbox_domain(conf, "project_key_1")

  refresh_mod_mbox(
    mailing_list = mbox_mailing_list,
    start_year_month = start_year_month,
    save_folder_path = save_path,
    verbose = TRUE
  )

  cli::cli_alert_success(paste0("Refreshed mailing list archives were saved at: ", save_path))

} else if (arguments[["refresh_pipermail"]] & arguments[["help"]]) {
  cli::cli_alert_info("Refreshes mailing list archives from pipermail using refresh_pipermail().")
} else if (arguments[["refresh_pipermail"]]) {

  conf_path <- arguments[["<project_conf.yml>"]]
  start_year_month <- arguments[["<start_year_month>"]]
  save_path <- arguments[["<save_path>"]]

  conf <- yaml::read_yaml(conf_path)

  pipermail_mailing_list <- get_pipermail_domain(conf, "project_key_1")

  refresh_pipermail(
    mailing_list = pipermail_mailing_list,
    start_year_month = start_year_month,
    save_folder_path = save_path,
    verbose = TRUE
  )

  cli::cli_alert_success(paste0("Refreshed mailing list archives were saved at: ", save_path))

} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  cli::cli_alert_danger("Invalid command or arguments. Use --help for usage information.")
}
