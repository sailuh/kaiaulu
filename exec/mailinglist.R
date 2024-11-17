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
  mailinglist.R parse <tools.yml> <project_conf.yml> <project_key> <save_file_name_path>
  mailinglist.R download modmbox help
  mailinglist.R download modmbox <project_conf.yml> <project_key> <start_year_month>
  mailinglist.R download pipermail help
  mailinglist.R download pipermail <project_conf.yml> <project_key> <start_year_month> <end_year_month>
  mailinglist.R (-h | --help)
  mailinglist.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Mailing Lists. Please see
  Kaiaulu's README.md for instructions on how to create <tools.yml>
  and <project_conf.yml>.

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

if (arguments[["parse"]] & arguments[["help"]]) {
  cli::cli_alert_info("Parses an mbox file using parse_mbox().")
} else if (arguments[["parse"]]) {

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  project_key <- arguments[["<project_key>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tools <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- get_tool_project("perceval", tools)
  mbox_file_path <- get_mbox_input_file(conf, project_key)

  parsed_mbox <- parse_mbox(
    perceval_path = perceval_path,
    mbox_file_path = mbox_file_path
  )

  data.table::fwrite(parsed_mbox, save_path)
  cli::cli_alert_success(paste0("Parsed mbox file was saved at: ", save_path))

} else if (arguments[["download"]] & arguments[["modmbox"]] & arguments[["help"]]) {
  cli::cli_alert_info("Downloads mailing list archives from mod_mbox using download_mod_mbox().")

} else if (arguments[["download"]] & arguments[["modmbox"]]) {

  conf_path <- arguments[["<project_conf.yml>"]]
  project_key <- arguments[["<project_key>"]]
  start_year_month <- arguments[["<start_year_month>"]]

  conf <- yaml::read_yaml(conf_path)
  mailing_list <- get_mbox_domain(conf, project_key)
  save_folder_path <- get_mbox_path(conf, project_key)

  refresh_mod_mbox(
    mailing_list = mailing_list,
    start_year_month = start_year_month,
    save_folder_path = save_folder_path,
    verbose = TRUE
  )

  cli::cli_alert_success(paste0("Downloaded mailing list archives were saved at: ", save_folder_path))

} else if (arguments[["download"]] & arguments[["pipermail"]] & arguments[["help"]]) {
  cli::cli_alert_info("Downloads mailing list archives from pipermail using download_pipermail().")
} else if (arguments[["download"]] & arguments[["pipermail"]]) {

  conf_path <- arguments[["<project_conf.yml>"]]
  project_key <- arguments[["<project_key>"]]
  start_year_month <- arguments[["<start_year_month>"]]

  conf <- yaml::read_yaml(conf_path)
  mailing_list <- get_pipermail_domain(conf, project_key)
  save_folder_path <- get_pipermail_path(conf, project_key)

  refresh_pipermail(
    mailing_list = mailing_list,
    start_year_month = start_year_month,
    save_folder_path = save_folder_path,
    verbose = TRUE
  )

  cli::cli_alert_success(paste0("Downloaded mailing list archives were saved at: ", save_folder_path))

} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  cli::cli_alert_danger("Invalid command or arguments. Use --help for usage information.")
}
