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
  mailinglist.R tabulate help
  mailinglist.R tabulate <tools.yml> <project_conf.yml> <save_file_name_path>
  mailinglist.R download modmbox help
  mailinglist.R download modmbox <project_conf.yml> <start_year_month> <end_year_month> <save_folder_path>
  mailinglist.R download pipermail help
  mailinglist.R download pipermail <project_conf.yml> <start_year_month> <end_year_month> <save_folder_path>
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

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')

if (arguments[["tabulate"]] & arguments[["help"]]) {
  cli::cli_alert_info("Tabulates a mailing list using parse_mbox().")
} else if (arguments[["tabulate"]]) {

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  mbox_file_path <- path.expand(conf[["mailing_list"]][["mod_mbox"]][["project_key_1"]][["mbox_file_path"]])

  project_mbox <- parse_mbox(perceval_path, mbox_file_path)

  data.table::fwrite(project_mbox, save_path)
  cli::cli_alert_success(paste0("Tabulated mailing list was saved at: ", save_path))

} else if (arguments[["download"]] & arguments[["modmbox"]] & arguments[["help"]]) {
  cli::cli_alert_info("Downloads mailing list archives from mod_mbox using download_mod_mbox().")
} else if (arguments[["download"]] & arguments[["modmbox"]]) {

  conf_path <- arguments[["<project_conf.yml>"]]
  start_year_month <- arguments[["<start_year_month>"]]
  end_year_month <- arguments[["<end_year_month>"]]
  save_folder_path <- arguments[["<save_folder_path>"]]

  conf <- yaml::read_yaml(conf_path)
  mailing_list <- conf[["mailing_list"]][["mod_mbox"]][["project_key_1"]][["mailing_list"]]

  download_mod_mbox(
    mailing_list = mailing_list,
    start_year_month = start_year_month,
    end_year_month = end_year_month,
    save_folder_path = save_folder_path,
    verbose = TRUE
  )

  cli::cli_alert_success(paste0("Downloaded mailing list archives were saved at: ", save_folder_path))

} else if (arguments[["download"]] & arguments[["pipermail"]] & arguments[["help"]]) {
  cli::cli_alert_info("Downloads mailing list archives from pipermail using download_pipermail().")
} else if (arguments[["download"]] & arguments[["pipermail"]]) {

  conf_path <- arguments[["<project_conf.yml>"]]
  start_year_month <- arguments[["<start_year_month>"]]
  end_year_month <- arguments[["<end_year_month>"]]
  save_folder_path <- arguments[["<save_folder_path>"]]

  conf <- yaml::read_yaml(conf_path)
  mailing_list <- conf[["mailing_list"]][["pipermail"]][["project_key_1"]][["mailing_list"]]

  download_pipermail(
    mailing_list = mailing_list,
    start_year_month = start_year_month,
    end_year_month = end_year_month,
    save_folder_path = save_folder_path,
    verbose = TRUE
  )

  cli::cli_alert_success(paste0("Downloaded mailing list archives were saved at: ", save_folder_path))

} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9600')
} else {
  cli::cli_alert_danger("Invalid command or arguments. Use --help for usage information.")
}

