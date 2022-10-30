#!/usr/local/bin/Rscript

# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

require(yaml,quietly=TRUE)
require(cli,quietly=TRUE)
require(docopt,quietly=TRUE)
require(kaiaulu,quietly=TRUE)
require(data.table,quietly=TRUE)



doc <- "
USAGE:
  mailinglist.R tabulate help
  mailinglist.R tabulate <tools.yml> <project_conf.yml> <save_file_name_path>
  mailinglist.R download modmbox help
  mailinglist.R download modmbox <project_conf.yml> <start_year> <end_year> <save_file_name_path>
  mailinglist.R download modmboxmonth help
  mailinglist.R download modmboxmonth <project_conf.yml> <start_year> <end_year> <save_folder_name_path>
  mailinglist.R (-h | --help)
  mailinglist.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Mailing Lists. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["tabulate"]] & arguments[["help"]]){
  cli_alert_info("Tabulates a mailing list using parse_mbox().")
}else if(arguments[["tabulate"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  mbox_path <- path.expand(conf[["mailing_list"]][["mbox"]])

  project_mbox <- parse_mbox(perceval_path,mbox_path)

  cli_alert_success(paste0("Tabulated mailing list was saved at: ",save_path))

  data.table::fwrite(project_mbox,save_path)
}else if(arguments[["download"]] & arguments[["modmbox"]] & arguments[["help"]]){
  cli_alert_info("Saves a mailing list archive from mod_mbox as a .mbox file
                 using download_mod_mbox().")
}else if(arguments[["download"]] & arguments[["modmbox"]]){


  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]
  conf <- yaml::read_yaml(conf_path)

  mod_mbox_url <- conf[["mailing_list"]][["domain"]]
  mailing_list <- conf[["mailing_list"]][["list_key"]][1]

  start_year <- arguments[["<start_year>"]]
  end_year <- arguments[["<end_year>"]]

  mbox <- download_mod_mbox(base_url = mod_mbox_url,
                            mailing_list = mailing_list,
                            from_year=start_year,
                            to_year=end_year,
                            save_file_path = save_path,
                            verbose = TRUE)

  cli_alert_success(paste0("Downloaded mailing list was saved at: ",save_path))
}else if(arguments[["download"]] & arguments[["modmboxmonth"]]){


  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_folder_name_path>"]]
  conf <- yaml::read_yaml(conf_path)

  mod_mbox_url <- conf[["mailing_list"]][["domain"]]
  mailing_list <- conf[["mailing_list"]][["list_key"]][1]

  start_year <- arguments[["<start_year>"]]
  end_year <- arguments[["<end_year>"]]

  mbox <- download_mod_mbox_per_month(base_url = mod_mbox_url,
                            mailing_list = mailing_list,
                            from_year=start_year,
                            to_year=end_year,
                            save_folder_path = save_path,
                            verbose = TRUE)

  cli_alert_success(paste0("Downloaded mailing list was saved at: ",save_path))
}
