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
  git.R tabulate help
  git.R tabulate <tools.yml> <project_conf.yml> <save_file_name_path>
  git.R (-h | --help)
  git.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Git. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["tabulate"]] & arguments[["help"]]){
  cli_alert_info("Tabulates a git log using parse_gitlog().")
}else if(arguments[["tabulate"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  cli_alert_success(paste0("Tabulated git log was saved at: ",save_path))

  data.table::fwrite(project_git,save_path)
}
