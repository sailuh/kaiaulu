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
require(jsonlite,quietly=TRUE)
require(knitr,quietly=TRUE)
require(magrittr,quietly=TRUE)
require(gt,quietly=TRUE)


# Rscript github.R refresh issues ../../kaiaulu/conf/kaiaulu.yml ../../rawdata/github/kaiaulu/issue_search/
# Rscript github.R refresh comments ../../kaiaulu/conf/kaiaulu.yml ../../rawdata/github/kaiaulu/issue_search/

doc <- "
USAGE:
  github refresh help
  github refresh issues <project_conf.yml> <save_file_name_path>
  github refresh comments <project_conf.yml> <save_file_name_path>
  github download Github help
  github (-h | --help)
  github --version

DESCRIPTION:
  Provides functions to refresh Github issue data. Please see
  Kaiaulu's README.md for instructions on how to create <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["refresh"]] & arguments[["help"]]){
  cli_alert_info("Downloads new data from Github Rest API")
}else if(arguments[["refresh"]] & arguments [["issues"]]){

  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  conf <- yaml::read_yaml(conf_path)

  save_path <- path.expand(conf[["issue_tracker"]][["github"]][["replies"]])
  save_path_issue_refresh <- paste0(save_path,"/issue_search/")
  save_path_issue <- paste0(save_path,"/issue/")
  save_path_issue_or_pr_comments <- paste0(save_path,"/issue_or_pr_comment/")
  # Path you wish to save all raw data. A folder with the repo name and sub-folders will be created.
  owner <- conf[["issue_tracker"]][["github"]][["owner"]] # Has to match github organization (e.g. github.com/sailuh)
  repo <- conf[["issue_tracker"]][["github"]][["repo"]] # Has to match github repository (e.g. github.com/sailuh/perceive)
  # your file github_token (a text file) contains the GitHub token API
  token <- scan("~/.ssh/github_token",what="character",quiet=TRUE)

  gh_response <- github_api_project_issue_refresh(owner,
                                                  repo,
                                                  token,
                                                  save_path_issue_refresh,
                                                  verbose=TRUE)
  github_api_iterate_pages(token,gh_response,
                           save_path_issue_refresh,
                           prefix="issue",
                           verbose=TRUE)

  cli_alert_success(paste0("Downloaded new Github issues saved at: ",save_path_issue_refresh))
}else if(arguments[["refresh"]] & arguments [["comments"]]){

  conf <- yaml::read_yaml("../conf/kaiaulu.yml")
  save_path <- path.expand(conf[["issue_tracker"]][["github"]][["replies"]])
  save_path_issue_or_pr_comments <- paste0(save_path,"/issue_or_pr_comment/")
  # Path you wish to save all raw data. A folder with the repo name and sub-folders will be created.
  owner <- conf[["issue_tracker"]][["github"]][["owner"]] # Has to match github organization (e.g. github.com/sailuh)
  repo <- conf[["issue_tracker"]][["github"]][["repo"]] # Has to match github repository (e.g. github.com/sailuh/perceive)
  # your file github_token (a text file) contains the GitHub token API
  token <- scan("~/.ssh/github_token",what="character",quiet=TRUE)

  gh_response_issue_or_pr_comment <- github_api_project_issue_or_pr_comment_refresh(owner,
                                                                                    repo,
                                                                                    token, save_path_issue_or_pr_comments, verbose=TRUE)

  # create directory and iterate over data
  #dir.create(save_path_issue_or_pr_comments)
  github_api_iterate_pages(token,gh_response_issue_or_pr_comment,
                           save_path_issue_or_pr_comments,
                           prefix="issue_or_pr_comment",
                           verbose=TRUE)
}




