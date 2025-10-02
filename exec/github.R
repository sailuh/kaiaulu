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
  github refresh <project_conf.yml> <project_key> [--issues | --comments | --pr]
  github parse help
  github parse <project_conf.yml> <save_file_name_path> [--issues | --comments | --pr]
  github (-h | --help)
  github --version

DESCRIPTION:
  Provides functions to refresh Github issue data. Please see
  Kaiaulu's README.md for instructions on how to create <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
  --issues      Refreshes/Parses issues
  --comments    Refreshes/Parses comments
  --pr          Refreshes/Parses Pull Requests
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')

if(arguments[["refresh"]] & arguments[["help"]]) {
  cli_alert_info("Downloads new data (whose type is specified by flags) from Github Rest API")
} else if(arguments[["refresh"]]) {

  conf_path <- arguments[["<project_conf.yml>"]]
  project_key <- arguments[["<project_key>"]]

  conf <- yaml::read_yaml(conf_path)
  # Path you wish to save all raw data. A folder with the repo name and sub-folders will be created.
  owner <- get_github_owner(conf, "project_key_1") # Has to match github organization (e.g. github.com/sailuh)
  repo <- get_github_repo(conf, "project_key_1") # Has to match github repository (e.g. github.com/sailuh/perceive)
  # your file github_token (a text file) contains the GitHub token API
  token <- scan("~/.ssh/github_token",what="character",quiet=TRUE)

  if (arguments[["--issues"]]) {
    save_path_issue <- get_github_issue_path(conf, project_key)

    gh_response <- github_api_project_issue(owner,repo,token)
    dir.create(save_path_issue)
    github_api_iterate_pages(token,gh_response,
                             save_path_issue,
                             prefix="issue",
                             verbose = TRUE)

    cli_alert_success(paste0("Downloaded new Github issues saved at: ", save_path_issue))
  } else if(arguments [["--pr"]]){
    save_path_pull_request <- get_github_pull_request_path(conf, project_key)

    gh_response <- github_api_project_pull_request(owner,repo,token)
    dir.create(save_path_pull_request)
    github_api_iterate_pages(token,gh_response,
                             save_path_pull_request,
                             prefix="pull_request",
                             verbose = TRUE)

    cli_alert_success(paste0("Downloaded new Github Pull Requests saved at: ", save_path_pull_request))
  } else if(arguments [["--comments"]]){
    save_path_issue_or_pr_comments <- path.expand(get_github_issue_or_pr_comment_path(conf, project_key))
    
    gh_response <- github_api_project_issue_or_pr_comments(owner, repo, token)
    dir.create(save_path_issue_or_pr_comments)
    github_api_iterate_pages(token,gh_response,
                             save_path_issue_or_pr_comments,
                             prefix="issue_or_pr_comment",
                             verbose= TRUE)

    cli_alert_success(paste0("Downloaded new Github issue or PR comments saved at: ", save_path_issue_or_pr_comments))
  }
} else if(arguments[["parse"]] & arguments[["help"]]) {
  cli_alert_info("Parses downloaded data (whose type is specified by flags) and saves into a csv denoted by <save_file_name_path>")
} else if(arguments[["parse"]]) {
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  if (arguments[["--issues"]]) {
    save_path_issue <- get_github_issue_path(conf, project_key)

    all_issue <- lapply(list.files(save_path_issue,
                                   full.names = TRUE),read_json)
    all_issue <- lapply(all_issue,
                        github_parse_project_issue)
    all_issue <- rbindlist(all_issue,fill=TRUE)

    data.table::fwrite(all_issue, save_path)
    cli::cli_alert_success(paste0("Dependencies table was saved at: ", save_path))
  } else if(arguments [["--pr"]]){
    save_path_pull_request <- get_github_pull_request_path(conf, project_key)

    all_pr <- lapply(list.files(save_path_pull_request,
                                full.names = TRUE),read_json)
    all_pr <- lapply(all_pr,
                     github_parse_project_pull_request)
    all_pr <- rbindlist(all_pr,fill=TRUE)

    data.table::fwrite(all_pr, save_path)
    cli::cli_alert_success(paste0("Dependencies table was saved at: ", save_path))
  } else if(arguments [["--comments"]]){
    save_path_issue_or_pr_comments <- path.expand(get_github_issue_or_pr_comment_path(conf, project_key))

    all_issue_or_pr_comments <- lapply(list.files(save_path_issue_or_pr_comments,
                                                  full.names = TRUE),read_json)
    all_issue_or_pr_comments <- lapply(all_issue_or_pr_comments,
                                       github_parse_project_issue_or_pr_comments)
    all_issue_or_pr_comments <- rbindlist(all_issue_or_pr_comments,fill=TRUE)

    data.table::fwrite(all_issue_or_pr_comments, save_path)
    cli::cli_alert_success(paste0("Dependencies table was saved at: ", save_path))
  }
} else if (arguments[["-h"]] || arguments[["--help"]]) {
  cli::cli_alert_info(doc)
} else if (arguments[["--version"]]) {
  cli::cli_alert_info('Kaiaulu 0.0.0.9700')
} else {
  stop("No/invalid option(s) provided.")
}




