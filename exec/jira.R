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

# rscript jira.R refresh /Users/cohen/Documents/GitHub/kaiaulu/conf/geronimo.yml /Users/cohen/Documents/analysis/geronimo/jira/issues --auth /Users/cohen/atlassian_credentials
# Rscript jira.R refresh issues ../../kaiaulu/tools.yml ../../kaiaulu/conf/geronimo.yml ../../rawdata/issue_tracker/geronimo/issue_comments/

doc <- "
USAGE:
  jira.R refresh help
  jira.R refresh <project_conf.yml> <save_path> [(--auth <credentials>)]
  jira.R download_date help
  jira.R download_date <project_conf.yml> <save_path> <date_lower_bound> <date_upper_bound> [(--auth <credentials>)]
  jira.R download_key help
  jira.R download_key <project_conf.yml> <save_path> <issue_key_lower_bound> <issue_key_upper_bound> [(--auth <credentials>)]
  jira.R (-h | --help)
  jira.R --version

DESCRIPTION:
  Provides functions to refresh JIRA issue data with and without comments. Please see Kaiaulu's README.md for instructions on how to create <tool.yml> and <project_conf.yml>.

COMMANDS:
  refresh          download JIRA issues which were added since the last issue was downloaded
  download_date    download JIRA issues within inputted date bounds
  download_key     download JIRA issues within inputted issue bounds

ARGUMENTS:
  <project_conf.yml>            path to configuration file for project you want to analyze
  <save_path>                   file path where output will be saved
  <date_lower_bound>            indicates lower date bound of issues to download. The acceptable formats are; yyyy/MM/dd HH:mm, yyyy-MM-dd HH:mm, yyyy/MM/dd, yyyy-MM-dd.
  <date_upper_bound>            indicates upper date bound of issues to download. The acceptable formats are; yyyy/MM/dd HH:mm, yyyy-MM-dd HH:mm, yyyy/MM/dd, yyyy-MM-dd.
  <issue_key_lower_bound>       indicates lower issue key bound of issues to download. The default format is <project key>-<issue number> such as GERONIMO-6000.
  <issue_key_upper_bound>       indicates upper issue key bound of issues to download. The default format is <project key>-<issue number> such as GERONIMO-6000.
  <credentials>                 file path to credentials file which contains username and password, see command help for more details and example

OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
  --auth        If authentication is needed, input credentials with this flag to access project
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["refresh"]] & arguments[["help"]]){
  cli_alert_info("Downloads new data from JIRA Rest API. If authentication is needed, save your username (e-mail) and password (API token) in a file, e.g. atlassian_credentials, where the first line is the username, and the second the API token, e.g.
```
jondoe@jondoe.com
jondoespassword
``` ")
}else if(arguments[["download_date"]] & arguments[["help"]]){
  cli_alert_info("Downloads data from JIRA Rest API based on inputted date bounds.The acceptable formats are; yyyy/MM/dd HH:mm, yyyy-MM-dd HH:mm, yyyy/MM/dd, yyyy-MM-dd.
  If authentication is needed, save your username (e-mail) and password (API token) in a file, e.g. atlassian_credentials, where the first line is the username, and the second the API token, e.g.
```
jondoe@jondoe.com
jondoespassword
``` ")
}else if(arguments[["download_key"]] & arguments[["help"]]){
  cli_alert_info("Downloads data from JIRA Rest API based on inputted issue key bounds.The default format is <project key>-<issue number> such as GERONIMO-6000.
  If authentication is needed, save your username (e-mail) and password (API token) in a file, e.g. atlassian_credentials, where the first line is the username, and the second the API token, e.g.
```
jondoe@jondoe.com
jondoespassword
``` ")
}else if(arguments[["refresh"]]){

  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_path>"]]

  conf <- yaml::read_yaml(conf_path)

  issue_tracker_domain <- conf[["issue_tracker"]][["jira"]][["project_key_1"]][["domain"]]
  issue_tracker_project_key <- conf[["issue_tracker"]][["jira"]][["project_key_1"]][["project_key"]]

  if(arguments[["--auth"]]){
    credentials_path <- arguments[["<credentials>"]]
    if(file.exists(credentials_path)){
      credentials <- scan(credentials_path, what = "character", quiet = TRUE)
      username <- credentials[1]
      password <- credentials[2]

    refresh_jira_issues(issue_tracker_domain,
                        jql_query = paste0("project='",issue_tracker_project_key,"'"),
                        fields = c("summary",
                                   "description",
                                   "creator",
                                   "assignee",
                                   "reporter",
                                   "issuetype",
                                   "status",
                                   "resolution",
                                   "components",
                                   "created",
                                   "updated",
                                   "resolutiondate",
                                   "priority",
                                   "votes",
                                   "watches",
                                   "versions",
                                   "fixVersions",
                                   "labels",
                                   "comment"),
                        username = username,
                        password = password,
                        save_folder_path = save_path,
                        max_results = 50,
                        max_total_downloads = 5000,
                        verbose = TRUE)

    cli_alert_success(paste0("Downloaded new JIRA issues saved at: ",save_path))
    }else{
      cli_alert_warning("Invalid credentials file path")
    }
  }else{
  refresh_jira_issues(issue_tracker_domain,
                      jql_query = paste0("project='",issue_tracker_project_key,"'"),
                      fields = c("summary",
                                 "description",
                                 "creator",
                                 "assignee",
                                 "reporter",
                                 "issuetype",
                                 "status",
                                 "resolution",
                                 "components",
                                 "created",
                                 "updated",
                                 "resolutiondate",
                                 "priority",
                                 "votes",
                                 "watches",
                                 "versions",
                                 "fixVersions",
                                 "labels",
                                 "comment"),
                      save_folder_path = save_path,
                      max_results = 50,
                      max_total_downloads = 5000,
                      verbose = TRUE)

  cli_alert_success(paste0("Downloaded new JIRA issues saved at: ",save_path))
  }
}else if(arguments[["download_date"]]){

  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_path>"]]
  date_lower_bound <- arguments[["<date_lower_bound>"]]
  date_upper_bound <- arguments[["<date_upper_bound>"]]

  conf <- yaml::read_yaml(conf_path)

  issue_tracker_domain <- conf[["issue_tracker"]][["jira"]][["project_key_1"]][["domain"]]
  issue_tracker_project_key <- conf[["issue_tracker"]][["jira"]][["project_key_1"]][["project_key"]]

  if(arguments[["--auth"]]){
    credentials_path <- arguments[["<credentials>"]]
    if(file.exists(credentials_path)){
      credentials <- scan(credentials_path, what = "character", quiet = TRUE)
      username <- credentials[1]
      password <- credentials[2]

    download_jira_issues_by_date(issue_tracker_domain,
                        jql_query = paste0("project='",issue_tracker_project_key,"'"),
                        fields = c("summary",
                                   "description",
                                   "creator",
                                   "assignee",
                                   "reporter",
                                   "issuetype",
                                   "status",
                                   "resolution",
                                   "components",
                                   "created",
                                   "updated",
                                   "resolutiondate",
                                   "priority",
                                   "votes",
                                   "watches",
                                   "versions",
                                   "fixVersions",
                                   "labels",
                                   "comment"),
                        username = username,
                        password = password,
                        save_folder_path = save_path,
                        max_results = 50,
                        max_total_downloads = 5000,
                        date_lower_bound = date_lower_bound,
                        date_upper_bound = date_upper_bound,
                        verbose = TRUE)

    cli_alert_success(paste0("Downloaded JIRA issues by date saved at: ",save_path))
    }else{
      cli_alert_warning("Invalid credentials file path")
    }
  }else{
  download_jira_issues_by_date(issue_tracker_domain,
                      jql_query = paste0("project='",issue_tracker_project_key,"'"),
                      fields = c("summary",
                                 "description",
                                 "creator",
                                 "assignee",
                                 "reporter",
                                 "issuetype",
                                 "status",
                                 "resolution",
                                 "components",
                                 "created",
                                 "updated",
                                 "resolutiondate",
                                 "priority",
                                 "votes",
                                 "watches",
                                 "versions",
                                 "fixVersions",
                                 "labels",
                                 "comment"),
                      save_folder_path = save_path,
                      max_results = 50,
                      max_total_downloads = 5000,
                      date_lower_bound = date_lower_bound,
                      date_upper_bound = date_upper_bound,
                      verbose = TRUE)

  cli_alert_success(paste0("Downloaded JIRA issues by date saved at: ",save_path))
  }
}else if(arguments[["download_key"]]){

  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_path>"]]
  issue_key_lower_bound <- arguments[["<issue_key_lower_bound>"]]
  issue_key_upper_bound <- arguments[["<issue_key_upper_bound>"]]

  conf <- yaml::read_yaml(conf_path)

  issue_tracker_domain <- conf[["issue_tracker"]][["jira"]][["project_key_1"]][["domain"]]
  issue_tracker_project_key <- conf[["issue_tracker"]][["jira"]][["project_key_1"]][["project_key"]]

  if(arguments[["--auth"]]){
    credentials_path <- arguments[["<credentials>"]]
    if(file.exists(credentials_path)){
      credentials <- scan(credentials_path, what = "character", quiet = TRUE)
      username <- credentials[1]
      password <- credentials[2]

    download_jira_issues_by_issue_key(issue_tracker_domain,
                        jql_query = paste0("project='",issue_tracker_project_key,"'"),
                        fields = c("summary",
                                   "description",
                                   "creator",
                                   "assignee",
                                   "reporter",
                                   "issuetype",
                                   "status",
                                   "resolution",
                                   "components",
                                   "created",
                                   "updated",
                                   "resolutiondate",
                                   "priority",
                                   "votes",
                                   "watches",
                                   "versions",
                                   "fixVersions",
                                   "labels",
                                   "comment"),
                        username = username,
                        password = password,
                        save_folder_path = save_path,
                        max_results = 50,
                        max_total_downloads = 5000,
                        issue_key_lower_bound = issue_key_lower_bound,
                        issue_key_upper_bound = issue_key_upper_bound,
                        verbose = TRUE)

    cli_alert_success(paste0("Downloaded JIRA issues by key saved at: ",save_path))
    }else{
      cli_alert_warning("Invalid credentials file path")
    }
  }else{
  download_jira_issues_by_issue_key(issue_tracker_domain,
                      jql_query = paste0("project='",issue_tracker_project_key,"'"),
                      fields = c("summary",
                                 "description",
                                 "creator",
                                 "assignee",
                                 "reporter",
                                 "issuetype",
                                 "status",
                                 "resolution",
                                 "components",
                                 "created",
                                 "updated",
                                 "resolutiondate",
                                 "priority",
                                 "votes",
                                 "watches",
                                 "versions",
                                 "fixVersions",
                                 "labels",
                                 "comment"),
                      save_folder_path = save_path,
                      max_results = 50,
                      max_total_downloads = 5000,
                      issue_key_lower_bound = issue_key_lower_bound,
                      issue_key_upper_bound = issue_key_upper_bound,
                      verbose = TRUE)

  cli_alert_success(paste0("Downloaded JIRA issues by key saved at: ",save_path))
  }
}


