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


# Rscript jira.R refresh issues ../../kaiaulu/tools.yml ../../kaiaulu/conf/geronimo.yml ../../rawdata/issue_tracker/geronimo/issue_comments/

doc <- "
USAGE:
  jira.R refresh help
  jira.R refresh issues <tools.yml> <project_conf.yml> <save_file_name_path>
  jira.R download modmbox help
  jira.R (-h | --help)
  jira.R --version

DESCRIPTION:
  Provides functions to refresh JIRA issue data with and without comments. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["refresh"]] & arguments[["help"]]){
  cli_alert_info("Downloads new data from JIRA Rest API")
}else if(arguments[["refresh"]] & arguments [["issues"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  save_path_issue_tracker_issues <- conf[["issue_tracker"]][["jira"]][["issues"]]
  save_folder_path <- conf[["issue_tracker"]][["jira"]][["issues"]]
  refresh_issues <- conf[["issue_tracker"]][["jira"]][["issues"]]
  issue_tracker_domain <- conf[["issue_tracker"]][["jira"]][["domain"]]
  issue_tracker_project_key <- conf[["issue_tracker"]][["jira"]][["project_key"]]

  if(file.exists("~/.ssh/atlassian_credentials")){
    credentials <- scan("~/.ssh/atlassian_credentials", what = "character", quiet = TRUE)
    username <- credentials[1]
    password <- credentials[2]
  }

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
                      username,
                      password,
                      save_path_issue_tracker_issues,
                      max_results = 50,
                      max_total_downloads = 5000,
                      verbose = TRUE)

  cli_alert_success(paste0("Downloaded new JIRA issues saved at: ",save_path_issue_tracker_issues))
}


