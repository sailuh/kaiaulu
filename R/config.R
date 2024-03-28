# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Configuration File Getter Functions ##############


#### tools.yml ####

#' Get tools.yml
#'
#' @return the tools.yml file
#' @export
get_tools <- function() {
  tool <- yaml::read_yaml("tools.yml")
  return(tool)
}

#' Get Tool Path
#'
#' Gets the tool path from the tools.yml file
#'
#' @param tool_name the name of the tool (e.g. "perceval" or "dv8")
#' @return the tool path from tools.yml
#' @export
get_tools_project <- function(tool_name) {
  tool_path <- tool[[tool_name]]
  return(tool_path)
}


#### Get Functions ####

#' Get config.yml File
#'
#' Gets the configuration file for any project, as long as it exists
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the config yaml file
#' @export
get_conf <- function(project_name) {
  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)
  return(conf)
}

#' Get Project Git Repo Path
#'
#' Gets the local project git repo path from the config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the local git repo path
#' @export
get_project_git_repo_path <- function(project_name) {
  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  # check to make sure the path exists
  if (!is.null(conf[["version_control"]])) {
    if (!is.null(conf[["version_control"]][["log"]])) {

      git_repo_path <- conf[["version_control"]][["log"]]
      return(git_repo_path)

    } else {
      stop("The \"version_control\" field does not exist in the configuration file.")
    }
  } else {
    stop("The \"log\" field does not exist in the configuration file.")
  }
}

#' Get Project Git Branch
#'
#' Gets a git branch of the project. Since there may be multiple branches for the project, a specific
#' branch must be specified. The order of the branch list can be viewed in the config file.
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @param branch_index the index of the branch
#' @return the git branch
#' @export
get_project_git_branch <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  git_branch <- conf[["version_control"]][["branch"]][branch_index]
}

#' Get File Extensions
#'
#' Gets the file extensions from the config file for filtering
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the file extensions
#' @export
get_project_file_extensions <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]
  return(file_extensions)
}

#' Get Substring Filepath
#'
#' Gets the substring filepaths from the config file for filtering
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the substring filepaths
#' @export
get_project_substring_filepath <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]
  return(substring_filepath)
}

#' Get Filter Commit Size
#'
#' Gets the commit size to filter out from the config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the commit size to filter out
#' @export
get_project_filter_commit_size <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  filter_commit_size <- conf[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]
  return(filter_commit_size)
}

#' Get Uctags Line Types
#'
#' Gets the Uctags keep lines type
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the uctags line type from the project config file
#' @export
get_project_uctags_line_types <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  kinds <- conf[["tool"]][["uctags"]][["keep_lines_type"]]
  return(kinds)
}

#' Get Code Language
#'
#' Gets the code language of the project from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the code language
#' @export
get_project_code_language <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  language <- conf[["tool"]][["depends"]][["code_language"]]
  return (language)
}

#' Get Keep Dependencies Type
#'
#' Gets the type of dependencies to keep from the config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the types of dependencies to keep
#' @export
get_project_keep_dependencies_type <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  keep_dependencies_type <- conf[["tool"]][["depends"]][["keep_dependencies_type"]]
  return (keep_dependencies_type)
}

#' Get Issue Id Regex
#'
#' Gets the issue ID regex on commit messages from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the issue id regex
#' @export
get_project_issue_id_regex <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  issue_id_regex <- conf[["commit_message_id_regex"]][["issue_id"]]
  return(issue_id_regex)
}

#' Get Jira Issues Path
#'
#' Gets the path to the Jira issues from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the Jira issues path
#' @export
get_project_jira_issues_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  jira_issues_path <- conf[["issue_tracker"]][["jira"]][["issues"]]
  return(jira_issues_path)
}

#' Get Jira Issues Comments Path
#'
#' Gets the path to the Jira issues with comments from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the Jira issues comments path
#' @export
get_project_jira_issues_comments_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  jira_issue_comments_path <- conf[["issue_tracker"]][["jira"]][["issue_comments"]]
  return(jira_issue_comments_path)
}

#' Get SrcML Filepath
#'
#' Gets the SrcML filepath where the output is stored
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the srcml filepath
#' @export
get_project_srcml_filepath <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  srcml_filepath <- conf[["tool"]][["srcml"]][["srcml_path"]]
  return(srcml_filepath)
}

#' Get Pattern4 Class Folder Path
#'
#' Gets the Pattern4 class folder path where the output is stored
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the class folder path
#' @export
get_project_pattern4_folder_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  class_folder_path <- conf[["tool"]][["pattern4"]][["class_folder_path"]]
  return(class_folder_path)
}

#' Get Pattern4 Output Filepath
#'
#' Gets the Pattern4 filepath where the output is stored
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the output filepath
#' @export
get_project_pattern4_folder_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  pattern4_output_filepath <- conf[["tool"]][["pattern4"]][["output_filepath"]]
  return(pattern4_output_filepath)
}

#' Get Topics
#'
#' Gets the topics and keywords
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the topics
#' @export
get_project_topics <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  topics <- conf[["analysis"]][["topics"]]
  return(topics)
}

#' Get Mbox Save Path
#'
#' Gets the local mbox save path from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the local mbox save path
#' @export
get_project_mbox_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  mbox_path <- conf[["mailing_list"]][["mbox"]]
  return(mbox_path)
}

#' Get Mbox Domain
#'
#' Gets the domain of the mailing list archive
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the domain of the mailing list archive
#' @export
get_project_mbox_domain <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  mod_mbox_url <- conf[["mailing_list"]][["domain"]]
  return(mbox_domain)
}

#' Get Mbox Lists
#'
#' Gets the list keys of the domain to be used
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the domain of the mailing list archive
#' @export
get_project_mbox_list_key <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  mailing_list <- conf[["mailing_list"]][["list_key"]]
  return(mbox_domain)
}

#' Get DV8 Project Folder Path
#'
#' Gets the dv8 project folder path from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the dv8 project folder path
#' @export
get_project_dv8_folder_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  project_path <- conf[["tool"]][["dv8"]][["folder_path"]]
  return(project_path)
}

#' Get Flaws Params
#'
#' Gets the architectural flaws thresholds
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the flaws params
#' @export
get_project_dv8_flaws_params <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  flaws_params <- conf[["tool"]][["dv8"]][["architectural_flaws"]]
  return(flaws_params)
}

#' Get GitHub Project Owner
#'
#' Gets the owner of the GitHub project
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the GitHub project owner name
#' @export
get_project_github_owner <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  owner <- conf[["issue_tracker"]][["github"]][["owner"]]
  return(flaws_params)
}

#' Get GitHub Repository
#'
#' Gets the name of the GitHub repository
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the GitHub repository name
#' @export
get_project_github_repo_name <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  repo <- conf[["issue_tracker"]][["github"]][["repo"]]
  return(repo)
}

#' Get GitHub Replies Save Path
#'
#' Gets the local save path for the GitHub replies
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the local save path for GitHub replies
#' @export
get_project_github_replies_save_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  save_path <- path.expand(conf[["issue_tracker"]][["github"]][["replies"]])
  return(save_path)
}

#' Get Jira Issue Tracker Domain
#'
#' Gets the Jira issue tracker domain from the config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the issue tracker domain
#' @export
get_project_jira_issue_tracker_domain <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  domain <- conf[["issue_tracker"]][["jira"]][["domain"]]
  return(domain)
}

#' Get Jira Issue Tracker Project Key
#'
#' Gets the Jira issue tracker project key from the config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the issue tracker project key
#' @export
get_project_jira_issue_tracker_key <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  key <- conf[["issue_tracker"]][["jira"]][["project_key"]]
  return(domain)
}

#' Get Jira Issue Tracker Issues Path
#'
#' Gets the local path of the Jira issue tracker issues in the config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the local issue tracker issues path
#' @export
get_project_jira_issue_save_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  save_path_issue <- conf[["issue_tracker"]][["jira"]][["issues"]]
  return(save_path_issue)
}

#' Get Jira Issue Tracker Issue Comments Path
#'
#' Gets the local path of the Jira issue tracker issues with comments in the config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the local issue tracker issues with comments path
#' @export
get_project_jira_issue_comments_save_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  save_path_issue_comments <- conf[["issue_tracker"]][["jira"]][["issue_comments"]]
  return(save_path)
}

#' Get Nvd Feed Folder Path
#'
#' Gets the folder path with nvd cve feeds
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the folder path with nvd cve feeds
#' @export
get_project_nvdfeed_folder_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  nvdfeed_folder_path <- conf[["vulnerabilities"]][["nvd_feed"]]
  return(nvdfeed_folder_path)
}

#' Get CVE ID Regex
#'
#' Gets the commit message CVE
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the commit message CVE
#' @export
get_project_cveid_regex <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  cveid_regex <- conf[["commit_message_id_regex"]][["cve_id"]]
  return(cveid_regex)
}

#' Get Enumeration Commit
#'
#' Gets the enumerated commit intervals for analysis. Since there may be multiple commits for the project,
#' a specific commit must be specified. The order of the commit list can be viewed in the config file.
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @param commit_index the index of the commit in the list
#' @return the commit interval
#' @export
get_project_enumeration_commit <- function(project_name, commit_index) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  commit <- conf[["analysis"]][["enumeration"]][["commit"]][commit_index]
  return(commit)
}

#' Get Window Start Commit
#'
#' Gets the analysis window start commit
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the analysis window size
#' @export
get_project_window_start_commit <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  start_commit <- conf[["analysis"]][["window"]][["start_commit"]]
  return(start_commit)
}

#' Get Window End Commit
#'
#' Gets the analysis window end commit
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the analysis window size
#' @export
get_project_window_end_commit <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  end_commit <- conf[["analysis"]][["window"]][["end_commit"]]
  return(end_commit)
}

#' Get Analysis Window Size
#'
#' Gets the analysis window size
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the analysis window size
#' @export
get_project_window_size <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  window_size <- conf[["analysis"]][["window"]][["size_days"]]
  return(window_size)
}
