# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Configuration File Getter Functions ##############

#' Get Parsed Tools File
#'
#' @param tool_path The path to the tools file, defaulted at tools.yml
#' @return the parsed tools file
#' @export
get_tools <- function(tool_path = "tools.yml") {

  tools <- yaml::read_yaml(tool_path)

  if (is.null(tools)) {
    warning("Path does not exist.")
  }

  return(tools)
}

#' Get Tools Project
#'
#' Gets the tools project from the parsed tools file
#'
#' @param tool_name The name of the tool (e.g. "perceval" or "dv8")
#' @param tools_file The parsed tools file, defaulted at `get_tools()`
#' @return the specified `tool_name` tools project from `tools_file`
#' @export
get_tools_project <- function(tool_name, tools_file = get_tools()) {

  tool_path <- tools_file[[tool_name]]

  if (is.null(tool_path)) {
    warning("Attribute does not exist.")
  }

  return(tool_path)
}

#' Get Parsed Config File
#'
#' Gets the parsed configuration file for any path-specified project
#'
#' @param config_path The path of the config file from the kaiaulu directory (e.g. "conf/kaiaulu.yml")
#' @return the parsed config file whose path is specified by `config_path`
#' @export
get_conf <- function(config_path) {

  conf <- yaml::read_yaml(config_path)

  if (is.null(conf)) {
    warning("Path does not exist.")
  }

  return(conf)
}

#' Get Project Git Repo Path
#'
#' Gets the local project git repository path of the project
#'
#' @param config_file The parsed config file
#' @return the local git repository path specified in `config_file`
#' @export
get_git_repo_path <- function(config_file) {

  git_repo_path <- config_file[["version_control"]][["log"]]

  if (is.null(git_repo_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(git_repo_path)
}

#' Get Project Git Branches
#'
#' Gets the list of git branches of the project
#'
#' @param config_file The parsed config file
#' @return the list of git branches
#' @export
get_git_branches <- function(config_file) {

  git_branch <- config_file[["version_control"]][["branch"]]

  if (is.null(git_branch)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(git_branch)
}

#' Get File Extensions
#'
#' Gets the file extensions for filtering
#'
#' @param config_file The parsed config file
#' @return the file extensions
#' @export
get_file_extensions <- function(config_file) {

  file_extensions <- config_file[["filter"]][["keep_filepaths_ending_with"]]

  if (is.null(file_extensions)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(file_extensions)
}

#' Get Substring Filepaths
#'
#' Gets the substring filepaths for filtering
#'
#' @param config_file The parsed config file
#' @return the substring filepaths
#' @export
get_substring_filepath <- function(config_file) {

  substring_filepath <- config_file[["filter"]][["remove_filepaths_containing"]]

  if (is.null(substring_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(substring_filepath)
}

#' Get Filter Commit Size
#'
#' Gets the commit size to filter out
#'
#' @param config_file The parsed config file
#' @return the commit size to filter out
#' @export
get_filter_commit_size <- function(config_file) {

  filter_commit_size <- config_file[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  if (is.null(filter_commit_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(filter_commit_size)
}

#' Get Uctags Line Types
#'
#' Gets the Uctags keep lines type
#'
#' @param config_file The parsed config file
#' @return the uctags line type
#' @export
get_uctags_line_types <- function(config_file) {

  kinds <- config_file[["tool"]][["uctags"]][["keep_lines_type"]]

  if (is.null(kinds)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(kinds)
}

#' Get Code Language
#'
#' Gets the code language of the project
#'
#' @param config_file The parsed config file
#' @return the code language
#' @export
get_code_language <- function(config_file) {

  language <- config_file[["tool"]][["depends"]][["code_language"]]

  if (is.null(language)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(language)
}

#' Get Keep Dependencies Type
#'
#' Gets the type of dependencies to keep
#'
#' @param config_file The parsed config file
#' @return the types of dependencies to keep
#' @export
get_keep_dependencies_type <- function(config_file) {

  keep_dependencies_type <- config_file[["tool"]][["depends"]][["keep_dependencies_type"]]

  if (is.null(keep_dependencies_type)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keep_dependencies_type)
}


#' Get DV8 Project Folder Path
#'
#' Gets the dv8 project folder path
#'
#' @param config_file The parsed config file
#' @return the dv8 project folder path
#' @export
get_dv8_folder_path <- function(config_file) {

  project_path <- config_file[["tool"]][["dv8"]][["folder_path"]]

  if (is.null(project_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(project_path)
}

#' Get Flaws Params
#'
#' Gets the architectural flaws thresholds
#'
#' @param config_file The parsed config file
#' @return the architectural flaws thresholds
#' @export
get_dv8_flaws_params <- function(config_file) {

  dv8_flaws_params <- config_file[["tool"]][["dv8"]][["architectural_flaws"]]

  if (is.null(dv8_flaws_params)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(dv8_flaws_params)
}

#' Get Issue Id Regex
#'
#' Gets the issue ID regex on commit messages
#'
#' @param config_file The parsed config file
#' @return the issue ID regex
#' @export
get_issue_id_regex <- function(config_file) {

  issue_id_regex <- config_file[["commit_message_id_regex"]][["issue_id"]]

  if (is.null(issue_id_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_id_regex)
}

#' Get SrcML Filepath
#'
#' Gets the SrcML filepath where the output is stored
#'
#' @param config_file The parsed config file
#' @return the SrcML filepath
#' @export
get_srcml_filepath <- function(config_file) {

  srcml_filepath <- config_file[["tool"]][["srcml"]][["srcml_path"]]

  if (is.null(srcml_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(srcml_filepath)
}

#' Get Pattern4 Class Folder Path
#'
#' Gets the Pattern4 class folder path where the output is stored
#'
#' @param config_file The parsed config file
#' @return the Pattern4 class folder path
#' @export
get_pattern4_folder_path <- function(config_file) {

  pattern4_folder_path <- config_file[["tool"]][["pattern4"]][["class_folder_path"]]

  if (is.null(pattern4_folder_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pattern4_folder_path)
}

#' Get Pattern4 Output Filepath
#'
#' Gets the Pattern4 filepath where the output is stored
#'
#' @param config_file The parsed config file
#' @return the Pattern4 output filepath
#' @export
get_pattern4_filepath <- function(config_file) {

  pattern4_filepath <- config_file[["tool"]][["pattern4"]][["output_filepath"]]

  if (is.null(pattern4_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pattern4_filepath)
}

#' Get Topics
#'
#' Gets the topics and keywords for analysis
#'
#' @param config_file The parsed config file
#' @return the topics
#' @export
get_topics <- function(config_file) {

  topics <- config_file[["analysis"]][["topics"]]

  if (is.null(topics)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(topics)
}

#' Get Mailing List Mod Mbox Key Indexes
#'
#' Gets the list of mod Mbox mail key indexes
#'
#' @param config_file The parsed config file
#' @return the list of mod Mbox mail key indexes
#' @export
get_mbox_key_indexes <- function(config_file) {

  mbox_keys <- config_file[["mailing_list"]][["mod_mbox"]]

  if (is.null(mbox_keys)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_keys)
}

#' Get Mbox Save Path
#'
#' Gets the local path to store Mbox data for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the local Mbox path for project specified by key index `project_key_index`
#' @export
get_mbox_path <- function(config_file, project_key_index) {

  mbox_path <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mbox"]]

  if (is.null(mbox_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_path)
}

#' Get Mbox Archive URL
#'
#' Gets the Mbox archive URL for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the domain of the mailing list archive for project specified by key index `project_key_index`
#' @export
get_mbox_domain <- function(config_file, project_key_index) {

  mbox_url <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["archive_url"]]

  if (is.null(mbox_url)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_url)
}

#' Get Mod Mbox Mailing List
#'
#' Gets the Mbox mailing list for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the Mbox mailing list for project specified by key index `project_key_index`
#' @export
get_mbox_mailing_list <- function(config_file, project_key_index) {

  mailing_list <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mailing_list"]]

  if (is.null(mailing_list)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mailing_list)
}

#' Get Mod Mbox Archive Type
#'
#' Gets the Mbox archive type for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the Mbox archive type for project specified by key index `project_key_index`
#' @export
get_mbox_mailing_list <- function(config_file, project_key_index) {

  archive_type <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["archive_type"]]

  if (is.null(archive_type)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(archive_type)
}

#' Get GitHub Issue Tracker Project Key Indexes
#'
#' Gets the list of GitHub issue tracker project key indexes
#'
#' @param config_file The parsed config file
#' @return the list of issue tracker project key indexes
#' @export
get_github_key_indexes <- function(config_file) {

  keys <- config_file[["issue_tracker"]][["github"]]

  if (is.null(keys)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keys)
}

#' Get GitHub Owner
#'
#' Gets the owner of the GitHub repository for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the GitHub project owner name for project specified by key index `project_key_index`
#' @export
get_github_owner <- function(config_file, project_key_index) {

  owner <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["owner"]]

  if (is.null(owner)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(owner)
}

#' Get GitHub Repository
#'
#' Gets the name of the GitHub repository for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the name of the GitHub repository for project specified by key index `project_key_index`
#' @export
get_github_repo <- function(config_file, project_key_index) {

  repo <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["repo"]]

  if (is.null(repo)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(repo)
}

#' Get GitHub Issue Save Path
#'
#' Gets the local save path for GitHub issues for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the local save path for GitHub issues for project specified by key index `project_key_index`
#' @export
get_github_issue_path <- function(config_file, project_key_index) {

  issue_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue"]]

  if (is.null(issue_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_path)
}

#' Get GitHub Issue Or PR Comment Save Path
#'
#' Gets the local save path for GitHub Issues or PR Comments for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the local save path for Github Issues or PR comments for project specified by key index `project_key_index`
#' @export
get_github_issue_or_pr_comment_path <- function(config_file, project_key_index) {

  issue_or_pr_comment_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_or_pr_comment"]]

  if (is.null(issue_or_pr_comment_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_or_pr_comment_path)
}

#' Get GitHub Issue Search Save Path
#'
#' Gets the local save path for GitHub issue search for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the local save path for GitHub issue search for project specified by key index `project_key_index`
#' @export
get_github_issue_search_path <- function(config_file, project_key_index) {

  issue_search_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_search"]]

  if (is.null(issue_search_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_search_path)
}

#' Get GitHub Pull Request Save Path
#'
#' Gets the local save path for GitHub pull requests for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the local save path for GitHub pull requests for project specified by key index `project_key_index`
#' @export
get_github_pull_request_path <- function(config_file, project_key_index) {

  pull_request_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["pull_request"]]

  if (is.null(pull_request_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pull_request_path)
}

#' Get GitHub Commit Save Path
#'
#' Gets the local save path for GitHub commits for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the local save path for GitHub commits for project specified by key index `project_key_index`
#' @export
get_github_commit_path <- function(config_file, project_key_index) {

  commit_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["commit"]]

  if (is.null(commit_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(commit_path)
}

#' Get Jira Issue Tracker Project Key Indexes
#'
#' Gets the list of Jira issue tracker project key indexes
#'
#' @param config_file The parsed config file
#' @return the list of Jira issue tracker project key indexes
#' @export
get_jira_key_indexes <- function(config_file) {

  jira_key <- config_file[["issue_tracker"]][["jira"]]

  if (is.null(jira_key)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_key)
}

#' Get Jira Project Domain
#'
#' Gets the Jira domain for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the Jira domain of the project key index for project specified by key index `project_key_index`
#' @export
get_jira_domain <- function(config_file, project_key_index) {

  domain <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["domain"]]

  if (is.null(domain)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(domain)
}

#' Get Jira Project Key Name
#'
#' Gets the Jira project key name for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the Jira project key name for project specified by key index `project_key_index`
#' @export
get_jira_project_key_name <- function(config_file, project_key_index) {

  name <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["project_key"]]

  if (is.null(name)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(name)
}

#' Get Jira Issues Path
#'
#' Gets the local path to store Jira issues for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the Jira issue path for project specified by key index `project_key_index`
#' @export
get_jira_issues_path <- function(config_file, project_key_index) {

  jira_issues_path <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["issues"]]

  if (is.null(jira_issues_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_issues_path)
}

#' Get Jira Issues Comments Path
#'
#' Gets the local path to store Jira issues with comments for a specific project key index
#'
#' @param config_file The parsed config file
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2")
#' @return the list of Jira issues comments paths for project specified by key index `project_key_index`
#' @export
get_jira_issues_comments_path <- function(config_file, project_key_index) {

  jira_issue_comments_path <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["issue_comments"]]

  if (is.null(jira_issue_comments_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_issue_comments_path)
}

#' Get Bugzilla Project Key
#'
#' Gets the project key name for the Bugzilla project
#'
#' @param config_file The parsed config file
#' @return the project key name
#' @export
get_bugzilla_project_key <- function(config_file) {

  bugzilla_key <- config_file[["issue_tracker"]][["bugzilla"]][["project_key_index"]][["project_key"]]

  if (is.null(bugzilla_key)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(bugzilla_key)
}

#' Get Nvd Feed Folder Path
#'
#' Gets the folder path with nvd feeds
#'
#' @param config_file The parsed config file
#' @return the folder path with nvd feeds
#' @export
get_nvdfeed_folder_path <- function(config_file) {

  nvdfeed_folder_path <- config_file[["vulnerabilities"]][["nvd_feed"]]

  if (is.null(nvdfeed_folder_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(nvdfeed_folder_path)
}

#' Get CVE ID Regex
#'
#' Gets the commit message CVE
#'
#' @param config_file The parsed config file
#' @return the commit message CVE
#' @export
get_cveid_regex <- function(config_file) {

  cveid_regex <- config_file[["commit_message_id_regex"]][["cve_id"]]

  if (is.null(cveid_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(cveid_regex)
}

#' Get Enumeration Commits
#'
#' Gets the list of enumerated commit intervals for analysis.
#'
#' @param config_file The parsed config file
#' @return the commit intervals as a list
#' @export
get_enumeration_commits <- function(config_file) {

  enumeration_commit <- config_file[["analysis"]][["enumeration"]][["commit"]]

  if (is.null(enumeration_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(enumeration_commit)
}

#' Get Window Start Commit
#'
#' Gets the analysis window start commit
#'
#' @param config_file The parsed config file
#' @return the analysis window start commit
#' @export
get_window_start_commit <- function(config_file) {

  start_commit <- config_file[["analysis"]][["window"]][["start_commit"]]

  if (is.null(start_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(start_commit)
}

#' Get Window End Commit
#'
#' Gets the analysis window end commit
#'
#' @param config_file The parsed config file
#' @return the analysis window end commit
#' @export
get_window_end_commit <- function(config_file) {

  end_commit <- config_file[["analysis"]][["window"]][["end_commit"]]

  if (is.null(end_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(end_commit)
}

#' Get Analysis Window Size
#'
#' Gets the analysis window size
#'
#' @param config_file The parsed config file
#' @return the analysis window size
#' @export
get_window_size <- function(config_file) {

  window_size <- config_file[["analysis"]][["window"]][["size_days"]]

  if (is.null(window_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(window_size)
}
