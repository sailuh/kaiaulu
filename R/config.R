# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Configuration File Getter Functions ##############

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
  tool <- yaml::read_yaml("tools.yml")
  tool_path <- tool[[tool_name]]
  return(tool_path)
}

#' Get config.yml File
#'
#' Gets the configuration file for any project
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the config yml file
#' @export
get_conf <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  if(is.null(conf)) {
    stop("This project configuration file is not valid.")
  } else {
    return(conf)
  }
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

  git_repo_path <- conf[["version_control"]][["log"]]

  if(is.null(git_repo_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(git_repo_path)
  }
}

#' Get Project Git Branches
#'
#' Gets the list of git branches of the project
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the git branches as a list
#' @export
get_project_git_branches <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  git_branch <- conf[["version_control"]][["branch"]]

  if(is.null(git_branch)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(git_branch)
  }
}

#' Get File Extensions
#'
#' Gets the file extensions for filtering
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the file extensions
#' @export
get_project_file_extensions <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]

  if(is.null(file_extensions)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(file_extensions)
  }
}

#' Get Substring Filepath
#'
#' Gets the substring filepaths for filtering
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the substring filepaths
#' @export
get_project_substring_filepath <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]

  if(is.null(substring_filepath)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(substring_filepath)
  }
}

#' Get Filter Commit Size
#'
#' Gets the commit size to filter out
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the commit size to filter out
#' @export
get_project_filter_commit_size <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  filter_commit_size <- conf[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  if(is.null(filter_commit_size)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(filter_commit_size)
  }
}

#' Get Uctags Line Types
#'
#' Gets the Uctags keep lines type
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the uctags line type
#' @export
get_project_uctags_line_types <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  kinds <- conf[["tool"]][["uctags"]][["keep_lines_type"]]

  if(is.null(kinds)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(kinds)
  }
}

#' Get Code Language
#'
#' Gets the code language of the project
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the code language
#' @export
get_project_code_language <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  language <- conf[["tool"]][["depends"]][["code_language"]]

  if(is.null(language)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(language)
  }
}

#' Get Keep Dependencies Type
#'
#' Gets the type of dependencies to keep
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the types of dependencies to keep
#' @export
get_project_keep_dependencies_type <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  keep_dependencies_type <- conf[["tool"]][["depends"]][["keep_dependencies_type"]]

  if(is.null(keep_dependencies_type)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(keep_dependencies_type)
  }
}


#' Get DV8 Project Folder Path
#'
#' Gets the dv8 project folder path
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the dv8 project folder path
#' @export
get_project_dv8_folder_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  project_path <- conf[["tool"]][["dv8"]][["folder_path"]]

  if(is.null(project_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(project_path)
  }
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

  if(is.null(flaws_params)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(flaws_params)
  }
}

#' Get Issue Id Regex
#'
#' Gets the issue ID regex on commit messages
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the issue id regex
#' @export
get_project_issue_id_regex <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  issue_id_regex <- conf[["commit_message_id_regex"]][["issue_id"]]

  if(is.null(issue_id_regex)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(issue_id_regex)
  }
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

  if(is.null(srcml_filepath)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(srcml_filepath)
  }
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

  if(is.null(class_folder_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(class_folder_path)
  }
}

#' Get Pattern4 Output Filepath
#'
#' Gets the Pattern4 filepath where the output is stored
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the output filepath
#' @export
get_project_pattern4_filepath <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  pattern4_output_filepath <- conf[["tool"]][["pattern4"]][["output_filepath"]]

  if(is.null(pattern4_output_filepath)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(pattern4_output_filepath)
  }
}

#' Get Topics
#'
#' Gets the topics and keywords for analysis
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the topics
#' @export
get_project_topics <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  topics <- conf[["analysis"]][["topics"]]

  if(is.null(topics)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(topics)
  }
}

#' Get Mbox Save Paths
#'
#' Gets the list of local paths to store Mbox data
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the list of mbox paths
#' @export
get_project_mbox_paths <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  mbox_path <- conf[["mailing_list"]][["mbox"]]

  if(is.null(mbox_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(mbox_path)
  }
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

  mod_mbox_domain <- conf[["mailing_list"]][["domain"]]

  if(is.null(mod_mbox_domain)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(mod_mbox_domain)
  }
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

  if(is.null(mailing_list)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(mailing_list)
  }
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

  if(is.null(owner)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(owner)
  }
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

  if(is.null(repo)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(repo)
  }
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

  if(is.null(save_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(save_path)
  }
}

#' Get Jira Issue Tracker Project Keys
#'
#' Gets the list of Jira issue tracker project keys
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the list of issue tracker project keys
#' @export
get_project_jira_project_keys <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  key <- conf[["issue_tracker"]][["jira"]][["project_key"]]

  if(is.null(key)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(key)
  }
}

#' Get Jira Project Domain
#'
#' Gets the Jira domain for a specific project key index
#'
#' @param project_key_index the name of the index of theproject key (e.g. "project_key_1" or "project_key_2")
#' @return the Jira domain of the project key index
#' @export
get_project_jira_domain <- function(project_key) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  domain <- conf[["issue_tracker"]][["jira"]][[project_key]][["domain"]]

  if(is.null(domain)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(domain)
  }
}

#' Get Jira Project Key Name
#'
#' Gets the project key name for a specific project key index
#'
#' @param project_key_index the name of the index of theproject key (e.g. "project_key_1" or "project_key_2")
#' @return the project key name
#' @export
get_project_jira_project_key_name <- function(project_key_index) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  name <- conf[["issue_tracker"]][["jira"]][[project_key_index]][["project_key"]]

  if(is.null(jira_issues_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(jira_issues_path)
  }
}

#' Get Jira Issues Path
#'
#' Gets the local path to store Jira issues for a specific project key index
#'
#' @param project_key_index the name of the index of theproject key (e.g. "project_key_1" or "project_key_2")
#' @return the Jira issue path
#' @export
get_project_jira_issues_paths <- function(project_key_index) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  jira_issues_path <- conf[["issue_tracker"]][["jira"]][[project_key_index]][["issues"]]

  if(is.null(jira_issues_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(jira_issues_path)
  }
}

#' Get Jira Issues Comments Path
#'
#' Gets the local path to store Jira issues with comments for a specific project key index
#'
#' @param project_key_index the name of the index of theproject key (e.g. "project_key_1" or "project_key_2")
#' @return the list of Jira issues comments paths
#' @export
get_project_jira_issues_comments_paths <- function(project_key_index) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  jira_issue_comments_path <- conf[["issue_tracker"]][["jira"]][[project_key_index]][["issue_comments"]]

  if(is.null(jira_issue_comments_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(jira_issue_comments_path)
  }
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

  if(is.null(nvdfeed_folder_path)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(nvdfeed_folder_path)
  }
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

  if(is.null(cveid_regex)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(cveid_regex)
  }
}

#' Get Enumeration Commits
#'
#' Gets the list of enumerated commit intervals for analysis.
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the commit intervals as a list
#' @export
get_project_enumeration_commits <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  commit <- conf[["analysis"]][["enumeration"]][["commit"]]

  if(is.null(commit)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(commit)
  }
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

  if(is.null(start_commit)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(start_commit)
  }
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

  if(is.null(end_commit)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(end_commit)
  }
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

  if(is.null(window_size)) {
    stop("This field does not exist in the configuration file.")
  } else {
    return(window_size)
  }
}
