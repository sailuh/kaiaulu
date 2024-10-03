# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

########## Configuration File Getter Functions ##########

##### General Getter Functions #####

#' Returns the parsed configuration file (.yml).
#'
#' @description The input file is expected to be in the .yml format.
#' The function returns a parsed version of the input .yml file, and it will
#' inform the user if the input .yml file path does not exist. The contents
#' of the input .yml file may contain machine-dependent paths that may need to
#' be modified by the user.
#'
#' @param config_path The path of the config file from the kaiaulu directory (e.g. "conf/kaiaulu.yml").
#' @return The parsed config file whose path is specified by `config_path`.
#' @export
get_parsed <- function(config_path) {

  conf <- yaml::read_yaml(config_path)

  if (is.null(conf)) {
    warning("Path does not exist.")
  }

  return(conf)
}

#' Returns the specified tool project from a parsed tool configuration file.
#'
#' @description This function returns a path to a specified tool from a
#' specified parsed tool configuration file. The function takes the input
#' `tool_name` and uses it to index a specific tool project in a parsed
#' tool configuration file, `config_file`, where it then returns the specified
#' tool project. The function will inform the user if the specified attribute,
#' `tool_name`, exists in the parsed configuration file, `config_file`.
#'
#' @param tool_name The name of the tool (e.g. "perceval" or "dv8").
#' @param config_file The parsed configuration file.
#' @return The specified `tool_name` tool project from `config_file`.
#' @export
get_tool_project <- function(tool_name, config_file) {

  tool_path <- config_file[[tool_name]]

  if (is.null(tool_path)) {
    warning("Attribute does not exist.")
  }

  return(tool_path)
}

##### Git Getter Functions #####

#' Returns the path to the .git of the project repository that is being analyzed.
#'
#' @description This function returns the specific path to the .git of the
#' project repository that is being analyzed specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the .git path of the project repository
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The local git repository path specified in `config_file`.
#' @export
get_git_repo_path <- function(config_file) {

  git_repo_path <- config_file[["version_control"]][["log"]]

  if (is.null(git_repo_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(git_repo_path)
}

#' Returns the list of git branches used for analysis in the current project.
#'
#' @description This function returns a list of the git branches used for
#' analysis in the current project specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the list of branches to be analyzed
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of git branches.
#' @export
get_git_branches <- function(config_file) {

  git_branch <- config_file[["version_control"]][["branch"]]

  if (is.null(git_branch)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(git_branch)
}

##### Filtering Getter Functions #####

#' Returns the list of file extensions used for filtering files to keep.
#'
#' @description This function returns the list of file extensions that will be
#' used for filtering files specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of file extensions exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of file extensions to keep.
#' @export
get_file_extensions <- function(config_file) {

  file_extensions <- config_file[["filter"]][["keep_filepaths_ending_with"]]

  if (is.null(file_extensions)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(file_extensions)
}

#' Returns the list of file extensions used for filtering files to remove.
#'
#' @description This function returns the list of file extensions that will be
#' used for filtering files specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of file extensions exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of file extensions to remove.
#' @export
get_substring_filepath <- function(config_file) {

  substring_filepath <- config_file[["filter"]][["remove_filepaths_containing"]]

  if (is.null(substring_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(substring_filepath)
}

#' Returns the commit size threshold to remove file paths.
#'
#' @description This function returns an integer number that represents the
#' threshold for a commit size to remove file paths specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the commit size
#' threshold exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The commit size to filter out.
#' @export
get_filter_commit_size <- function(config_file) {

  filter_commit_size <- config_file[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  if (is.null(filter_commit_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(filter_commit_size)
}

##### Third Party Tools Getter Functions #####

#' Returns the types to keep to to be considered for analysis.
#'
#' @description This function returns the types of file-file dependencies that
#' should be considered, that are specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the lines type to keep exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The lines type to keep for analysis.
#' @export
get_uctags_line_types <- function(config_file) {

  kinds <- config_file[["tool"]][["uctags"]][["keep_lines_type"]]

  if (is.null(kinds)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(kinds)
}

#' Returns the code language for analysis.
#'
#' @description This function returns the specified code language that should
#' be used to parse file-file static dependencies with the depends tool, that
#' is specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the code language exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The code language for parsing file-file static dependencies.
#' @export
get_code_language <- function(config_file) {

  language <- config_file[["tool"]][["depends"]][["code_language"]]

  if (is.null(language)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(language)
}

#' Returns a list of the types of dependencies to keep for analysis.
#'
#' @description This function returns the specified types of dependencies to
#' keep for analysis with the depends tool, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the list of the
#' types of dependencies exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return A list of the types of dependencies to keep for analysis.
#' @export
get_keep_dependencies_type <- function(config_file) {

  keep_dependencies_type <- config_file[["tool"]][["depends"]][["keep_dependencies_type"]]

  if (is.null(keep_dependencies_type)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keep_dependencies_type)
}


#' Returns the path to the folder used to store files for DV8 analysis.
#'
#' @description This function returns the path to the folder that will be
#' used to store various intermediate files for DV8 analysis, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the path
#' path to the folder for intermediate file storage for DV8 analysis exists in
#' the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The DV8 project folder path.
#' @export
get_dv8_folder_path <- function(config_file) {

  project_path <- config_file[["tool"]][["dv8"]][["folder_path"]]

  if (is.null(project_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(project_path)
}

#' Returns the list of architectural flaws thresholds for DV8 analysis.
#'
#' @description This function returns the list of architectural flaws thresholds
#' for DV8 analysis, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the list of architectural flaws thresholds
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of DV8 architectural flaws thresholds.
#' @export
get_dv8_flaws_params <- function(config_file) {

  dv8_flaws_params <- config_file[["tool"]][["dv8"]][["architectural_flaws"]]

  if (is.null(dv8_flaws_params)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(dv8_flaws_params)
}

#' Returns the file path for the output of the srcML analysis for the project.
#'
#' @description This function returns the file path to be used to store the
#' output of the srcML analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the file path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The output file path for srcML analysis.
#' @export
get_srcml_filepath <- function(config_file) {

  srcml_filepath <- config_file[["tool"]][["srcml"]][["srcml_path"]]

  if (is.null(srcml_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(srcml_filepath)
}

#' Returns the folder path for class pattern4 analysis.
#'
#' @description This function returns the folder path used to store the classes
#' for the pattern4 analysis for the project, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The Pattern4 class folder path.
#' @export
get_pattern4_folder_path <- function(config_file) {

  pattern4_folder_path <- config_file[["tool"]][["pattern4"]][["class_folder_path"]]

  if (is.null(pattern4_folder_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pattern4_folder_path)
}

#' Returns the folder path for the output of the pattern4 analysis.
#'
#' @description This function returns the folder path that contains the
#' output of the pattern4 analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The Pattern4 output folder path.
#' @export
get_pattern4_filepath <- function(config_file) {

  pattern4_filepath <- config_file[["tool"]][["pattern4"]][["output_filepath"]]

  if (is.null(pattern4_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pattern4_filepath)
}

##### Mailing List Getter Functions #####

#' Returns the list of mailing list mod mbox project keys.
#'
#' @description This function returns the list of mailing list mod mbox project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of mod mbox mailing list keys.
#' @export
get_mbox_key_indexes <- function(config_file) {

  mbox_keys <- config_file[["mailing_list"]][["mod_mbox"]]

  if (is.null(mbox_keys)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_keys)
}

#' Returns the local folder path to store mbox data for a specific project key.
#'
#' @description This function returns the local folder path used to store
#' mbox data for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local folder path to store mbox data exists in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local mbox path for project specified by key `project_key_index`.
#' @export
get_mbox_path <- function(config_file, project_key_index) {

  mbox_path <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mbox"]]

  if (is.null(mbox_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_path)
}

#' Returns the URL to the archives for mbox for a specific project key.
#'
#' @description This function returns the URL to the archives for a specific
#' project key, `project_key_index`, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the specific URL to the archives for
#' mbox exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The URL of the mailing list archive for project specified by key `project_key_index`.
#' @export
get_mbox_domain <- function(config_file, project_key_index) {

  mbox_url <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mailing_list"]]

  if (is.null(mbox_url)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_url)
}

#' Returns the mbox mailing list for a specific project key.
#'
#' @description This function returns the specific mbox mailing list for a
#' specific project key, `project_key_index`, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the mailing
#' list for mbox exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The mbox mailing list for project specified by key `project_key_index`.
#' @export
get_mbox_mailing_list <- function(config_file, project_key_index) {

  mailing_list <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mailing_list_type"]]

  if (is.null(mailing_list)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mailing_list)
}

#' Returns the mbox archive type for a specific project key.
#'
#' @description This function returns the specific mbox archive type for a
#' specific project key, `project_key_index`, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the archive type
#' for mbox exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The mbox archive type for project specified by key `project_key_index`.
#' @export
get_mbox_archive_type <- function(config_file, project_key_index) {

  archive_type <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["archive_type"]]

  if (is.null(archive_type)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(archive_type)
}

##### Issue Tracker Getter Functions #####

#' Returns the list of GitHub issue tracker project keys.
#'
#' @description This function returns the list of GitHub issue tracker project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of GitHub issue tracker project keys.
#' @export
get_github_keys <- function(config_file) {

  keys <- config_file[["issue_tracker"]][["github"]]

  if (is.null(keys)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keys)
}

#' Returns the owner for a GitHub repository for a specific project key.
#'
#' @description This function returns the owner for a GitHub repository for a
#' specific project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the owner for the GitHub repository exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The GitHub project owner name for project specified by key `project_key_index`.
#' @export
get_github_owner <- function(config_file, project_key_index) {

  owner <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["owner"]]

  if (is.null(owner)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(owner)
}

#' Returns the name of the GitHub repository for a specific project key.
#'
#' @description This function returns the name of the GitHub repository for a
#' specific project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the name of the GitHub repository exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The name of the GitHub repository for project specified by key `project_key_index`.
#' @export
get_github_repo <- function(config_file, project_key_index) {

  repo <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["repo"]]

  if (is.null(repo)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(repo)
}

#' Returns the local folder path for GitHub issues for a specific project key.
#'
#' @description This function returns the local folder path for GitHub issues
#' for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the folder path for GitHub issues exists
#' in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub issues for project specified by key `project_key_index`.
#' @export
get_github_issue_path <- function(config_file, project_key_index) {

  issue_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue"]]

  if (is.null(issue_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_path)
}

#' Returns the local folder path for GitHub Issue or Pull Request comments for
#' a specific project key.
#'
#' @description This function returns the local folder path for GitHub Issue or
#' Pull Request comments for a specific project key, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the comments exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub Issues or PR comments for project specified by key `project_key_index`.
#' @export
get_github_issue_or_pr_comment_path <- function(config_file, project_key_index) {

  issue_or_pr_comment_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_or_pr_comment"]]

  if (is.null(issue_or_pr_comment_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_or_pr_comment_path)
}

#' Returns the local folder path for GitHub Issue Searches for a specific
#' project key.
#'
#' @description This function returns the local folder path for GitHub Issue
#' Searches for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the local folder path for the issue
#' searches exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub issue search for project specified by key `project_key_index`.
#' @export
get_github_issue_search_path <- function(config_file, project_key_index) {

  issue_search_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_search"]]

  if (is.null(issue_search_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_search_path)
}

#' Returns the local folder path for GitHub Pull Requests for a specific
#' project key.
#'
#' @description This function returns the local folder path for GitHub Pull
#' Requests for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the pull requests exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub pull requests for project specified by key `project_key_index`.
#' @export
get_github_pull_request_path <- function(config_file, project_key_index) {

  pull_request_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["pull_request"]]

  if (is.null(pull_request_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pull_request_path)
}

#' Returns the local folder path for GitHub issue events for a specific project
#' key.
#'
#' @description This function returns the local folder path for GitHub issue
#' events for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the issue events exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub issue events for project specified by key `project_key_index`.
#' @export
get_github_issue_event_path <- function(config_file, project_key_index) {

  issue_event_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_event"]]

  if (is.null(issue_event_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_event_path)
}

#' Returns the local folder path for GitHub commits for a specific project key.
#'
#' @description This function returns the local folder path for GitHub commits
#' for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the commits exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub commits for project specified by key `project_key_index`.
#' @export
get_github_commit_path <- function(config_file, project_key_index) {

  commit_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["commit"]]

  if (is.null(commit_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(commit_path)
}

#' Returns the list of Jira issue tracker project keys.
#'
#' @description This function returns the list of Jira issue tracker project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of Jira issue tracker project keys.
#' @export
get_jira_keys <- function(config_file) {

  jira_key <- config_file[["issue_tracker"]][["jira"]]

  if (is.null(jira_key)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_key)
}

#' Returns the Jira project domain for a specific project key.
#'
#' @description This function returns the Jira project domain for a specific
#' project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the domain exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Jira domain for project specified by key `project_key_index`.
#' @export
get_jira_domain <- function(config_file, project_key_index) {

  domain <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["domain"]]

  if (is.null(domain)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(domain)
}

#' Returns the name of the Jira project key for a specific project key.
#'
#' @description This function returns the Jira project key name for a specific
#' project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the project key name exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Jira project key name for project specified by key `project_key_index`.
#' @export
get_jira_project_key_name <- function(config_file, project_key_index) {

  name <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["project_key"]]

  if (is.null(name)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(name)
}

#' Returns the local folder path for Jira issues for a specific project key.
#'
#' @description This function returns the folder path for Jira issues for a
#' specific project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the folder path for Jira issues exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Jira issue folder path for project specified by key `project_key_index`.
#' @export
get_jira_issues_path <- function(config_file, project_key_index) {

  jira_issues_path <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["issues"]]

  if (is.null(jira_issues_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_issues_path)
}

#' Returns the local folder path for Jira issue comments for a specific
#' project key.
#'
#' @description This function returns the local folder path for Jira issue
#' comments for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the comments exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The folder path for Jira issue comments for project specified by key `project_key_index`.
#' @export
get_jira_issues_comments_path <- function(config_file, project_key_index) {

  jira_issue_comments_path <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["issue_comments"]]

  if (is.null(jira_issue_comments_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_issue_comments_path)
}

#' Returns the name of the Bugzilla project key.
#'
#' @description This function returns the name of the Bugzilla project key, that
#' is specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the name of the Bugzilla project key exists in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The Bugzilla project key name.
#' @export
get_bugzilla_project_key <- function(config_file) {

  bugzilla_key <- config_file[["issue_tracker"]][["bugzilla"]][["project_key"]]

  if (is.null(bugzilla_key)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(bugzilla_key)
}

##### Vulnerability Getter Functions #####

#' Returns the local folder path that contains the nvd (National Vulnerability
#' Database) feeds.
#'
#' @description This function returns the local folder path for nvd feeds,
#' that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the local folder path for the nvd feeds exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The folder path with nvd feeds.
#' @export
get_nvdfeed_folder_path <- function(config_file) {

  nvdfeed_folder_path <- config_file[["vulnerabilities"]][["nvd_feed"]]

  if (is.null(nvdfeed_folder_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(nvdfeed_folder_path)
}

##### Regular Expression Getter Functions #####

#' Returns the cve (Common Vulnerabilities and Exposures) regular expression
#' for commit messages.
#'
#' @description This function returns the cve regular expression for commit
#' messages, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the cve regular expression for commit messages exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The commit message CVE regular expression.
#' @export
get_cveid_regex <- function(config_file) {

  cveid_regex <- config_file[["commit_message_id_regex"]][["cve_id"]]

  if (is.null(cveid_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(cveid_regex)
}

#' Returns the issue Id regular expression for commit messages.
#'
#' @description This function returns the issue Id regular expression for commit
#' messages, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the issue Id regular expression for commit messages exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The commit message issue Id regular expression.
#' @export
get_issue_id_regex <- function(config_file) {

  issue_id_regex <- config_file[["commit_message_id_regex"]][["issue_id"]]

  if (is.null(issue_id_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_id_regex)
}

##### Analysis Getter Functions #####

#' Returns the list of enumerated commit intervals for analysis.
#'
#' @description This function returns a list of enumerated commit intervals,
#' that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the list of enumerated commit intervals exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of enumerated commit intervals.
#' @export
get_enumeration_commits <- function(config_file) {

  enumeration_commit <- config_file[["analysis"]][["enumeration"]][["commit"]]

  if (is.null(enumeration_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(enumeration_commit)
}

#' Returns the starting commit for a window for analysis.
#'
#' @description This function returns the starting commit for a window of time
#' for analysis (the time stamp is inferred from gitlog), that is specified in
#' the input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the start commit
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The start commit for a window for analysis.
#' @export
get_window_start_commit <- function(config_file) {

  start_commit <- config_file[["analysis"]][["window"]][["start_commit"]]

  if (is.null(start_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(start_commit)
}

#' Returns the ending commit for a window for analysis.
#'
#' @description This function returns the ending commit for a window of time
#' for analysis (the time stamp is inferred from gitlog), that is specified in
#' the input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the end commit
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The end commit for a window for analysis.
#' @export
get_window_end_commit <- function(config_file) {

  end_commit <- config_file[["analysis"]][["window"]][["end_commit"]]

  if (is.null(end_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(end_commit)
}

#' Returns the size of a window for analysis.
#'
#' @description This function returns the size of a window, that is
#' specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the window size exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The size of a window for analysis.
#' @export
get_window_size <- function(config_file) {

  window_size <- config_file[["analysis"]][["window"]][["size_days"]]

  if (is.null(window_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(window_size)
}

#' Returns the list of topics and keywords for analysis.
#'
#' @description This function returns the list of keywords and topics for
#' analysis, that is specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of keywords and topics exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file.
#' @return The list of keywords and topics for analysis.
#' @export
get_topics <- function(config_file) {

  topics <- config_file[["analysis"]][["topics"]]

  if (is.null(topics)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(topics)
}
