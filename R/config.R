# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Configuration File Getter Functions ##############

## Variables ##
tool <- yaml::read_yaml("tools.yml")
conf_apr <- yaml::read_yaml("conf/apr.yml")
conf_camel <- yaml::read_yaml("conf/camel.yml")
conf_geronimo <- yaml::read_yaml("conf/geronimo.yml")
conf_junit5 <- yaml::read_yaml("conf/junit5.yml")


#### tools.yml ####

#' Get tools.yml
#'
#' @return the tools.yml file
#' @export
get_tools <- function() {
  return(tool)
}

#' Get srcml path from tools.yml
#'
#' @return the srcml path from tools.yml
#' @export
get_tools_srcml <- function() {
  srcml_path <- tool[["srcml"]]
  return(srcml_path)
}

#' Get perceval path from tools.yml
#'
#' @return the srcml path from tools.yml
#' @export
get_tools_perceval <- function() {
  perceval_path <- tool[["perceval"]]
  return(perceval_path)
}

#' Get utags path from tools.yml
#'
#' @return the srcml path from tools.yml
#' @export
get_tools_utags <- function () {
  utags_path <- tool[["utags"]]
  return(utags_path)
}

#' Get dv8 path from tools.yml
#'
#' @return the dv8 path from tools.yml
#' @export
get_tools_dv8 <- function () {
  dv8_path <- tool[["dv8"]]
  return(dv8_path)
}

#' Get scc path from tools.yml
#'
#' @return the scc path from tools.yml
#' @export
get_tools_scc <- function () {
  scc_path <- tool[["scc"]]
}

#' Get depends path from tools.yml
#'
#' @return the depends path from tools.yml
#' @export
get_tools_depends <- function () {
  depends_jar_path <- tool[["depends"]]
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
#' Gets the project git repo path
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the git repo path from the project config file
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
      stop("This field does not exist in the configuration file.")
    }
  } else {
    stop("This field does not exist in the configuration file.")
  }
}

#' Get File Extensions
#'
#' Gets the file extensions from the config file for filtering
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the file extensions from the project config file
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
#' @return the file extensions from the project config file
#' @export
get_project_substring_filepath <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]
  return(substring_filepath)
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
#' Gets the code language of the project
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the uctags line type from the project config file
#' @export
get_project_code_language <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  language <- conf[["tool"]][["depends"]][["code_language"]]
  return (language)
}

#' Get Issue Id Regex
#'
#' Gets the issue ID regex on commit messages from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the issue id regex from geronimo.yml
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
#' @return the Jira issues path from geronimo.yml
#' @export
get_project_jira_issues_path <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  jira_issues_path <- conf[["issue_tracker"]][["jira"]][["issues"]]
  return(jira_issues_path)
}

#' Get SrcML Filepath
#'
#' Gets the SrcML filepath from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the srcml filepath from junit5.yml
#' @export
get_project_srcml_filepath <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  srcml_filepath <- conf[["tool"]][["srcml"]][["srcml_path"]]
  return(srcml_filepath)
}

#' Get Topics
#'
#' Gets the topics and keywords from the project config file
#'
#' @param project_name the name of the project config file (e.g. "kaiaulu" or "geronimo")
#' @return the topics from junit5.yml
#' @export
get_junit5_topics <- function(project_name) {

  conf_path <- paste0("conf/", project_name, ".yml")
  conf <- yaml::read_yaml(conf_path)

  topics <- conf_junit5[["analysis"]][["topics"]]
  return(topics)
}
