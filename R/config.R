# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


########## Configuration File Parser Functions ##########

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
parse_config <- function(config_path) {

  conf <- yaml::read_yaml(config_path)

  if (is.null(conf)) {
    warning("Path does not exist.")
  }

  return(conf)
}

##### File Directory Function

#' Creates file directories needed by the configuration file (.yml).
#'
#' @description The input is the parsed config file that the user will be using.
#' It will take the parsed config to get the filepaths that need to be created to
#' use the config file.
#'
#' @param config The parsed config file the function is creating the directory for.
#' @param verbose A boolean variable that prints operational messages when set to TRUE.
#' Obtained from `parse_config` function.
create_file_directory <- function(conf, verbose = TRUE) {
  # Create the git_repo folder
  create_file_path(conf$version_control$log)

  # Create the mailing_list directory, if needed
  if (!is.null(conf$mailing_list)) {
    # Check if there is mod_mbox
    if (!is.null(conf$mailing_list$mod_mbox)) {
      # Create for each project key
      project_keys <- names(conf$mailing_list$mod_mbox)
      for (key in project_keys) {
        mailing_list <- conf$mailing_list$mod_mbox[[key]]
        create_file_path(mailing_list$save_folder_path)
      }
    } else {
      if (verbose) {
        message("No mod_mbox found")
      }
    }
    # Check if there is pipermail
    if (!is.null(conf$mailing_list$pipermail)) {
      # Create for each project key
      project_keys <- names(conf$mailing_list$pipermail)
      for (key in project_keys) {
        mailing_list <- conf$mailing_list$pipermail[[key]]
        create_file_path(mailing_list$save_folder_path)
      }
    } else {
      if (verbose) {
        message("No pipermail found")
      }
    }
  } else {
    if (verbose) {
      message("No mailing_list found")
    }
  }

  # Create the issue_tracker directory, if needed
  if (!is.null(conf$issue_tracker)) {
    # Check for jira
    if (!is.null(conf$issue_tracker$jira)) {
      # Create for each project key
      project_keys <- names(conf$issue_tracker$jira)
      for (key in project_keys) {
        issue_tracker <- conf$issue_tracker$jira[[key]]
        create_file_path(issue_tracker$issues)
        create_file_path(issue_tracker$issue_comments)
      }
    } else {
      if (verbose) {
        message("No jira found")
      }
    }
    # Check for github
    if (!is.null(conf$issue_tracker$github)) {
      # Create for each project key
      project_keys <- names(conf$issue_tracker$github)
      for (key in project_keys) {
        issue_tracker <- conf$issue_tracker$github[[key]]
        create_file_path(issue_tracker$issue_or_pr_comment)
        create_file_path(issue_tracker$issue)
        create_file_path(issue_tracker$issue_search)
        create_file_path(issue_tracker$issue_event)
        create_file_path(issue_tracker$pull_request)
        create_file_path(issue_tracker$commit)
      }
    } else {
      if (verbose) {
        message("No github found")
      }
    }
    # Check for bugzilla
    if (!is.null(conf$issue_tracker$bugzilla)) {
      # Create for each project key
      project_keys <- names(conf$issue_tracker$bugzilla)
      for (key in project_keys) {
        issue_tracker <- conf$issue_tracker$bugzilla[[key]]
        create_file_path(issue_tracker$issues)
        create_file_path(issue_tracker$issue_comments)
      }
    } else {
      if (verbose) {
        message("No bugzilla found")
      }
    }
  } else {
    if (verbose) {
      message("No issue_tracker found")
    }
  }

  # Create the tools directory, if needed
  if (!is.null(conf$tool)) {
    # Check for dv8
    if (!is.null(conf$tool$dv8)) {
      create_file_path(conf$tool$dv8$folder_path)
    } else {
      if (verbose) {
        message("dv8 is unused")
      }
    }
    # Check for srcml
    if (!is.null(conf$tool$srcml)) {
      create_file_path(conf$tool$srcml$srcml_path)
    } else {
      if (verbose) {
        message("srcml is unused")
      }
    }
    # Check for pattern4
    if (!is.null(conf$tool$pattern4)) {
      create_file_path(conf$tool$pattern4$class_folder_path)
      create_file_path(conf$tool$pattern4$output_filepath)
    } else {
      if (verbose) {
        message("pattern4 is unused")
      }
    }
    # Check for understand
    if (!is.null(conf$tool$understand)) {
      create_file_path(conf$tool$understand$project_path)
      create_file_path(conf$tool$understand$output_path)
    }
  } else {
    if (verbose) {
      message("no tools used")
    }
  }
}

#' A helper function for `create_file_directory`.
#'
#' @description The function checks if a filepath exists on the local device.
#' If the filepath does not exist, then the function creates it.
#'
#' @param filepath The filepath to create.
#' @param verbose A boolean variable that prints the operational messages when set to TRUE.
create_file_path <- function(filepath, verbose= TRUE) {
  if (!is.null(filepath)) {
    # Check if the filepath already exists
    if (dir.exists(filepath)) {
      if (verbose) {
        message("Filepath: ", filepath, " already exists.")
      }
    } else {
      # Create the filepath
      dir.create(filepath, recursive= TRUE)
      if (verbose) {
        message("Created filepath: ", filepath)
      }
    }
  } else {
    if (verbose) {
      message("Invalid filepath: ", filepath)
    }
  }
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of git branches.
#' @export
get_git_branches <- function(config_file) {

  git_branch <- config_file[["version_control"]][["branch"]]

  if (is.null(git_branch)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(git_branch)
}



##### Mailing List Functions Start #####

#' Returns the list of mailing list mod mbox project keys.
#'
#' @description This function returns the list of mailing list mod mbox project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of mod mbox mailing list keys.
#' @export
get_mbox_key_indexes <- function(config_file) {

  mbox_keys <- config_file[["mailing_list"]][["mod_mbox"]]

  if (is.null(mbox_keys)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_keys)
}

#' Returns the URL to the archives for mbox for a specific project key.
#'
#' @description This function returns the URL to the archives for a specific
#' project key, `project_key_index`, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the specific URL to the archives for
#' mbox exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The URL of the mbox mailing list archive for project specified by key `project_key_index`.
#' @export
get_mbox_domain <- function(config_file, project_key_index) {

  mbox_url <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mailing_list"]]

  if (is.null(mbox_url)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_url)
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local mbox path for project specified by key `project_key_index`.
#' @export
get_mbox_path <- function(config_file, project_key_index) {

  mbox_path <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["save_folder_path"]]

  if (is.null(mbox_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_path)
}

#' Returns the local input file for mbox for a specific project key.
#'
#' @description This function returns the local file used for input for
#' mbox for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local input file path for mbox exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local input file mbox path for project specified by key `project_key_index`.
#' @export
get_mbox_input_file <- function(config_file, project_key_index) {

  mbox_input <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mbox_file_path"]]

  if (is.null(mbox_input)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_input)
}

#' Returns the URL to the archives for pipermail for a specific project key.
#'
#' @description This function returns the URL to the archives for a specific
#' project key, `project_key_index`, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the specific URL to the archives for
#' pipermail exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The URL of the pipermail mailing list archive for project specified by key `project_key_index`.
#' @export
get_pipermail_domain <- function(config_file, project_key_index) {

  pipermail_url <- config_file[["mailing_list"]][["pipermail"]][[project_key_index]][["mailing_list"]]

  if (is.null(pipermail_url)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pipermail_url)
}

#' Returns the local folder path to store pipermail data for a specific project key.
#'
#' @description This function returns the local folder path used to store
#' pipermail data for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local folder path to store pipermail data exists in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local pipermail path for project specified by key `project_key_index`.
#' @export
get_pipermail_path <- function(config_file, project_key_index) {

  pipermail_path <- config_file[["mailing_list"]][["pipermail"]][[project_key_index]][["save_folder_path"]]

  if (is.null(pipermail_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pipermail_path)
}

#' Returns the local input file for pipermail for a specific project key.
#'
#' @description This function returns the local file used for input for
#' pipermail for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local input file path for pipermail exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local input file pipermail path for project specified by key `project_key_index`.
#' @export
get_pipermail_input_file <- function(config_file, project_key_index) {

  pipermail_input <- config_file[["mailing_list"]][["pipermail"]][[project_key_index]][["mbox_file_path"]]

  if (is.null(pipermail_input)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pipermail_input)
}

##### Mailing List Functions End #####

##### Issue Tracker Functions Start #####

##### Jira Functions #####

#' Returns the list of Jira issue tracker project keys.
#'
#' @description This function returns the list of Jira issue tracker project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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

##### Github Functions #####

#' Returns the list of GitHub issue tracker project keys.
#'
#' @description This function returns the list of GitHub issue tracker project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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

### Bugzilla Functions #####

#' Returns the name of the Bugzilla project key for a specific project key index.
#'
#' @description This function returns the name of the Bugzilla project key for
#' a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the name of the Bugzilla project key
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Bugzilla project key name for project specified by key `project_key_index`.
#' @export
get_bugzilla_project_key <- function(config_file, project_key_index) {

  bugzilla_key <- config_file[["issue_tracker"]][["bugzilla"]][[project_key_index]][["project_key"]]

  if (is.null(bugzilla_key)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(bugzilla_key)
}

#' Returns the local folder path for Bugzilla issues for a specific project key.
#'
#' @description This function returns the local folder path for Bugzilla issues
#' for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the folder path for Bugzilla issues
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for Bugzilla issues for project specified by key `project_key_index`.
#' @export
get_bugzilla_issue_path <- function(config_file, project_key_index) {

  issue_path <- config_file[["issue_tracker"]][["bugzilla"]][[project_key_index]][["issues"]]

  if (is.null(issue_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_path)
}

#' Returns the local folder path for Bugzilla issue comments for a specific project key.
#'
#' @description This function returns the local folder path for Bugzilla issue
#' comments for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the folder path for Bugzilla issue
#' comments exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for Bugzilla issue comments for project specified by key `project_key_index`.
#' @export
get_bugzilla_issue_comment_path <- function(config_file, project_key_index) {

  issue_comment_path <- config_file[["issue_tracker"]][["bugzilla"]][[project_key_index]][["issue_comments"]]

  if (is.null(issue_comment_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_comment_path)
}

##### Issue Tracker Functions End #####



##### Vulnerabilities Functions Start #####

#' Returns the local folder path that contains the nvd (National Vulnerability
#' Database) feeds.
#'
#' @description This function returns the local folder path for nvd feeds,
#' that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the local folder path for the nvd feeds exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The folder path with nvd feeds.
#' @export
get_nvdfeed_folder_path <- function(config_file) {

  nvdfeed_folder_path <- config_file[["vulnerabilities"]][["nvd_feed"]]

  if (is.null(nvdfeed_folder_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(nvdfeed_folder_path)
}

##### Vulnerabilities Functions End #####



##### Regular Expression Functions Start #####

#' Returns the issue Id regular expression for commit messages.
#'
#' @description This function returns the issue Id regular expression for commit
#' messages, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the issue Id regular expression for commit messages exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The commit message issue Id regular expression.
#' @export
get_issue_id_regex <- function(config_file) {

  issue_id_regex <- config_file[["commit_message_id_regex"]][["issue_id"]]

  if (is.null(issue_id_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_id_regex)
}

#' Returns the cve (Common Vulnerabilities and Exposures) regular expression
#' for commit messages.
#'
#' @description This function returns the cve regular expression for commit
#' messages, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the cve regular expression for commit messages exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The commit message CVE regular expression.
#' @export
get_cveid_regex <- function(config_file) {

  cveid_regex <- config_file[["commit_message_id_regex"]][["cve_id"]]

  if (is.null(cveid_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(cveid_regex)
}

##### Regular Expression Functions End #####



##### Filter Functions Start #####

#' Returns the list of file extensions used for filtering files to keep.
#'
#' @description This function returns the list of file extensions that will be
#' used for filtering files specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of file extensions exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The commit size to filter out.
#' @export
get_filter_commit_size <- function(config_file) {

  filter_commit_size <- config_file[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  if (is.null(filter_commit_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(filter_commit_size)
}

##### Filter Functions End #####



##### Third Party Tools Functions Start #####

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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The specified `tool_name` tool project from `config_file`.
#' @export
get_tool_project <- function(tool_name, config_file) {

  tool_path <- config_file[[tool_name]]

  if (is.null(tool_path)) {
    warning("Attribute does not exist.")
  }

  return(tool_path)
}

#' Returns the depends code language for analysis.
#'
#' @description This function returns the specified code language that should
#' be used to parse file-file static dependencies with the depends tool, that
#' is specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the depends code language exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The code language for parsing file-file static dependencies.
#' @export
get_depends_code_language <- function(config_file) {

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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return A list of the types of depends dependencies to keep for analysis.
#' @export
get_depends_keep_dependencies_type <- function(config_file) {

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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of DV8 architectural flaws thresholds.
#' @export
get_dv8_flaws_params <- function(config_file) {

  dv8_flaws_params <- config_file[["tool"]][["dv8"]][["architectural_flaws"]]

  if (is.null(dv8_flaws_params)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(dv8_flaws_params)
}

#' Returns the types to keep to to be considered for analysis.
#'
#' @description This function returns the types of file-file dependencies that
#' should be considered, that are specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the lines type to keep exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The lines type to keep for analysis.
#' @export
get_uctags_line_types <- function(config_file) {

  kinds <- config_file[["tool"]][["uctags"]][["keep_lines_type"]]

  if (is.null(kinds)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(kinds)
}

#' Returns the file path for the output of the srcML analysis for the project.
#'
#' @description This function returns the file path to be used to store the
#' output of the srcML analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the file path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The Pattern4 output folder path.
#' @export
get_pattern4_filepath <- function(config_file) {

  pattern4_filepath <- config_file[["tool"]][["pattern4"]][["output_filepath"]]

  if (is.null(pattern4_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pattern4_filepath)
}

#' Returns the understand code language for analysis.
#'
#' @description This function returns the specified code language that should
#' be used to parse dependencies with the understand tool, that
#' is specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the understand code language exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The code language for parsing with the understand tool.
#' @export
get_understand_code_language <- function(config_file) {

  language <- config_file[["tool"]][["understand"]][["code_language"]]

  if (is.null(language)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(language)
}

#' Returns a list of the types of understand dependencies to keep for analysis.
#'
#' @description This function returns the specified types of dependencies to
#' keep for analysis with the understand tool, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the list of the
#' types of understand dependencies exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return A list of the types of understand dependencies to keep for analysis.
#' @export
get_understand_keep_dependencies_type <- function(config_file) {

  keep_dependencies_type <- config_file[["tool"]][["understand"]][["keep_dependencies_type"]]

  if (is.null(keep_dependencies_type)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keep_dependencies_type)
}

#' Returns the folder path for the input of the understand analysis.
#'
#' @description This function returns the folder path that contains the
#' input of the understand analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The understand project folder path.
#' @export
get_understand_project_path <- function(config_file) {

  understand_project_path <- config_file[["tool"]][["understand"]][["project_path"]]

  if (is.null(understand_project_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(understand_project_path)
}

#' Returns the folder path for the output of the understand analysis.
#'
#' @description This function returns the folder path that contains the
#' output of the understand analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The understand output folder path.
#' @export
get_understand_output_path <- function(config_file) {

  understand_output_path <- config_file[["tool"]][["understand"]][["output_path"]]

  if (is.null(understand_output_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(understand_output_path)
}

##### Third Party Tools Functions End #####



##### Analysis Functions Start #####

#' Returns the list of topics and keywords for analysis.
#'
#' @description This function returns the list of keywords and topics for
#' analysis, that is specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of keywords and topics exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of keywords and topics for analysis.
#' @export
get_topics <- function(config_file) {

  topics <- config_file[["analysis"]][["topics"]]

  if (is.null(topics)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(topics)
}

#' Returns the starting commit for a window for analysis.
#'
#' @description This function returns the starting commit for a window of time
#' for analysis (the time stamp is inferred from gitlog), that is specified in
#' the input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the start commit
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
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
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The size of a window for analysis.
#' @export
get_window_size <- function(config_file) {

  window_size <- config_file[["analysis"]][["window"]][["size_days"]]

  if (is.null(window_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(window_size)
}

#' Returns the list of enumerated commit intervals for analysis.
#'
#' @description This function returns a list of enumerated commit intervals,
#' that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the list of enumerated commit intervals exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of enumerated commit intervals.
#' @export
get_enumeration_commits <- function(config_file) {

  enumeration_commit <- config_file[["analysis"]][["enumeration"]][["commit"]]

  if (is.null(enumeration_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(enumeration_commit)
}

##### Analysis Functions End #####
