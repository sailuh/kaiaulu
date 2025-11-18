# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Filter commit files by extension
#'
#' Filters a data.table containing filepaths using the specified extensions
#'
#' @param dt_file any data.table containing filepaths
#' @param extension a character vector of extensions (e.g. c(py,java)) to *keep*
#' in the table
#' @param file_column_name a string indicating the column name which contains filepaths
#' @return a data.table which contains only filepaths with the specified extensions
#' @export
#' @family {filters}
#' @seealso \code{\link{parse_gitlog}} and \code{\link{parse_dependencies}} to create dt_file
filter_by_file_extension <- function(dt_file,extension,file_column_name){
  file_extension_re <- stri_c('[.](',stri_c(extension,collapse="|"),')$')
  is_file_with_extension <- stri_detect_regex(dt_file[[file_column_name]],file_extension_re)
  return(dt_file[is_file_with_extension])
}
#' Filter by filepath substring
#'
#' Filters a data.table with filepaths using the specified substring (e.g. remove
#' all filepaths which contain the word 'test' anywhere in it)
#'
#' @param dt_file any data.table containing filepaths
#' @param substring a character vector of substrings (e.g. c(py,java)) we wish to *filter*
#' @param file_column_name a string indicating the column name which contains filepaths
#' @return a data.table which contains does *not* contain filepaths with the specified words
#' @export
#' @family filters
#' @seealso \code{\link{parse_gitlog}} and \code{\link{parse_dependencies}} to create dt_file
filter_by_filepath_substring <- function(dt_file,substring,file_column_name){
  file_contains_re <- stri_c('(',stri_c(substring,collapse="|"),')')
  is_not_filepath_with_substring <- !stri_detect_regex(dt_file[[file_column_name]],file_contains_re)
  return(dt_file[is_not_filepath_with_substring])
}

#' Filter Replies by Author Substring
#'
#' Removes rows where the author or recipient address contains certain substrings.
#' Accepts one or more substrings for filtering.
#'
#' @param reply_dt A data.table from \code{\link{parse_jira_replies}}, \code{\link{parse_github_replies}}, or \code{\link{parse_mbox}}.
#' @param substrings A character vector of substrings to filter (e.g., c("bot", "jenkins")).
#' @param file_column_name Character vector of columns to apply the filter to (default: c("reply_from", "reply_to")).
#' @param case_insensitive Logical. Must explicitly pass TRUE or FALSE.
#' @return A filtered data.table without rows matching any of the substrings.
#' @export
#' @family filters
filter_by_reply_author_substring <- function(reply_dt, substrings, file_column_name = c("reply_from", "reply_to"), case_insensitive) {
  if (missing(case_insensitive)) stop("You must provide TRUE or FALSE for case_insensitive")
  
  missing_cols <- setdiff(file_column_name, colnames(reply_dt))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns are missing from reply_dt:", paste(missing_cols, collapse = ", ")))
  }
  
  pattern <- stri_c('(', stri_c(substrings, collapse = "|"), ')')

  filtered_dt <- copy(reply_dt)
  
  for (col in file_column_name) {
    matches <- stri_detect_regex(filtered_dt[[col]], pattern, case_insensitive = case_insensitive)
    matches[is.na(matches)] <- FALSE
    filtered_dt <- filtered_dt[!matches]
  }
  
  return(filtered_dt)
}

#' Filter Replies by Subject Substring
#'
#' Removes rows where the subject contains certain substrings.
#'
#' @param reply_dt A data.table from \code{\link{parse_jira_replies}}, \code{\link{parse_github_replies}} or \code{\link{parse_mbox}}.
#' @param substrings A character vector of substrings to filter.
#' @param case_insensitive Logical. Must explicitly pass TRUE or FALSE.
#' @return A filtered data.table without matching subjects.
#' @export
#' @family filters
filter_by_reply_subject_substring <- function(reply_dt, substrings, case_insensitive) {
  if (missing(case_insensitive)) stop("You must provide TRUE or FALSE for case_insensitive")
  if (!("reply_subject" %in% colnames(reply_dt))) {
    stop("The data.table must contain a 'reply_subject' column")
  }
  
  pattern <- stri_c('(', stri_c(substrings, collapse = "|"), ')')
  is_not_match <- !stri_detect_regex(reply_dt$reply_subject, pattern, case_insensitive = case_insensitive)
  
  return(reply_dt[is_not_match])
}

#' Filter Replies by Body Substring
#'
#' Removes rows where the message body contains certain substrings.
#'
#' @param reply_dt A data.table from \code{\link{parse_jira_replies}}, \code{\link{parse_github_replies}} or \code{\link{parse_mbox}}.
#' @param substrings A character vector of substrings to filter.
#' @param case_insensitive Logical. Must explicitly pass TRUE or FALSE.
#' @return A filtered data.table without matching bodies.
#' @export
#' @family filters
filter_by_reply_body_substring <- function(reply_dt, substrings, case_insensitive) {
  if (missing(case_insensitive)) stop("You must provide TRUE or FALSE for case_insensitive")
  if (!("reply_body" %in% colnames(reply_dt))) {
    stop("The data.table must contain a 'reply_body' column")
  }
  
  pattern <- stri_c('(', stri_c(substrings, collapse = "|"), ')')
  is_not_match <- !stri_detect_regex(reply_dt$reply_body, pattern, case_insensitive = case_insensitive)
  
  return(reply_dt[is_not_match])
}

#' Replace Tokens in specified columns. 
#'
#' Replaces patterns in a specified column of a data.table with token placeholders.
#'
#' @param dt_file A data.table containing the column to process.
#' @param regex_to_replace_key_with A named list of token names and their corresponding regex patterns.
#' @param file_column_name The name of the column to perform replacements on.
#' @return The data.table with regex matches replaced by token names.
#' @export
#' @family filters
replace_token_regex_with <- function(dt_file, regex_to_replace_key_with, file_column_name) {
  
  # Initialize counters if not exists
  if (!exists("counters")) counters <<- list()
  
  # Loop over each token/regex pair
  for (token_name in names(regex_to_replace_key_with)) {
    regex <- regex_to_replace_key_with[[token_name]]
    
    # Count matches for all rows
    matches <- sum(stringi::stri_count_regex(dt_file[[file_column_name]], regex))

    # Apply replacement to every row
    dt_file[[file_column_name]] <- stringi::stri_replace_all_regex(
      dt_file[[file_column_name]],
      regex,
      paste0(" ", token_name, " ")
    )
  }
  
  return(dt_file)
}

#' Filter by commit interval
#'
#' Filters a data.table by with author or commit datetime using the specified start and end commits
#'
#' @param git_log any data.table with a named column `data.AuthorDate` with datetime in POSIXct
#' @param start_commit a commit hash which indicates the start of interval (the commit must exist in `git_log`)
#' @param end_commit a commit hash which indicates the end of interval (the commit must exist in `git_log`)
#' @return a data.table which contains only commits within `start_commit` and `end_commit`
#' @export
#' @family filters
#' @seealso \code{\link{parse_gitlog}} to create git_log
filter_by_commit_interval <- function(git_log,start_commit,end_commit){
  data.AuthorDate <- NULL # due to NSE notes in R CMD check
  start_date <- get_date_from_commit_hash(git_log,start_commit)
  end_date <- get_date_from_commit_hash(git_log,end_commit)
  git_log <- git_log[data.AuthorDate >= start_date & data.AuthorDate <= end_date]
  return(git_log)
}

#' Filter Git Log by all last files changes in snapshot
#'
#' @description Filter a git log to show the last commit change made
#' to a file since the start of the project.
#'
#' @param git_log A parsed git project by \code{parse_gitlog}.
#' @param p_commit_hash A commit hash from git_log
#' @export
filter_by_last_files_change <- function(git_log,p_commit_hash){
  timestamp <- first(git_log[commit_hash == p_commit_hash])$author_datetimetz
  git_log <- git_log[author_datetimetz <= timestamp]
  last_changes_per_file <- git_log[,last(.SD),by=file_pathname]
  return(last_changes_per_file)
}
#' Filter by number of files in commit
#'
#' If a commit modified more than `commit_size` files, then all file changes in `project_git`
#' associated to the commit are deleted from the table. Specifying a `commit_size` is useful
#' to decrease threats to validity on "migration commits" when performing a graph projection
#' on a file-commit network to obtain co-change.(e.g. \code{\link{transform_gitlog_to_hdsmj}}).
#'
#'
#' @param project_git The parsed `project_git` file obtained from \code{\link{parse_gitlog}}.
#' @param commit_size Commits which modify a number of files greater than this threshold are deleted from
#' `project_git`.
#' @return a data.table which only contain file changes to commits less or equal than `commit_size.
#' @export
#' @family filters
filter_by_commit_size <- function(project_git,commit_size = 30){
  nfiles_less_threshold <- project_git[,.(frequency=.N),by="commit_hash"][frequency <= commit_size]$commit_hash
  is_less_than_threshold_files <- project_git$commit_hash %in% nfiles_less_threshold
  project_git <- project_git[is_less_than_threshold_files]
  return(project_git)
}
