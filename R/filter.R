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
