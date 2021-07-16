# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Churn Metric
#'
#' Simply adds two columns, expected to be additions and deletions from a file.
#'
#' @param lines_added numeric vector additions to a file due to a commit
#' @param lines_removed numeric vector of deletions to a file due to a commit
#' in the table
#' @return a numeric vector of churn
#' @export
#' @family {metrics}
#' @seealso \code{\link{parse_gitlog}} to obtain additions and deletions from gitlog
metric_churn <- function(lines_added,lines_removed){
    churn <- lines_added + lines_removed
  return(churn)
}
# git_log is a data.table, where each row is identified by a commit. It must contain
# 4 columns in any order but with these column names: commit_hash, date, added, removed.

#' Churn Metric per Commit Interval
#'
#' Calculates the churn metric for a sequence of commits
#'
#' @param git_log a parsed git log table where each row is identified by commit+file
#' @return a single value with the sum of all churn in the commit interval
#' @family {metrics}
#' @seealso \code{\link{parse_gitlog}} to obtain `git_log``
#' @export
metric_churn_per_commit_interval <- function(git_log){
  churn <- NULL # due to NSE notes in R CMD check
  git_log <- metric_churn_per_commit_per_file(git_log)

  # Calculate Churn per Commit
  git_log <- git_log[,.(commit_churn=sum(churn)),by=c("data.commit","data.Author")]

  # Calculate the sum churn of the time interval
  commit_interval_churn <- sum(git_log$commit_churn)
  return(commit_interval_churn)
}
#' Churn Metric per Commit per File
#'
#' Calculates the churn metric for a sequence of commits per commit per file
#'
#' @param git_log a parsed git log table where each row is identified by commit+file
#' @return `git_log` with an additional `churn` column.
#' @family {metrics}
#' @seealso \code{\link{parse_gitlog}} to obtain `git_log`
#' @export
metric_churn_per_commit_per_file <- function(git_log){
  added <- removed <- NULL # due to NSE notes in R CMD check
  # Filter files which do not contain added or removed lines specified (i.e. value is "-")
  git_log <- git_log[added != "-" & removed != "-"]

  # Calculate Churn per Commit per File
  git_log$churn <- metric_churn(as.numeric(git_log$added),
                                as.numeric(git_log$removed))
  return(git_log)
}
#' Commit Message Id Coverage Metric
#'
#' Calculates the number of commits from the git log which contains the message id.
#'
#' @param git_log a parsed git log table where each row is identified by commit+file
#' @param commit_message_id_regex the regex to extract the id from the commit message
#' in the table
#' @return a single numeric value with the number of commits which contains the id
#' @export
#' @family {metrics}
#' @seealso \code{\link{parse_gitlog}} to obtain additions and deletions from gitlog
commit_message_id_coverage <- function(git_log,commit_message_id_regex){
  #data.commit <- data.message <- NULL
  git_log <- unique(git_log[,.(commit_hash,commit_message)])
  is_match <- stringi::stri_detect_regex(git_log$commit_message,
                                         pattern = commit_message_id_regex)
  return(length(is_match[is_match]))
}
# Various imports
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
NULL
