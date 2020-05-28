# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# Conversion to numeric must be performed before function call.
#' @export
metric_churn <- function(lines_added,lines_removed){
  churn <- lines_added + lines_removed
  return(churn)
}
# git_log is a data.table, where each row is identified by a commit. It must contain
# 4 columns in any order but with these column names: commit_hash, date, added, removed.
#' @export
metric_commit_interval_churn <- function(git_log,start_commit,end_commit){

  git_log$churn <- metric_churn(git_log$added,git_log$removed)
  # Calculate Churn per Commit
  project_git_commit_churn <- git_log[,.(commit_churn=sum(churn)),
                                  by=c("commit_hash","date")]
  # Filter files which do not contain added or removed lines specified (i.e. value is "-")
  project_git_commit_churn <- project_git_commit_churn[!is.na(commit_churn)]
  # Can't assume the data is ordered choronologically
  project_git_commit_churn <- project_git_commit_churn[order(date)]
  # Identify the time interval of the date interval of the commit hash interval
  interval_timestamps <- project_git_commit_churn[commit_hash %in%
                                                c(start_commit,end_commit)]$date
  start_date <- interval_timestamps[1]
  end_date <- interval_timestamps[2]
  # Use date interval to calculate the sum churn of the time interval
  commit_interval_churn <- sum(project_git_commit_churn[date >= start_date &
                                                      date <= end_date]$commit_churn)
  return(commit_interval_churn)
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
