# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' File Bug Frequency
#'
#' The total number of commits of all closed bug type issues the file was involved.
#'
#' @param project_git a parsed git log obtained from \code{\link{parse_commit_message_id}}
#' @param jira_issues a parsed jira issue log obtained from \code{\link{parse_jira}}
#' @return a two column data.table of the form file_pathname | file_bug_frequency
#' @export
#' @family metrics
metric_file_bug_frequency <- function(project_git,jira_issues){

  jira_issues_bug <- jira_issues[(issue_status == "Closed" | issue_status == "Resolved") & issue_type == "Bug"][,.(issue_key,issue_type)]
  file_bug_frequency <- merge(project_git[,.(file_pathname,commit_message_id)],
                              jira_issues_bug,all.x=TRUE,by.x="commit_message_id",
                              by.y="issue_key")
  file_bug_frequency <- file_bug_frequency[!is.na(issue_type)]
  file_bug_frequency <- file_bug_frequency[,.(file_bug_frequency=.N),by = "file_pathname"]
  return(file_bug_frequency[,.(file_pathname,file_bug_frequency)])

}

#' File Non Bug Frequency
#'
#' The total number of commits of all closed non-bug type issues the file was involved.
#'
#' @param project_git a parsed git log obtained from \code{\link{parse_commit_message_id}}
#' @param jira_issues a parsed jira issue log obtained from \code{\link{parse_jira}}
#' @return a two column data.table of the form file_pathname | non_file_bug_frequency
#' @export
#' @family metrics
metric_file_non_bug_frequency <- function(project_git,jira_issues){

  jira_issues_non_bug <- jira_issues[(issue_status == "Closed" | issue_status == "Resolved") & issue_type != "Bug"][,.(issue_key,issue_type)]
  file_non_bug_frequency <- merge(project_git[,.(file_pathname,commit_message_id)],
                                  jira_issues_non_bug,all.x=TRUE,by.x="commit_message_id",
                              by.y="issue_key")
  file_non_bug_frequency <- file_non_bug_frequency[!is.na(issue_type)]
  file_non_bug_frequency <- file_non_bug_frequency[,.(file_non_bug_frequency=.N),by = "file_pathname"]
  return(file_non_bug_frequency[,.(file_pathname,file_non_bug_frequency)])

}

#' File Bug Churn
#'
#' The total churn sum of commits of all closed bug type issues the file was involved.
#'
#' @param project_git a parsed git log obtained from \code{\link{parse_commit_message_id}}
#' @param jira_issues a parsed jira issue log obtained from \code{\link{parse_jira}}
#' @return a two column data.table of the form file_pathname | file_bug_churn
#' @export
#' @family metrics
metric_file_bug_churn <- function(project_git,jira_issues){

  project_git <- metric_churn_per_commit_per_file(project_git)
  jira_issues_bug <- jira_issues[(issue_status == "Closed" | issue_status == "Resolved") & issue_type == "Bug"][,.(issue_key,issue_type)]
  file_bug_churn <- merge(project_git[,.(file_pathname,churn,commit_message_id)],
                              jira_issues_bug,all.x=TRUE,by.x="commit_message_id",
                              by.y="issue_key")
  file_bug_churn <- file_bug_churn[!is.na(issue_type)]
  file_bug_churn <- file_bug_churn[,.(file_bug_churn=sum(churn,na.rm=TRUE)),by = "file_pathname"]
  return(file_bug_churn[,.(file_pathname,file_bug_churn)])

}

#' File Non Bug Churn
#'
#' The total churn sum of commits of all closed non-bug type issues the file was involved.
#'
#' @param project_git a parsed git log obtained from \code{\link{parse_commit_message_id}}
#' @param jira_issues a parsed jira issue log obtained from \code{\link{parse_jira}}
#' @return a two column data.table of the form file_pathname | file_non_bug_churn
#' @export
#' @family metrics
metric_file_non_bug_churn <- function(project_git,jira_issues){

  project_git <- metric_churn_per_commit_per_file(project_git)
  jira_issues_non_bug <- jira_issues[(issue_status == "Closed" | issue_status == "Resolved") & issue_type != "Bug"][,.(issue_key,issue_type)]
  file_non_bug_churn <- merge(project_git[,.(file_pathname,churn,commit_message_id)],
                          jira_issues_non_bug,all.x=TRUE,by.x="commit_message_id",
                          by.y="issue_key")
  file_non_bug_churn <- file_non_bug_churn[!is.na(issue_type)]
  file_non_bug_churn <- file_non_bug_churn[,.(file_non_bug_churn=sum(churn,na.rm=TRUE)),by = "file_pathname"]
  return(file_non_bug_churn[,.(file_pathname,file_non_bug_churn)])

}

#' File Churn
#'
#' The total churn of a file
#'
#' @param project_git a parsed git log obtained from \code{\link{parse_gitlog}}
#' @return a two column data.table of the form file_pathname | file_churn
#' @export
#' @family metrics
metric_file_churn <- function(project_git){

  project_file_churn <- metric_churn_per_commit_per_file(project_git)
  project_file_churn <- project_file_churn[,.(file_churn=sum(churn)),by="file_pathname"]
  return(project_file_churn[,.(file_pathname,file_churn)])

}

#' Churn Metric
#'
#' Simply adds two columns, expected to be additions and deletions from a file.
#'
#' @param lines_added numeric vector additions to a file due to a commit
#' @param lines_removed numeric vector of deletions to a file due to a commit
#' in the table
#' @return a numeric vector of churn
#' @export
#' @family metrics
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
#' @family metrics
#' @seealso \code{\link{parse_gitlog}} to obtain `git_log``
#' @export
metric_churn_per_commit_interval <- function(git_log){
  churn <- NULL # due to NSE notes in R CMD check
  git_log <- metric_churn_per_commit_per_file(git_log)

  # Calculate Churn per Commit
  git_log <- git_log[,.(commit_churn=sum(churn)),by=c("commit_hash","author_datetimetz")]

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
#' @family metrics
#' @seealso \code{\link{parse_gitlog}} to obtain `git_log`
#' @export
metric_churn_per_commit_per_file <- function(git_log){
  added <- removed <- NULL # due to NSE notes in R CMD check
  # Filter files which do not contain added or removed lines specified (i.e. value is "-")
  git_log <- git_log[lines_added != "-" & lines_removed != "-"]

  # Calculate Churn per Commit per File
  git_log$churn <- metric_churn(as.numeric(git_log$lines_added),
                                as.numeric(git_log$lines_removed))
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
#' @family metrics
#' @seealso \code{\link{parse_gitlog}} to obtain additions and deletions from gitlog
commit_message_id_coverage <- function(git_log,commit_message_id_regex){
  #data.commit <- data.message <- NULL
  git_log <- unique(git_log[,.(commit_hash,commit_message)])
  is_match <- stringi::stri_detect_regex(git_log$commit_message,
                                         pattern = commit_message_id_regex)
  return(length(is_match[is_match]))
}

#' Productivity Author Commits
#'
#' Counts the unique number of commits per author in a rolling window based on 
#' the git log.
#'
#' @param project_git_log a parsed git log obtained from \code{\link{parse_gitlog}}
#' @param lag The number of days to look back for the rolling window (90 day default)
#' @return A three column data.table of the form author_name_email | author_datetimetz | author_total_commits.
#' For each row (each timestamp in author_datetimetz), author_total_commits is the number of unique 
#' commits by that author in the preceding 90 days ending at that timestamp.
#' @export
#' @family metrics
#' @seealso \code{\link{parse_gitlog}}
productivity_author_commits <- function(project_git_log, lag = 90) {
  author_name_email <- author_datetimetz <- commit_hash <- NULL

  # Determine timezone from the author_datetimetz column in project_git_log
  tz_val <- attr(project_git_log$author_datetimetz, "tzone")

  # Coerce project_git_log to a data.table
  dt <- data.table::as.data.table(project_git_log)[, .(
    author_name_email,
    author_datetimetz = as.POSIXct(author_datetimetz, tz = tz_val),
    commit_hash
  )]

  # Order data
  # Order rows by author_name_email and author_datetimetz in ascending order
  data.table::setorder(dt, author_name_email, author_datetimetz)
  data.table::setkey(dt, author_name_email, author_datetimetz)

  # Rolling window
  result <- dt[, {
    window_start <- stringi::stri_datetime_add(
      author_datetimetz,
      days = -lag,
      tz = tz_val
    )

    author_total_commits <- dt[.SD,
      on = .(author_name_email,
             author_datetimetz >= window_start,
             author_datetimetz <= author_datetimetz),
      .(val = data.table::uniqueN(commit_hash)),
      by = .EACHI]$val

    .(
      author_name_email = author_name_email,
      author_datetimetz = author_datetimetz,
      author_total_commits = author_total_commits
    )

  }, by = .(author_name_email, author_datetimetz)]

  return(result[])
}

#' Productivity Author Churn
#'
#' Calculates the churn per author in a rolling window based on the git log.
#'
#' @param project_git_log a parsed git log obtained from \code{\link{parse_gitlog}}
#' @param lag The number of days to look back for the rolling window (90 day default)
#' @return a five column data.table of the form author_name_email | author_datetimetz | lines_added | 
#' lines_removed | author_churn. 
#' At each timestamp, the lines_added, lines_removed, and author_churn are the totals across all rows 
#' for that author within the prior 90 days ending at that timestamp.
#' @export
#' @family metrics
#' @seealso \code{\link{parse_gitlog}} to obtain additions and deletions from gitlog
productivity_author_churn <- function(project_git_log, lag = 90) {
  author_name_email <- author_datetimetz <- lines_added <- lines_removed <- churn <- NULL # due to NSE notes in R CMD check

  # Add churn per commit per file row (and filter out "-" rows)
  dt <- metric_churn_per_commit_per_file(data.table::as.data.table(project_git_log))

  # Determine timezone
  tz_val <- attr(dt$author_datetimetz, "tzone")

  dt[, author_datetimetz := as.POSIXct(author_datetimetz, tz = tz_val)]

  # Order data
  # Order rows by author_name_email and author_datetimetz in ascending order
  data.table::setorder(dt, author_name_email, author_datetimetz)
  data.table::setkey(dt, author_name_email, author_datetimetz)

  # Rolling window
  result <- dt[, {
    window_start <- stringi::stri_datetime_add(
      author_datetimetz,
      days = -lag,
      tz = tz_val
    )

    agg <- dt[.SD,
      on = .(author_name_email,
             author_datetimetz >= window_start,
             author_datetimetz <= author_datetimetz),
      .(
        lines_added = sum(as.numeric(lines_added), na.rm = TRUE),
        lines_removed = sum(as.numeric(lines_removed), na.rm = TRUE),
        author_churn = sum(churn, na.rm = TRUE)
      ),
      by = .EACHI]

    .(
      author_name_email = author_name_email,
      author_datetimetz = author_datetimetz,
      lines_added = agg$lines_added,
      lines_removed = agg$lines_removed,
      author_churn = agg$author_churn
    )

  }, by = .(author_name_email, author_datetimetz)]

  return(result[])
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
