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

#' Engagement Communication Metric
#'
#' @description Uses a rolling window of 90 days to check, for each 90-day window, how many times
#' an author communicated via a message
#' @param datetimetz a data table column indicating the timestamp of an author's message
#' @param user_name_email a data table column indicating the author of a message
#' @param quit_lag the number of days since a developer's last message
#' @export
#' @references Wouter Mulder (2025). Am I finished yet? A discovery of burnout and
#' ragequits within open-source projects. (Master thesis, Jheronimus Academy of Data Science).
engagement_communication <- function(datetimetz, user_name_email, quit_lag = 90) {
  
  # Determine timezone
  tz_val <- attr(datetimetz, "tzone")

  # Create data table
  dt <- data.table::data.table(
    datetimetz = as.POSIXct(datetimetz, tz = tz_val),
    user_name_email = user_name_email
  )

  # Order data
  data.table::setorder(dt, user_name_email, datetimetz)

  # For each message, count how many messages the same author sent
  # in the rolling window i.e. the prior 90 days up to and including the message timestamp
  result <- dt[, {
    all_times <- datetimetz
    unique_times <- unique(datetimetz)

    window_start <- stringi::stri_datetime_add(
      unique_times,
      value = -quit_lag,
      units = "days",
      tz = tz_val
    )

    message_count <- sapply(seq_along(unique_times), function(i) {
      sum(all_times >= window_start[i] & all_times <= unique_times[i], na.rm = TRUE)
    })

    .(datetimetz = unique_times,
      message_count = message_count)

  }, by = list(user_name_email)]

  return(result[])
}

#' Productivity Author Commits Metric
#'
#' Counts the unique number of commits per author in a rolling window based on
#' the git log.
#'
#' @param project_git a parsed git log obtained from \code{\link{parse_gitlog}}
#' @param lag the number of days to look back for the rolling window (90 day default)
#' @return a three column data.table of the form author_name_email | author_datetimetz | author_total_commits.
#' For each row (each timestamp in author_datetimetz), author_total_commits is the number of unique 
#' commits by that author in the preceding 90 days ending at that timestamp.
#' @export
#' @family metrics
#' @seealso \code{\link{parse_gitlog}}
productivity_author_commits <- function(project_git, lag = 90) {
  author_name_email <- author_datetimetz <- commit_hash <- NULL

  # Determine timezone from the author_datetimetz column in project_git
  tz_val <- attr(project_git$author_datetimetz, "tzone")

  # Coerce project_git to a data.table
  dt <- data.table::as.data.table(project_git)[, .(
    author_name_email,
    author_datetimetz = as.POSIXct(author_datetimetz, tz = tz_val),
    commit_hash
  )]

  # Order rows by author_name_email and author_datetimetz in ascending order
  data.table::setorder(dt, author_name_email, author_datetimetz)

  # Rolling window
  result <- dt[, {
    all_times   <- author_datetimetz
    unique_times <- unique(all_times)
    
    window_start <- stringi::stri_datetime_add(
      unique_times,
      value = -lag,
      units = "days",
      tz = tz_val
    )

    author_total_commits <- sapply(seq_along(unique_times), function(i) {
      idx <- all_times >= window_start[i] & all_times <= unique_times[i]
      data.table::uniqueN(commit_hash[idx])
    })

    .(
      author_datetimetz = unique_times,
      author_total_commits = as.integer(author_total_commits)
    )
  }, by = .(author_name_email)]

  return(result[])
}

#' Productivity Author Churn Metric
#'
#' Calculates the churn per author in a rolling window based on the git log.
#'
#' @param project_git a parsed git log obtained from \code{\link{parse_gitlog}}
#' @param lag the number of days to look back for the rolling window (90 day default)
#' @return a five column data.table of the form author_name_email | author_datetimetz | lines_added | 
#' lines_removed | author_churn. 
#' At each timestamp, the lines_added, lines_removed, and author_churn are the totals across all rows 
#' for that author within the prior 90 days ending at that timestamp.
#' @export
#' @family metrics
#' @seealso \code{\link{parse_gitlog}} to obtain additions and deletions from gitlog
productivity_author_churn <- function(project_git, lag = 90) {
  author_name_email <- author_datetimetz <- lines_added <- lines_removed <- churn <- NULL # due to NSE notes in R CMD check

  # Add churn per commit per file row (and filter out "-" rows)
  dt <- metric_churn_per_commit_per_file(data.table::as.data.table(project_git))

  # Determine timezone
  tz_val <- attr(dt$author_datetimetz, "tzone")

  dt[, author_datetimetz := as.POSIXct(author_datetimetz, tz = tz_val)]

  # Order rows by author_name_email and author_datetimetz in ascending order
  data.table::setorder(dt, author_name_email, author_datetimetz)

  # Rolling window
  result <- dt[, {
    all_times    <- author_datetimetz
    unique_times <- unique(all_times)

    window_start <- stringi::stri_datetime_add(
      unique_times,
      value = -lag,
      units = "days",
      tz = tz_val
    )

    data.table::rbindlist(lapply(seq_along(unique_times), function(i) {
      idx <- all_times >= window_start[i] & all_times <= unique_times[i]
      data.table::data.table(
        author_datetimetz = unique_times[i],
        lines_added = sum(as.numeric(lines_added[idx]), na.rm = TRUE),
        lines_removed = sum(as.numeric(lines_removed[idx]), na.rm = TRUE),
        author_churn = sum(churn[idx], na.rm = TRUE)
      )
    }))
  }, by = .(author_name_email)]

  return(result[])
}

#' Jira Author Communication Count Metric
#'
#' Calculates the number of messages/comments an author has sent in Jira within
#' a 90-day rolling window.
#'
#' @param comments_dt a parsed jira comments table obtained from \code{\link{parse_jira}}
#' @param comment_created_col the column name in `comments_dt` which contains the comment creation time
#' @param comment_author_col the column name in `comments_dt` which contains the comment author name
#' @param lag the number of days to look back for the rolling window (default is 90)
#' @return a three column data.table of the form datetimetz | comment_author_name | comment_count
#' @export
#' @family metrics
#' @seealso \code{\link{parse_jira}} to obtain the comments table
jira_author_communication_count <- function(comments_dt, 
                                               comment_created_col = "comment_created_datetimetz", 
                                               comment_author_col = "comment_author_name", 
                                               lag = 90) {

raw_time   <- comments_dt[[comment_created_col]]
raw_author <- comments_dt[[comment_author_col]]

tz_val <- "UTC"

dt <- data.table::data.table(
    datetimetz = as.POSIXct(raw_time, format = "%Y-%m-%dT%H:%M:%OS%z", tz = tz_val),
    comment_author_name = raw_author
  )

  data.table::setorder(dt, comment_author_name, datetimetz)

  result <- dt[, {
    all_times    <- datetimetz
    unique_times <- unique(all_times)

    window_start <- stringi::stri_datetime_add(
      unique_times,
      value = -lag,
      units = "days",
      tz = tz_val
    )

    comment_count <- sapply(seq_along(unique_times), function(i) {
      sum(all_times >= window_start[i] & all_times <= unique_times[i], na.rm = TRUE)
    })

    .(
      datetimetz = unique_times,
      comment_count = as.integer(comment_count)
    )
  }, by = .(author = comment_author_name)]

  data.table::setnames(result, "author", "comment_author_name")

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
