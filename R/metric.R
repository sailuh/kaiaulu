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

#' Engagement Communication Metric.
#'
#' @description Uses a rolling window of 90 days to check, for each 90-day window, how many times
#' an author communicated via a message.
#' @param datetimetz A data table column indicating the timestamp of an author's message
#' @param user_name_email A data table column indicating the author of a message
#' @param quit_lag The number of days since a developer's last message
#' @export
#' @references Wouter Mulder (2025). Am I finished yet? A discovery of burnout and
#' ragequits within open-source projects. (Master thesis, Jheronimus Academy of Data Science).
engagement_communication <- function(datetimetz, user_name_email, quit_lag = 90) {

  # Determine timezone
  tz_val <- attr(datetimetz, "tzone")
  if (is.null(tz_val)) {
    tz_val <- Sys.timezone()
  }

  # Create data table
  dt <- data.table::data.table(
    datetimetz = as.POSIXct(datetimetz, tz = tz_val),
    user_name_email = user_name_email
  )

  # Order data
  data.table::setorder(dt, user_name_email, datetimetz)

  # Rolling window message count
  result <- dt[, {
    # Compute start of each rolling window
    window_start <- stringi::stri_datetime_add(
      datetimetz,
      days = -quit_lag,
      tz = tz_val
    )

    # Find index of the last message before window starts
    # +1L is used later to get the first message inside the window
    start_idx <- findInterval(window_start, datetimetz)

    # Count all messages in the 90-day rolling window ending at the most recent message
    message_count <- vapply(seq_len(.N), function(i) {
      idx <- seq.int(start_idx[i] + 1L, i)
      length(idx)
    }, integer(1))

    # Return a data table with the below columns
    .(datetimetz = datetimetz,
      user_name_email = user_name_email,
      message_count = message_count)

  }, by = .(user_name_email)]

  return(result[])
}

#' Engagement Sentiment Metric.
#'
#' @description Apply an aggregate function to the sentiment (polarity) for each 
#' 90 day window (quit_lag) from the author (user_name_email)
#' @param datetimetz A data table column indicating the timestamp of an author's message
#' @param user_name_email A data table column indicating the author of a message
#' @param quit_lag The number of days since a developer's last message
#' @param polarity A data table column indicating the sentiment of a message
#' @param aggregate_func The aggregate function to apply to the polarity values in the windows
#' @export
#' @references Wouter Mulder (2025). Am I finished yet? A discovery of burnout and
#' ragequits within open-source projects. (Master thesis, Jheronimus Academy of Data Science).
engagement_sentiment <- function(datetimetz, user_name_email, polarity, quit_lag = 90, aggregate_func) {

  # Convert polarity strings to numeric representation: positive = 1, negative = -1, neutral = 0
  numeric_polarity <- data.table::fifelse(polarity == "positive", 1,
                                          data.table::fifelse(polarity == "negative", -1, 0))

  # Determine timezone
  tz_val <- attr(datetimetz, "tzone")
  if (is.null(tz_val)) {
    tz_val <- Sys.timezone()
  }

  # Create data table
  dt <- data.table::data.table(
    datetimetz = as.POSIXct(datetimetz, tz = tz_val),
    user_name_email = user_name_email,
    polarity = numeric_polarity
  )

  # Order data in non-decreasing order of datetimetz for each user
  data.table::setorder(dt, user_name_email, datetimetz)

  # Rolling window and aggregation
  result <- dt[, {
    # Compute start of each rolling window
    window_start <- stringi::stri_datetime_add(
      datetimetz,
      days = -quit_lag,
      tz = tz_val
    )

    # Find index of the last message before window starts
    # +1L is used later to get the first message inside the window
    start_idx <- findInterval(window_start, datetimetz)

    # Compute rolling aggregate of polarity values for each message
    aggregate_val <- vapply(seq_len(.N), function(i) {
      idx <- seq.int(start_idx[i] + 1L, i)
      aggregate_func(polarity[idx], na.rm = TRUE)
    }, numeric(1))

    # Return a data table with the below columns
    .(datetimetz = datetimetz,
      user_name_email = user_name_email,
      aggregate_polarity = aggregate_val)

  }, by = .(user_name_email)]

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
