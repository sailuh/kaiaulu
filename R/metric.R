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
#' an author communicated via a message.
#' @param timestamp a data table column indicating the POSIXct timestamp of an author's message
#' @param author_login a data table column indicating the author of a message
#' @param lag the number of days since an author's last message
#' @export
#' @family metrics
#' @references Wouter Mulder (2025). Am I finished yet? A discovery of burnout and
#' ragequits within open-source projects. (Master thesis, Jheronimus Academy of Data Science).
engagement_communication <- function(timestamp, author_login, lag = 90) {
  # Extract timezone attribute from POSIXct timestamp vector
  # timestamp should be POSIXct; tz_val becomes a character string (e.g., "UTC")
  tz_val <- attr(timestamp, "tzone")

  # Create data table
  dt <- data.table::data.table(
    timestamp = as.POSIXct(timestamp, tz = tz_val),
    author_login = author_login
  )

  # Order data
  data.table::setorder(dt, author_login, timestamp)

  # For each author, implement a rolling window:
  # Create consecutive non-overlapping windows of lag days and count messages in each
  result <- dt[, {
    all_times <- timestamp
    
    if (length(all_times) == 0) {
      return(data.table::data.table(
        timestamp = as.POSIXct(character(), tz = tz_val),
        message_count = integer()
      ))
    }
    
    min_time <- min(all_times, na.rm = TRUE)
    max_time <- max(all_times, na.rm = TRUE)
    
    # Create window boundaries using a list of data.tables to preserve POSIXct class
    window_results <- list()
    current_start <- min_time
    
    while (current_start < max_time) {
      current_end <- stringi::stri_datetime_add(
        current_start,
        value = lag,
        units = "days",
        tz = tz_val
      )
      
      # Count messages in this window [current_start, current_end)
      # Only include complete windows
      if (current_end <= max_time) {
        count <- sum(all_times >= current_start & all_times < current_end, na.rm = TRUE)
        window_results[[length(window_results) + 1]] <- data.table::data.table(
          timestamp = current_end,
          message_count = as.integer(count)
        )
      }
      
      current_start <- current_end
    }
    
    if (length(window_results) > 0) {
      data.table::rbindlist(window_results)
    } else {
      data.table::data.table(
        timestamp = as.POSIXct(character(), tz = tz_val),
        message_count = integer()
      )
    }

  }, by = .(author_login)]

  return(result[])
}

#' Productivity Author Commits Metric
#'
#' @description Counts the unique number of commits per author in a rolling window based on
#' a project's git log.
#' @param project_git a parsed git log obtained from \code{\link{parse_gitlog}}
#' @param lag the number of days to look back for the rolling window (90 day default)
#' @return a three column data.table of the form author_name_email | author_datetimetz | author_total_commits.
#' For each row (each timestamp in author_datetimetz), author_total_commits is the number of unique 
#' commits by that author in the preceding number of `lag` days, ending at that timestamp.
#' @export
#' @family metrics
#' @seealso \code{\link{parse_gitlog}}
productivity_author_commits <- function(project_git, lag = 90) {
  author_name_email <- author_datetimetz <- commit_hash <- NULL # due to NSE notes in R CMD check

  # Extract timezone attribute from POSIXct author_datetimetz column
  # author_datetimetz should be POSIXct; tz_val becomes a character string (e.g., "UTC")
  tz_val <- attr(project_git$author_datetimetz, "tzone")

  # Coerce project_git to a data.table
  dt <- data.table::as.data.table(project_git)[, .(
    author_name_email,
    author_datetimetz = as.POSIXct(author_datetimetz, tz = tz_val),
    commit_hash
  )]

  # Order rows by author_name_email and author_datetimetz in ascending order
  data.table::setorder(dt, author_name_email, author_datetimetz)

  # Rolling window: create consecutive non-overlapping windows of lag days
  result <- dt[, {
    all_times <- author_datetimetz
    all_hashes <- commit_hash
    
    if (length(all_times) == 0) {
      return(data.table::data.table(
        author_datetimetz = as.POSIXct(character(), tz = tz_val),
        author_total_commits = integer()
      ))
    }
    
    min_time <- min(all_times, na.rm = TRUE)
    max_time <- max(all_times, na.rm = TRUE)
    
    # Create window boundaries using a list of data.tables to preserve POSIXct class
    window_results <- list()
    current_start <- min_time
    
    while (current_start < max_time) {
      current_end <- stringi::stri_datetime_add(
        current_start,
        value = lag,
        units = "days",
        tz = tz_val
      )
      
      # Get unique commits in this window [current_start, current_end)
      # Only include complete windows
      if (current_end <= max_time) {
        idx <- all_times >= current_start & all_times < current_end
        unique_commits <- data.table::uniqueN(all_hashes[idx])
        
        if (any(idx)) {
          window_results[[length(window_results) + 1]] <- data.table::data.table(
            author_datetimetz = current_end,
            author_total_commits = as.integer(unique_commits)
          )
        }
      }
      
      current_start <- current_end
    }
    
    if (length(window_results) > 0) {
      data.table::rbindlist(window_results)
    } else {
      data.table::data.table(
        author_datetimetz = as.POSIXct(character(), tz = tz_val),
        author_total_commits = integer()
      )
    }
  }, by = .(author_name_email)]

  return(result[])
}

#' Productivity Author Churn Metric
#'
#' @description Calculates the churn per author in a rolling window based on a project's git log.
#' @param project_git a parsed git log obtained from \code{\link{parse_gitlog}}
#' @param lag the number of days to look back for the rolling window (90 day default)
#' @return a five column data.table of the form author_name_email | author_datetimetz | lines_added | 
#' lines_removed | author_churn. 
#' At each timestamp, the lines_added, lines_removed, and author_churn are the totals across all rows 
#' for that author within the prior `lag` days, ending at that timestamp.
#' @export
#' @family metrics
#' @seealso \code{\link{parse_gitlog}} to obtain additions and deletions from gitlog
productivity_author_churn <- function(project_git, lag = 90) {
  author_name_email <- author_datetimetz <- lines_added <- lines_removed <- churn <- NULL # due to NSE notes in R CMD check

  # Add churn per commit per file row (and filter out "-" rows)
  dt <- metric_churn_per_commit_per_file(data.table::as.data.table(project_git))

  # Extract timezone attribute from POSIXct author_datetimetz column
  # author_datetimetz should be POSIXct; tz_val becomes a character string (e.g., "UTC")
  tz_val <- attr(dt$author_datetimetz, "tzone")

  dt[, author_datetimetz := as.POSIXct(author_datetimetz, tz = tz_val)]

  # Order rows by author_name_email and author_datetimetz in ascending order
  data.table::setorder(dt, author_name_email, author_datetimetz)

  # Rolling window: create consecutive non-overlapping windows of lag days
  result <- dt[, {
    all_times <- author_datetimetz
    all_added <- lines_added
    all_removed <- lines_removed
    all_churn <- churn
    
    if (length(all_times) == 0) {
      return(data.table::data.table(
        author_datetimetz = as.POSIXct(character(), tz = tz_val),
        lines_added = numeric(),
        lines_removed = numeric(),
        author_churn = numeric()
      ))
    }
    
    min_time <- min(all_times, na.rm = TRUE)
    max_time <- max(all_times, na.rm = TRUE)
    
    # Create window boundaries and sum churn metrics in each
    window_results <- list()
    current_start <- min_time
    
    while (current_start < max_time) {
      current_end <- stringi::stri_datetime_add(
        current_start,
        value = lag,
        units = "days",
        tz = tz_val
      )
      
      # Get churn metrics in this window [current_start, current_end)
      # Only include complete windows
      if (current_end <= max_time) {
        idx <- all_times >= current_start & all_times < current_end
        
        if (any(idx)) {
          window_results[[length(window_results) + 1]] <- data.table::data.table(
            author_datetimetz = current_end,
            lines_added = sum(as.numeric(all_added[idx]), na.rm = TRUE),
            lines_removed = sum(as.numeric(all_removed[idx]), na.rm = TRUE),
            author_churn = sum(all_churn[idx], na.rm = TRUE)
          )
        }
      }
      
      current_start <- current_end
    }
    
    if (length(window_results) > 0) {
      data.table::rbindlist(window_results)
    } else {
      data.table::data.table(
        author_datetimetz = as.POSIXct(character(), tz = tz_val),
        lines_added = numeric(),
        lines_removed = numeric(),
        author_churn = numeric()
      )
    }
  }, by = .(author_name_email)]

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
