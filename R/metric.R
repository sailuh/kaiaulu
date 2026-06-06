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

############## SD-lift helpers (PR #409) ##############
# Helpers folded in from R/myths_*.R per Carlos's review.
# Style consistent with kaiaulu: snake_case, data.table,
# explicit returns, MPL license inherited from this file.

#' Parse PyDriller B-SZZ Output
#'
#' Reads a CSV produced by an external PyDriller SZZ pass and returns
#' a data.table joinable to a gitlog on commit_hash. Each row is one
#' (fixing_commit -> introducing_commit, file) triple.
#'
#' @param szz_csv_path Path to the SZZ CSV. Expected columns:
#'   fixing_commit_hash, introducing_commit_hash, file_path, jira_keys,
#'   fixing_date, introducing_date.
#' @return data.table with the same columns, dates parsed to POSIXct.
#' @export
parse_szz_bugfixes <- function(szz_csv_path) {
  dt <- data.table::fread(szz_csv_path)
  dt[, fixing_date       := as.POSIXct(fixing_date,       tz = "UTC")]
  dt[, introducing_date  := as.POSIXct(introducing_date,  tz = "UTC")]
  return(dt)
}

#' Count Bug Introductions per Late-Hire Window
#'
#' For each late-hire event, counts bug-introducing commits in the
#' pre-window and post-window. Uses the SZZ pairs table: a commit is
#' "bug-introducing" if it appears as introducing_commit_hash for any
#' downstream fix. Returns one row per late hire.
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param late_hires Output of \code{detect_late_hires()}.
#' @param window_days Numeric. Pre/post window size in days.
#' @return data.table with: id, hire_at, pre_intros, post_intros,
#'   inj_rate_pre, inj_rate_post.
#' @export
compute_injection_changes <- function(szz, late_hires,
                                      window_days = 90) {
  id_col <- if ("identity_id" %in% names(late_hires)) "identity_id"
            else "author_name_email"
  intros <- unique(szz[, .(introducing_commit_hash, introducing_date)])
  results <- lapply(seq_len(nrow(late_hires)), function(i) {
    hire_at   <- late_hires[i, first_commit_at]
    win_start <- hire_at - as.difftime(window_days, units = "days")
    win_end   <- hire_at + as.difftime(window_days, units = "days")
    pre_n  <- intros[introducing_date >= win_start &
                     introducing_date <  hire_at, .N]
    post_n <- intros[introducing_date >  hire_at &
                     introducing_date <= win_end, .N]
    data.table(
      id            = late_hires[i, get(id_col)],
      hire_at       = hire_at,
      pre_intros    = pre_n,
      post_intros   = post_n,
      inj_rate_pre  = pre_n  / window_days,
      inj_rate_post = post_n / window_days
    )
  })
  return(rbindlist(results))
}

#' Estimate Leak Rate from SZZ Pairs
#'
#' Leak rate = fraction of introductions that are still un-fixed
#' beyond a given latency threshold. Higher = more bugs leak past the
#' immediate review window into the field.
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param latency_days Numeric. Bugs taking longer than this to fix
#'   count as "leaked." Default 30.
#' @return Numeric in [0,1].
#' @export
estimate_leak_rate <- function(szz, latency_days = 30) {
  latencies <- as.numeric(difftime(szz$fixing_date,
                                   szz$introducing_date,
                                   units = "days"))
  latencies <- latencies[is.finite(latencies) & latencies >= 0]
  if (length(latencies) == 0) return(NA_real_)
  return(mean(latencies > latency_days))
}

#' Compute Failure Rate per Rolling Window
#'
#' Calibrates the rework model's \code{failrate} parameter as the
#' fraction of commits in each window that introduce a bug (per SZZ).
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param project_git A gitlog data.table.
#' @param window_days Numeric. Window size in days.
#' @return data.table with window_start, n_commits, n_intro_commits, failrate.
#' @export
compute_failrate_per_window <- function(szz, project_git, window_days = 90) {
  commits <- unique(project_git[, .(commit_hash, author_datetimetz)])
  intro_hashes <- unique(szz$introducing_commit_hash)
  commits[, has_intro := commit_hash %in% intro_hashes]
  commits <- commits[order(author_datetimetz)]

  start <- min(commits$author_datetimetz)
  end   <- max(commits$author_datetimetz)
  step  <- as.difftime(window_days, units = "days")
  windows <- seq(start, end, by = step)
  out <- lapply(windows, function(ws) {
    we <- ws + step
    chunk <- commits[author_datetimetz >= ws & author_datetimetz < we]
    if (nrow(chunk) == 0) return(NULL)
    data.table(
      window_start    = ws,
      n_commits       = nrow(chunk),
      n_intro_commits = sum(chunk$has_intro),
      failrate        = sum(chunk$has_intro) / nrow(chunk)
    )
  })
  return(rbindlist(out[!sapply(out, is.null)]))
}

#' Compute Per-Release-Phase Defect Flow
#'
#' For each release-tag phase, count:
#' - injected: bug-introducing commits whose date falls in this phase
#' - caught:   bug-fixing commits in same phase as their introducer
#' - leaked:   introducers in this phase whose fix is in a later phase
#'             or absent
#'
#' \code{tst_proxy} = caught / max(injected, 1) — calibrates the defmap
#' model's \code{tst} testing-intensity parameter.
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param project_git A gitlog data.table.
#' @param tag_dates data.table with columns: tag, date (POSIXct).
#' @return data.table per phase with injected, caught, leaked, tst_proxy.
#' @export
compute_per_phase_defects <- function(szz, project_git, tag_dates) {
  tag_dates <- tag_dates[order(date)]
  phase_for <- function(ts) {
    idx <- findInterval(ts, tag_dates$date)
    idx[idx == 0] <- 1L
    tag_dates$tag[idx]
  }
  szz[, phase_intro := phase_for(introducing_date)]
  szz[, phase_fix   := phase_for(fixing_date)]

  intro_per_phase <- unique(szz[, .(introducing_commit_hash, phase_intro,
                                    phase_fix)])
  out <- intro_per_phase[, .(
    injected = .N,
    caught   = sum(phase_intro == phase_fix, na.rm = TRUE),
    leaked   = sum(phase_intro != phase_fix, na.rm = TRUE)
  ), by = phase_intro]
  setnames(out, "phase_intro", "phase")
  out[, tst_proxy := caught / pmax(injected, 1)]
  return(out)
}

#' Compute DORA-style Metrics from Tags + SZZ
#'
#' batch_size  : mean commits between consecutive tags
#' cfr         : (bug-fix commits) / (total commits) over the full history
#' arrival_rate: commits per day (mean)
#' rec_rate    : 1 / median(fix_date - intro_date) in days
#'
#' @param szz Output of \code{parse_szz_bugfixes()}.
#' @param project_git A gitlog data.table.
#' @param tag_dates data.table with columns: tag, date (POSIXct).
#' @return list with named numeric values.
#' @export
compute_dora_metrics <- function(szz, project_git, tag_dates) {
  commits <- unique(project_git[, .(commit_hash, author_datetimetz)])
  setorder(commits, author_datetimetz)
  total_commits <- nrow(commits)
  span_days <- as.numeric(difftime(max(commits$author_datetimetz),
                                   min(commits$author_datetimetz),
                                   units = "days"))

  arrival_rate <- total_commits / max(span_days, 1)

  # batch_size = total commits / (tags - 1)
  n_tags <- nrow(tag_dates)
  batch_size <- if (n_tags >= 2) total_commits / (n_tags - 1) else NA_real_

  n_fixes <- uniqueN(szz$fixing_commit_hash)
  cfr <- n_fixes / total_commits

  latencies <- as.numeric(difftime(szz$fixing_date, szz$introducing_date,
                                   units = "days"))
  latencies <- latencies[is.finite(latencies) & latencies >= 0]
  median_mttr <- if (length(latencies)) median(latencies) else NA_real_
  rec_rate <- if (is.finite(median_mttr) && median_mttr > 0) 1 / median_mttr
              else NA_real_

  return(list(
    batch_size   = batch_size,
    cfr          = cfr,
    arrival_rate = arrival_rate,
    rec_rate     = rec_rate,
    n_tags       = n_tags,
    span_days    = span_days
  ))
}

#' Compute Per-File Bug Frequency
#'
#' Joins gitlog (with commit_message_id) to JIRA bugs and counts
#' bug-touching commits per file.
#'
#' @param project_git A gitlog data.table that has been processed
#'   through parse_commit_message_id.
#' @param jira_bugs A data.table of bug issues from
#'   parse_jira()$issues filtered to issue_type == "Bug".
#' @param issue_id_regex Used to extract the issue key from the
#'   commit_message_id column.
#' @return data.table with file_pathname, bug_count.
#' @export
compute_file_bug_frequency <- function(project_git, jira_bugs,
                                       issue_id_regex) {
  bug_keys <- jira_bugs$issue_key
  bug_commits <- project_git[commit_message_id %in% bug_keys]
  return(bug_commits[, .(bug_count = uniqueN(commit_hash)), by = file_pathname])
}

#' Compute Pay Rate from Refactoring Activity
#'
#' Pay rate = fraction of commits in each rolling window that contain
#' at least one RefactoringMiner-detected refactoring. Calibrates the
#' \code{pay_rate} parameter of the debt SD model.
#'
#' @param project_git A gitlog data.table.
#' @param refactorings Output of \code{flatten_refactoring_json()}.
#' @param window_days Numeric. Rolling window size in days. Default 90.
#' @return data.table with: window_start, window_end, n_commits,
#'   n_refactor_commits, pay_rate.
#' @export
compute_pay_rate <- function(project_git, refactorings,
                             window_days = 90) {
  commits <- unique(project_git[, .(commit_hash, author_datetimetz)])
  refactor_hashes <- unique(refactorings$commit_hash)
  commits[, has_refactor := commit_hash %in% refactor_hashes]
  commits <- commits[order(author_datetimetz)]

  start <- min(commits$author_datetimetz)
  end   <- max(commits$author_datetimetz)
  step  <- as.difftime(window_days, units = "days")

  windows <- seq(start, end, by = step)
  out <- lapply(windows, function(ws) {
    we <- ws + step
    chunk <- commits[author_datetimetz >= ws & author_datetimetz < we]
    if (nrow(chunk) == 0) return(NULL)
    data.table(
      window_start       = ws,
      window_end         = we,
      n_commits          = nrow(chunk),
      n_refactor_commits = sum(chunk$has_refactor),
      pay_rate           = sum(chunk$has_refactor) / nrow(chunk)
    )
  })
  return(rbindlist(out[!sapply(out, is.null)]))
}

#' Crude Born-Rate Proxy from Gitlog Churn
#'
#' Approximates the debt model's \code{born_rate} as the share of
#' commits in each window that touch many files (a proxy for "ship
#' fast → introduce debt"). Threshold defaults to 5 files.
#'
#' @param project_git A gitlog data.table.
#' @param window_days Numeric. Window size in days.
#' @param big_commit_files Numeric. Min files for "big commit." Default 5.
#' @return data.table with: window_start, n_commits, n_big_commits,
#'   born_rate.
#' @export
compute_born_rate_proxy <- function(project_git, window_days = 90,
                                    big_commit_files = 5) {
  cs <- project_git[, .(
    files_touched = uniqueN(file_pathname),
    when          = min(author_datetimetz)
  ), by = commit_hash]
  cs[, big := files_touched >= big_commit_files]

  start <- min(cs$when); end <- max(cs$when)
  step  <- as.difftime(window_days, units = "days")
  windows <- seq(start, end, by = step)
  out <- lapply(windows, function(ws) {
    we <- ws + step
    chunk <- cs[when >= ws & when < we]
    if (nrow(chunk) == 0) return(NULL)
    data.table(
      window_start  = ws,
      n_commits     = nrow(chunk),
      n_big_commits = sum(chunk$big),
      born_rate     = sum(chunk$big) / nrow(chunk)
    )
  })
  return(rbindlist(out[!sapply(out, is.null)]))
}

#' Compute Per-File Churn over a Recent Window
#'
#' @param project_git A gitlog data.table.
#' @param window_days Numeric. Window size for "recent."
#' @return data.table with file_pathname, churn_score in [0,1].
#' @export
compute_file_churn <- function(project_git, window_days = 180) {
  cutoff <- max(project_git$author_datetimetz) -
           as.difftime(window_days, units = "days")
  recent <- project_git[author_datetimetz >= cutoff]
  recent_commits <- recent[, .(recent_n = uniqueN(commit_hash)),
                           by = file_pathname]
  total_commits  <- project_git[, .(total_n = uniqueN(commit_hash)),
                                by = file_pathname]
  merged_table <- merge(recent_commits, total_commits, by = "file_pathname",
             all.y = TRUE)
  merged_table[is.na(recent_n), recent_n := 0]
  merged_table[, churn_score := recent_n / total_n]
  return(merged_table[, .(file_pathname, churn_score)])
}

#' Assign Each File to a Stock (Patterned, Legacy, or Drift)
#'
#' Implements the archpat model's partition. A file is Patterned if
#' it participates in any GoF pattern instance; Legacy if it has
#' high accumulated bug count and is not Patterned; Drift if it has
#' recent high churn and is not Patterned or Legacy.
#'
#' @param patterned_files Character vector of file paths in GoF
#'   patterns.
#' @param file_bug_freq Output of compute_file_bug_frequency.
#' @param file_churn Output of compute_file_churn.
#' @param legacy_bug_threshold Numeric. Min bug count for Legacy.
#' @param drift_churn_threshold Numeric in [0,1]. Min churn for Drift.
#' @return data.table with file_pathname, stock in
#'   {"Patterned","Legacy","Drift","Other"}.
#' @export
assign_file_partition <- function(patterned_files,
                                  file_bug_freq,
                                  file_churn,
                                  legacy_bug_threshold  = 5,
                                  drift_churn_threshold = 0.7) {
  all_files <- union(file_bug_freq$file_pathname,
                     file_churn$file_pathname)
  out <- data.table(file_pathname = all_files)
  out <- merge(out, file_bug_freq, by = "file_pathname", all.x = TRUE)
  out <- merge(out, file_churn,    by = "file_pathname", all.x = TRUE)
  out[is.na(bug_count), bug_count := 0]
  out[is.na(churn_score), churn_score := 0]
  out[, stock := fcase(
    file_pathname %in% patterned_files,                       "Patterned",
    bug_count    >= legacy_bug_threshold,                     "Legacy",
    churn_score  >= drift_churn_threshold,                    "Drift",
    default = "Other"
  )]
  out
}

