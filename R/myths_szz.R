# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# Kaiaulu helpers — szz
# Carried in from icse27theories/lifts/functions.R.
# Style follows kaiaulu verb_noun + data.table.

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
