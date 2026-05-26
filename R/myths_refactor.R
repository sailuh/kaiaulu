# Refactoring + Release-Snapshot Helpers
#
# Drive RefactoringMiner, parse its JSON output, compute debt pay-rate and born-rate proxies, traverse release tags, and stage snapshots. Supports debt, archpat, and any release-anchored lift.
#
# Carried into kaiaulu from icse27theories/lifts/functions.R.
# Style follows kaiaulu's verb_noun snake_case + data.table.

require(data.table)
require(stringi)
require(magrittr)

# Null-coalescing helper used by several functions below. Local to each
# myths_*.R file rather than a global import so each module is
# self-contained when sourced.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


#' Build a Tag-Date Table for a Git Repo
#'
#' Wraps git system calls to enumerate tags in v:refname order and
#' fetch each tag's commit date.
#'
#' @param git_repo_path Path to .git or worktree.
#' @return data.table with tag, date (POSIXct).
#' @export
get_tag_dates <- function(git_repo_path) {
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  tags <- system2("git", c("-C", repo, "tag", "--sort=v:refname"),
                  stdout = TRUE)
  if (length(tags) == 0) return(data.table(tag = character(),
                                           date = as.POSIXct(character())))
  unix_ts <- vapply(tags, function(tg) {
    out <- system2("git",
                   c("-C", repo, "log", "-1", "--format=%ct", tg),
                   stdout = TRUE)
    if (length(out) == 0) NA_character_ else out[1]
  }, character(1))
  data.table(tag  = tags,
             date = as.POSIXct(as.numeric(unix_ts),
                               origin = "1970-01-01", tz = "UTC"))[
                                 order(date)]
}

#' Get Release Tags from a Git Repository
#'
#' Wraps system call to \code{git tag}. Returns tags in lexicographic
#' order (which usually approximates release order for SemVer projects).
#'
#' @param git_repo_path Path to the .git directory or working tree.
#' @return Character vector of tag names.
#' @export
get_release_tags <- function(git_repo_path) {
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  out  <- system2("git",
                  args = c("-C", repo, "tag", "--sort=v:refname"),
                  stdout = TRUE)
  out
}

#' Check Out a Git Snapshot to a Temporary Directory
#'
#' Creates a worktree at the given tag/commit. Returns the worktree
#' path. Caller is responsible for cleanup via
#' \code{system2("git", c("-C", repo, "worktree", "remove", path))}.
#'
#' @param git_repo_path Path to the .git directory or working tree.
#' @param ref Tag name, branch name, or commit hash.
#' @return Character path to the snapshot directory.
#' @export
checkout_snapshot <- function(git_repo_path, ref) {
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  snap <- tempfile(pattern = paste0("snap_", gsub("/", "_", ref), "_"))
  system2("git",
          args = c("-C", repo, "worktree", "add", "--detach", snap, ref),
          stdout = FALSE, stderr = FALSE)
  snap
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
  rbindlist(out[!sapply(out, is.null)])
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
  rbindlist(out[!sapply(out, is.null)])
}

#' Run RefactoringMiner on a Git Repository
#'
#' System call to the RefactoringMiner CLI. Returns the path to the
#' resulting JSON file. The JSON is the standard RefactoringMiner
#' \code{-all} output (every refactoring across the project history).
#'
#' @param refminer_jar Path to RefactoringMiner-*.jar.
#' @param git_repo_path Path to the .git directory or working tree.
#' @param out_path Optional output file path. Default: tempfile.
#' @return Character path to the resulting JSON file.
#' @export
run_refactoring_miner <- function(refminer_jar, git_repo_path,
                                  out_path = NULL) {
  repo <- gsub("/\\.git/?$", "", git_repo_path)
  if (is.null(out_path)) {
    out_path <- tempfile(fileext = ".json")
  }
  system2("java",
          args = c("-jar", refminer_jar,
                   "-a", repo,        # -a = all commits
                   "-json", out_path),
          stdout = FALSE, stderr = FALSE)
  out_path
}

#' Flatten RefactoringMiner JSON to a data.table
#'
#' kaiaulu's parse_java_code_refactoring_json returns a nested list.
#' This helper flattens to one row per refactoring event.
#'
#' @param refminer_json_path Path to a RefactoringMiner JSON output
#'   file (from \code{run_refactoring_miner}).
#' @return data.table with: commit_hash, refactoring_type,
#'   refactoring_description, left_locations, right_locations.
#' @export
flatten_refactoring_json <- function(refminer_json_path) {
  raw <- jsonlite::fromJSON(refminer_json_path, simplifyVector = FALSE)
  rows <- lapply(raw$commits, function(c) {
    if (length(c$refactorings) == 0) return(NULL)
    rbindlist(lapply(c$refactorings, function(r) {
      data.table(
        commit_hash             = c$sha1,
        refactoring_type        = r$type,
        refactoring_description = r$description %||% NA_character_,
        left_locations  = paste(sapply(r$leftSideLocations,  `[[`,
                                       "filePath"), collapse = ";"),
        right_locations = paste(sapply(r$rightSideLocations, `[[`,
                                       "filePath"), collapse = ";")
      )
    }))
  })
  rbindlist(rows[!sapply(rows, is.null)])
}

#' Merge Commit Dates Back into a Refactoring Table
#'
#' RefactoringMiner output has commit_hash but no date. Join from
#' gitlog to recover the commit_datetimetz.
#'
#' @export
merge_commit_dates <- function(refactorings, project_git) {
  hashes <- unique(project_git[, .(commit_hash, author_datetimetz)])
  m <- merge(refactorings, hashes, by = "commit_hash", all.x = TRUE)
  m$author_datetimetz
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
  m <- merge(recent_commits, total_commits, by = "file_pathname",
             all.y = TRUE)
  m[is.na(recent_n), recent_n := 0]
  m[, churn_score := recent_n / total_n]
  m[, .(file_pathname, churn_score)]
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

#' @export
compute_smell_appearance_rate <- function(gof_per_tag) NA_real_
#' @export
compute_legacy_growth_rate    <- function(project_git, patterned_files) NA_real_
#' @export
compute_born_pat_rate         <- function(refactorings, gof_per_tag) NA_real_
#' @export
compute_born_leg_rate         <- function(project_git, patterned_files) NA_real_

# ---- Utility -------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Merge Commit Dates Back into a Refactoring Table
#'
#' RefactoringMiner output has commit_hash but no date. Join from
#' gitlog to recover the commit_datetimetz.
#'
#' @export
merge_commit_dates <- function(refactorings, project_git) {
  hashes <- unique(project_git[, .(commit_hash, author_datetimetz)])
  m <- merge(refactorings, hashes, by = "commit_hash", all.x = TRUE)
  m$author_datetimetz
}
