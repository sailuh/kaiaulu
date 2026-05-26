# Brooks-Style Late-Hire Velocity Analysis
#
# Detect late-hire events from a project git log and compute pre/post veteran velocity per hire.
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


#' Detect Late-Hire Events from Git Log
#'
#' A late hire is a developer whose first commit is at least
#' \code{min_project_age_days} after the project's first commit.
#' Returns a data.table with one row per late hire.
#'
#' @param project_git A gitlog data.table (output of parse_gitlog +
#'   optional identity_match). Must contain identity_id (preferred)
#'   or author_name_email and author_datetimetz columns.
#' @param min_project_age_days Numeric. Minimum days after project
#'   start for a hire to count as "late." Default 365.
#' @return data.table with columns: identity_id (or
#'   author_name_email), first_commit_at, days_after_project_start.
#' @export
detect_late_hires <- function(project_git, min_project_age_days = 365) {
  id_col <- if ("identity_id" %in% names(project_git)) "identity_id"
            else "author_name_email"

  first_commits <- project_git[, .(
    first_commit_at = min(author_datetimetz)
  ), by = id_col]

  project_start <- min(project_git$author_datetimetz)
  first_commits[, days_after_project_start :=
    as.numeric(difftime(first_commit_at, project_start, units = "days"))]

  first_commits[days_after_project_start >= min_project_age_days]
}

#' Compute Veteran Velocity Before and After Each Late-Hire Event
#'
#' For each late hire, compute the commit rate of veterans (devs who
#' joined before the hire) in the windows before and after. Returns
#' one row per late hire.
#'
#' @param project_git A gitlog data.table.
#' @param late_hires Output of \code{detect_late_hires()}.
#' @param window_days Numeric. Size of the pre/post window in days.
#' @return data.table with columns: identity_id (or
#'   author_name_email), pre_velocity, post_velocity, brooks_tax.
#' @export
compute_velocity_changes <- function(project_git, late_hires,
                                     window_days = 90) {
  id_col <- if ("identity_id" %in% names(project_git)) "identity_id"
            else "author_name_email"

  results <- lapply(seq_len(nrow(late_hires)), function(i) {
    hire_id   <- late_hires[i, get(id_col)]
    hire_at   <- late_hires[i, first_commit_at]
    win_start <- hire_at - as.difftime(window_days, units = "days")
    win_end   <- hire_at + as.difftime(window_days, units = "days")

    # Veterans = devs who joined strictly before hire_at
    veterans <- project_git[author_datetimetz < hire_at,
                            unique(get(id_col))]

    pre_commits <- project_git[
      get(id_col) %in% veterans &
      author_datetimetz >= win_start &
      author_datetimetz <  hire_at,
      uniqueN(commit_hash)
    ]
    post_commits <- project_git[
      get(id_col) %in% veterans &
      author_datetimetz >  hire_at &
      author_datetimetz <= win_end,
      uniqueN(commit_hash)
    ]

    data.table(
      id            = hire_id,
      hire_at       = hire_at,
      pre_velocity  = pre_commits  / window_days,
      post_velocity = post_commits / window_days
    )
  })
  rbindlist(results)
}
