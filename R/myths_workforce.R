# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# Kaiaulu helpers — workforce
# Carried in from icse27theories/lifts/functions.R.
# Style follows kaiaulu verb_noun + data.table.

#' Compute Workforce Cohort Distribution
#'
#' For each developer, compute tenure (last_commit - first_commit) and
#' assign to Jr / Tr / Sr buckets.
#'
#' @param project_git A gitlog data.table with identity_id.
#' @param jr_max_days Tenure < this = Jr. Default 365.
#' @param sr_min_days Tenure >= this = Sr. Default 1095 (3 years).
#' @return data.table with identity_id, tenure_days, cohort.
#' @export
compute_cohorts <- function(project_git, jr_max_days = 365,
                            sr_min_days = 1095) {
  per_dev <- project_git[, .(
    first_commit = min(author_datetimetz),
    last_commit  = max(author_datetimetz),
    n_commits    = uniqueN(commit_hash)
  ), by = identity_id]
  per_dev[, tenure_days := as.numeric(difftime(last_commit, first_commit,
                                               units = "days"))]
  per_dev[, cohort := fcase(
    tenure_days <  jr_max_days,  "Jr",
    tenure_days >= sr_min_days,  "Sr",
    default                   = "Tr"
  )]
  return(per_dev)
}

#' Estimate Transition Rates from Cohort Trajectories
#'
#' Buckets the project history into time slices and counts devs who
#' moved Jr→Tr (train) and Tr→Sr (promote) between slices. Rates =
#' transitions / starting-bucket-size, normalised to per-year.
#'
#' Slice size defaults to 90 days (not 365) to avoid the artifact where
#' jr_max_days=365 + slice=365 forces every surviving Jr to graduate,
#' saturating train_rate at 1.0.
#'
#' @param project_git A gitlog data.table with identity_id.
#' @param jr_max_days Tenure < this = Jr at slice midpoint.
#' @param sr_min_days Tenure >= this = Sr at slice midpoint.
#' @param slice_days Time-slice width. Default 90.
#' @return list with train_rate, promote_rate (medians over slices,
#'   annualised by multiplying per-slice fractions by 365/slice_days).
#' @export
estimate_transition_rates <- function(project_git, jr_max_days = 365,
                                      sr_min_days = 1095,
                                      slice_days = 90) {
  start <- min(project_git$author_datetimetz)
  end   <- max(project_git$author_datetimetz)
  step  <- as.difftime(slice_days, units = "days")
  cuts  <- seq(start, end, by = step)
  if (length(cuts) < 2) return(list(train_rate = NA_real_,
                                    promote_rate = NA_real_))

  # First-commit-date per dev (anchor for tenure)
  per_dev <- project_git[, .(first_commit = min(author_datetimetz)),
                          by = identity_id]

  cohorts_at <- function(when) {
    active <- per_dev[first_commit <= when]
    active[, td := as.numeric(difftime(when, first_commit, units = "days"))]
    active[, cohort := fcase(
      td <  jr_max_days, "Jr",
      td >= sr_min_days, "Sr",
      default          = "Tr"
    )]
    active[, .(identity_id, cohort)]
  }

  train_n   <- promote_n   <- integer(0)
  jr_at_t   <- tr_at_t     <- integer(0)
  for (i in seq_len(length(cuts) - 1)) {
    c0 <- cohorts_at(cuts[i])
    c1 <- cohorts_at(cuts[i + 1])
    m  <- merge(c0, c1, by = "identity_id",
                suffixes = c("_0", "_1"))
    jr_at_t   <- c(jr_at_t,   sum(m$cohort_0 == "Jr"))
    tr_at_t   <- c(tr_at_t,   sum(m$cohort_0 == "Tr"))
    train_n   <- c(train_n,   sum(m$cohort_0 == "Jr" & m$cohort_1 == "Tr"))
    promote_n <- c(promote_n, sum(m$cohort_0 == "Tr" & m$cohort_1 == "Sr"))
  }

  annualise <- 365 / slice_days
  return(list(
    train_rate   = annualise * median(train_n   / pmax(jr_at_t, 1),
                                      na.rm = TRUE),
    promote_rate = annualise * median(promote_n / pmax(tr_at_t, 1),
                                      na.rm = TRUE),
    n_slices     = length(cuts) - 1,
    slice_days   = slice_days
  ))
}
