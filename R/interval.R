# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# metric_function <- metric_churn_per_commit_interval

#' Calculate Metric for Commit Interval
#'
#' Calculates a metric using `metric_function` for a list of commit intervals specified in `dt_range`. All commit
#' ranges must exist in `project_git` in order to derive the original timestamps to subset the interval.
#' @param project_git a data.table where the key is commit+file
#' @param dt_range a data.table which contains a column `range` specifying the
#' commit interval on the form `commitsha1-commitsha2`
#' @return a numeric vector of `metric_function` values for each commit interval specified in `dt_range`
#' @seealso \code{\link{metric_churn_per_commit_interval}} as an example of metric_function
#' @export
interval_commit_metric <- function(project_git,dt_range,metric_function,file_extensions,substring_filepath){
  commit_interval_metric <- c()
  for(i in 1:nrow(dt_range)){
    start_commit <- stri_split_regex(dt_range[i]$range,pattern = "-")[[1]][1]
    end_commit <- stri_split_regex(dt_range[i]$range,pattern = "-")[[1]][2]

    sum_metric <- filter_by_commit_interval(project_git,start_commit,end_commit)  %>%
      filter_by_file_extension(file_extensions)  %>%
      filter_by_filepath_substring(substring_filepath)  %>%
      metric_function()

    commit_interval_metric <- c(commit_interval_metric,sum_metric)
  }
  return(commit_interval_metric)
}
#' Get datetime from commit hash in a git log
#'
#' Locates the commit hash in `git_log` and obtains the timestamp. Assumes `git_log` contains a `data.AuthorDate``
#' column.
#' @param git_log a data.table where the key is commit+file
#' @param commit_hash a string character with the commit hash we want to obtain the datetime from
#' commit interval on the form `commitsha1-commitsha2`
#' @return a numeric vector of `metric_function` values for each commit interval specified in `dt_range`
get_date_from_commit_hash <- function(git_log,commit_hash){
  return(git_log[data.commit == commit_hash]$data.AuthorDate[1])
}

#' @importFrom magrittr %>%
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
NULL
