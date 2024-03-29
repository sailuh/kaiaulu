% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval.R
\name{interval_commit_metric}
\alias{interval_commit_metric}
\title{Calculate Metric for Commit Interval}
\usage{
interval_commit_metric(
  project_git,
  dt_range,
  metric_function,
  file_extensions,
  substring_filepath,
  file_column_name
)
}
\arguments{
\item{project_git}{a data.table where the key is commit+file}

\item{dt_range}{a data.table which contains a column `range` specifying the
commit interval on the form `commitsha1-commitsha2`}

\item{metric_function}{A metric function from metric.R to apply to each interval}

\item{file_extensions}{a character vector of extensions (e.g. c(py,java)) to *keep*}

\item{substring_filepath}{a character vector of substrings (e.g. c(py,java)) we wish to *filter*}

\item{file_column_name}{a string indicating the column name which contains filepaths}
}
\value{
a numeric vector of `metric_function` values for each commit interval specified in `dt_range`
}
\description{
Calculates a metric using `metric_function` for a list of commit intervals specified in `dt_range`. All commit
ranges must exist in `project_git` in order to derive the original timestamps to subset the interval.
}
\seealso{
\code{\link{metric_churn_per_commit_interval}} as an example of metric_function
}
