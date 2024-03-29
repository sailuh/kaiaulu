% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric.R
\name{metric_churn_per_commit_interval}
\alias{metric_churn_per_commit_interval}
\title{Churn Metric per Commit Interval}
\usage{
metric_churn_per_commit_interval(git_log)
}
\arguments{
\item{git_log}{a parsed git log table where each row is identified by commit+file}
}
\value{
a single value with the sum of all churn in the commit interval
}
\description{
Calculates the churn metric for a sequence of commits
}
\seealso{
\code{\link{parse_gitlog}} to obtain `git_log``

Other {metrics}: 
\code{\link{commit_message_id_coverage}()},
\code{\link{metric_churn_per_commit_per_file}()},
\code{\link{metric_churn}()},
\code{\link{metric_file_bug_churn}()},
\code{\link{metric_file_bug_frequency}()},
\code{\link{metric_file_churn}()},
\code{\link{metric_file_non_bug_churn}()},
\code{\link{metric_file_non_bug_frequency}()}
}
\concept{{metrics}}
