% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jira.R
\name{refresh_jira_issues}
\alias{refresh_jira_issues}
\title{Refresh JIRA Issues and/or Comments}
\usage{
refresh_jira_issues(
  domain,
  jql_query,
  fields,
  username = NULL,
  password = NULL,
  save_folder_path,
  max_results,
  max_total_downloads,
  verbose
)
}
\arguments{
\item{domain}{Custom JIRA domain URL}

\item{jql_query}{Specific query string to specify criteria for fetching}

\item{fields}{List of fields that are downloaded in each issue}

\item{username}{the JIRA username}

\item{password}{the JIRA password/API token}

\item{save_folder_path}{Path that files will be saved in .json format}

\item{max_results}{(optional) the [maximum number of results](https://confluence.atlassian.com/jirakb/how-to-use-the-maxresults-api-parameter-for-jira-issue-search-rest-api-1251999998.html)
to download per page. Default is 50.}

\item{max_total_downloads}{Maximum downloads per function call.
This parameter specifies how many issues should be downloaded. Subsequent API calls will stop if they
reach or surpass this value.}

\item{verbose}{Set verbose=TRUE to print execution details.}
}
\description{
Uses the adopted file name convention by \code{\link{download_jira_issues}} to identify
the latest downloaded JIRA issue key KEY-i, and calls
\code{\link{download_jira_issues_by_issue_key}} with lower bound KEY-(i+1) to download all
newer issues.
}
\details{
If the directory is empty, then all issues will be downloaded. This function can therefore
be used in the specified folder to continuously refresh available issues and/or comments
data.
}
\seealso{
\code{\link{download_jira_issues_by_date}} to download JIRA issues and/or comments by date range

\code{\link{download_jira_issues_by_issue_key}} to download JIRA issues and/or comments by issue key range

\code{\link{download_jira_issues}} for more flexibility in specifying the JQL query

\code{\link{parse_jira}} to parse jira data with or without comments

\code{\link{parse_jira_latest_date}} to retrieve the file path of the latest issue key

Other downloaders: 
\code{\link{download_jira_issues_by_date}()},
\code{\link{download_jira_issues_by_issue_key}()},
\code{\link{download_jira_issues}()}

Other jira: 
\code{\link{download_jira_issues_by_date}()},
\code{\link{download_jira_issues_by_issue_key}()},
\code{\link{download_jira_issues}()}
}
\concept{downloaders}
\concept{jira}
