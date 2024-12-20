% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{github_api_project_issue_refresh}
\alias{github_api_project_issue_refresh}
\title{Download Project Issues Refresh}
\usage{
github_api_project_issue_refresh(
  owner,
  repo,
  token,
  save_path_issue_refresh,
  issue_or_pr,
  verbose
)
}
\arguments{
\item{owner}{GitHub's repository owner (e.g. sailuh)}

\item{repo}{GitHub's repository name (e.g. kaiaulu)}

\item{token}{Your GitHub API token}

\item{save_path_issue_refresh}{The folder path that the refresh downloader downloads to}

\item{issue_or_pr}{This specifies whether issues or pull requests are being searched for.
Acceptable inputs are "is:issue" or "is:pull-request".}

\item{verbose}{A boolean value that prints operational messages when set to TRUE.
These may include announcing successful execution of code, API queries, files saved, etc.}
}
\description{
Uses the adopted file name convention by \code{\link{github_api_iterate_pages}} to identify
the latest downloaded Github created_at date among directory "issue_search".
It returns the first page of the github query for issues created after this date by calling
\code{\link{github_api_project_issue_search}}
}
\details{
If the issue directory is empty, then the created query will not be appended to the api call
and the first page of a query retrieving all issues will be returned. This function can therefore
be used in the specified folder to continuously refresh available issues
data.
}
\references{
For details, see \url{https://docs.github.com/en/rest/reference/issues#list-repository-issues}.

For details on timestampes, se \url{https://docs.github.com/en/search-github/searching-on-github/searching-issues-and-pull-requests#search-by-when-an-issue-or-pull-request-was-created-or-last-updated}
}
\seealso{
\code{link{github_api_project_issue}} to download all issue data

\code{link{format_created_at_from_file}} for function that iterates through
a .json file and returns the greatest 'created_at' value

\code{link{github_api_iterate_pages}} to write data returned by this function to file as .json
}
