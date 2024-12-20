% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{github_parse_search_issues_refresh}
\alias{github_parse_search_issues_refresh}
\title{Parse Issues JSON from refresh to Table}
\usage{
github_parse_search_issues_refresh(api_responses)
}
\arguments{
\item{api_responses}{API response obtained from github_api_* function.}
}
\description{
Note not all columns available in the downloaded json are parsed. This parser
is adapted from \code{link{github_parse_project_issue}} to parse data
from the refresh_issue folder. This data is downloaded from the Github API
search endpoint and has a different level of nesting than the original data
}
\seealso{
\code{link{github_api_project_issue_refresh}} to refresh issue data
}
