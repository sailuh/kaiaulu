% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{github_parse_project_issue}
\alias{github_parse_project_issue}
\title{Parse Issues JSON to Table}
\usage{
github_parse_project_issue(api_responses)
}
\arguments{
\item{api_responses}{API response obtained from github_api_* function.}
}
\description{
Note not all columns available in the downloaded json are parsed.
}
