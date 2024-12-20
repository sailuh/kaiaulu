% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{parse_github_replies}
\alias{parse_github_replies}
\title{Parse GitHub Issue and Pull Request Comments}
\usage{
parse_github_replies(
  issues_json_folder_path,
  pull_requests_json_folder_path,
  comments_json_folder_path,
  commit_json_folder_path
)
}
\arguments{
\item{issues_json_folder_path}{The path to the downloaded issues JSON. See \code{\link{github_api_project_issue}}.}

\item{pull_requests_json_folder_path}{The path to the downloaded pull requests JSON. See \code{\link{github_api_project_pull_request}}.}

\item{comments_json_folder_path}{The path to the downloaded comments JSON. See \code{\link{github_api_project_issue_or_pr_comments}}.}

\item{commit_json_folder_path}{The path to the downloaded commits JSON (used to map github username to the git log). See \code{\link{github_api_project_commits}}.}
}
\value{
A single reply table which combines the communication from the three jsons.
}
\description{
Parses Issue, Pull Request, and Comments Endpoints into a reply table.
See example usage in the download_github_comments.Rmd vignette.
}
