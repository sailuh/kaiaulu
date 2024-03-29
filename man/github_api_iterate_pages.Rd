% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{github_api_iterate_pages}
\alias{github_api_iterate_pages}
\title{GitHub Page Iterator}
\usage{
github_api_iterate_pages(
  token,
  gh_response,
  save_folder_path,
  prefix = NA,
  max_pages = NA
)
}
\arguments{
\item{token}{Your GitHub API token}

\item{gh_response}{A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).}

\item{save_folder_path}{A folder path to save the downloaded json pages "as-is".}

\item{prefix}{Prefix to be added to every json file name}

\item{max_pages}{The maximum number of pages to download. MAX = Available token requests left}
}
\description{
GitHub API endpoints return data in pages, each containing by default 100 entries.
This iterator can be used to iterate over the next page in order to download all the
project's data available from the endpoint (up to a user-defined maximum or the remaining
available requests in the used user's token).
}
\references{
For details see \url{https://docs.github.com/en/free-pro-team@latest/rest/guides/traversing-with-pagination}.
}
\keyword{internal}
