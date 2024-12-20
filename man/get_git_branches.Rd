% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{get_git_branches}
\alias{get_git_branches}
\title{Returns the list of git branches used for analysis in the current project.}
\usage{
get_git_branches(config_file)
}
\arguments{
\item{config_file}{The parsed configuration file obtained from \code{\link{parse_config}}.}
}
\value{
The list of git branches.
}
\description{
This function returns a list of the git branches used for
analysis in the current project specified in the input parameter
`config_file`. The input, `config_file` must be a parsed configuration file.
The function will inform the user if the list of branches to be analyzed
exists in the parsed configuration file, `config_file`.
}
