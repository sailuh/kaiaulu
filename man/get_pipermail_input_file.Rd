% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{get_pipermail_input_file}
\alias{get_pipermail_input_file}
\title{Returns the local input file for pipermail for a specific project key.}
\usage{
get_pipermail_input_file(config_file, project_key_index)
}
\arguments{
\item{config_file}{The parsed configuration file obtained from \code{\link{parse_config}}.}

\item{project_key_index}{The name of the index of the project key (e.g. "project_key_1" or "project_key_2").}
}
\value{
The local input file pipermail path for project specified by key `project_key_index`.
}
\description{
This function returns the local file used for input for
pipermail for a specific project key, `project_key_index`, that is specified
in the input parameter `config_file`. The input, `config_file` must be a
parsed configuration file. The function will inform the user if the specific
local input file path for pipermail exists in the parsed configuration file,
`config_file`.
}
