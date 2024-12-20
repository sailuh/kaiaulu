% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{get_mbox_key_indexes}
\alias{get_mbox_key_indexes}
\title{Returns the list of mailing list mod mbox project keys.}
\usage{
get_mbox_key_indexes(config_file)
}
\arguments{
\item{config_file}{The parsed configuration file obtained from \code{\link{parse_config}}.}
}
\value{
The list of mod mbox mailing list keys.
}
\description{
This function returns the list of mailing list mod mbox project
keys, that is specified in the input parameter `config_file`. The input,
`config_file` must be a parsed configuration file. The function will inform
the user if the project keys exist in the parsed configuration
file, `config_file`.
}
