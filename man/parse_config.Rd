% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{parse_config}
\alias{parse_config}
\title{Returns the parsed configuration file (.yml).}
\usage{
parse_config(config_path)
}
\arguments{
\item{config_path}{The path of the config file from the kaiaulu directory (e.g. "conf/kaiaulu.yml").}
}
\value{
The parsed config file whose path is specified by `config_path`.
}
\description{
The input file is expected to be in the .yml format.
The function returns a parsed version of the input .yml file, and it will
inform the user if the input .yml file path does not exist. The contents
of the input .yml file may contain machine-dependent paths that may need to
be modified by the user.
}
