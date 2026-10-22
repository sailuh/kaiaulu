\name{get_pysenti_predict_path}
\alias{get_pysenti_predict_path}
\title{Returns the path to the PySenti prediction Python script}
\usage{
get_pysenti_predict_path(config_file)
}
\arguments{
\item{config_file}{A parsed configuration file obtained from \code{\link{parse_config}}.}
}
\description{
This function retrieves the path to the PySenti prediction Python script from
a parsed configuration file. If the attribute does not exist, a warning is issued.
}
\value{
The file path to the PySenti prediction script as a string.
}
