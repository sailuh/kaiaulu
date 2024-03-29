% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter_by_file_extension}
\alias{filter_by_file_extension}
\title{Filter commit files by extension}
\usage{
filter_by_file_extension(dt_file, extension, file_column_name)
}
\arguments{
\item{dt_file}{any data.table containing filepaths}

\item{extension}{a character vector of extensions (e.g. c(py,java)) to *keep*
in the table}

\item{file_column_name}{a string indicating the column name which contains filepaths}
}
\value{
a data.table which contains only filepaths with the specified extensions
}
\description{
Filters a data.table containing filepaths using the specified extensions
}
\seealso{
\code{\link{parse_gitlog}} and \code{\link{parse_dependencies}} to create dt_file
}
\concept{{filters}}
