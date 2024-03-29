% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/src.R
\name{write_gof_patterns}
\alias{write_gof_patterns}
\title{Write GoF Patterns}
\usage{
write_gof_patterns(
  pattern4_path,
  class_folder_path,
  output_filepath = "/tmp/gof.xml"
)
}
\arguments{
\item{pattern4_path}{The path to Tsantalis' pattern4 jar}

\item{class_folder_path}{The path to a folder one
level above subdirectories that contain the class files.}

\item{output_filepath}{Optional path to store the XML generated by pattern4. If not
specified, it will be saved to `/tmp/gof.xml`.}
}
\value{
A data.table containing the parsed gof patterns per class.
}
\description{
Write GoF patterns generated by `pattern4.jar` into a a table.
\url{https://www.srcml.org/documentation.html}.
Pattern4.jar is available on
[Tsantalis' homepage](https://users.encs.concordia.ca/~nikolaos/pattern_detection.html)).
}
\references{
N. Tsantalis, A. Chatzigeorgiou, G. Stephanides, S. T. Halkidis,
"Design Pattern Detection Using Similarity Scoring",
IEEE Transactions on Software Engineering,
vol. 32, no. 11, pp. 896-909, November, 2006.
}
