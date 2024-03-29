% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dv8.R
\name{parse_dv8_architectural_flaws}
\alias{parse_dv8_architectural_flaws}
\title{Parse Architecture Flaws Map}
\usage{
parse_dv8_architectural_flaws(
  dv8_path,
  flaws_path,
  dsm_type = "merge",
  keep_intermediate_files = FALSE,
  progress_bar = NULL
)
}
\arguments{
\item{dv8_path}{path to dv8 binary}

\item{flaws_path}{path to architecture folder created by \code{\link{dv8_mdsmb_to_flaws}}}

\item{dsm_type}{the type of dsm that should be used available in the folder (i.e. merge, hdsm, or sdsm)}

\item{keep_intermediate_files}{TRUE if the user wishes to keep the intermediate files generated by this function, FALSE otherwise}

\item{progress_bar}{a boolean specifying if a progress bar should be shown.}
}
\value{
data.table object with file to flaw mapping
}
\description{
Computes from the folder template a table containing the
file assignment to identified architectural flaws. Note
this is a computational expensive function, and may take
awhile to finish.
}
\seealso{
Other dv8: 
\code{\link{dependencies_to_sdsmj}()},
\code{\link{dv8_clsxb_to_clsxj}()},
\code{\link{dv8_clsxj_to_clsxb}()},
\code{\link{dv8_depends_to_sdsmj}()},
\code{\link{dv8_dsmj_to_dsmb}()},
\code{\link{dv8_hdsmb_sdsmb_to_mdsmb}()},
\code{\link{dv8_mdsmb_to_flaws}()},
\code{\link{gitlog_to_hdsmj}()},
\code{\link{graph_to_dsmj}()},
\code{\link{parse_dv8_clusters}()},
\code{\link{transform_dependencies_to_sdsmj}()},
\code{\link{transform_gitlog_to_hdsmj}()},
\code{\link{transform_temporal_gitlog_to_adsmj}()}
}
\concept{dv8}
