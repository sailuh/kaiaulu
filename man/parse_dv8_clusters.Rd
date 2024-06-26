% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dv8.R
\name{parse_dv8_clusters}
\alias{parse_dv8_clusters}
\title{Parses a DV8 json cluster file to R data.table.}
\usage{
parse_dv8_clusters(clsxj_path)
}
\arguments{
\item{clsxj_path}{path to clustering json created by \code{\link{dv8_clsxb_to_clsxj}}}
}
\description{
Parses a cluster *-clsx.json into a data table.
}
\seealso{
Other parsers: 
\code{\link{parse_bugzilla_perceval_rest_issue_comments}()},
\code{\link{parse_bugzilla_perceval_traditional_issue_comments}()},
\code{\link{parse_bugzilla_rest_comments}()},
\code{\link{parse_bugzilla_rest_issues_comments}()},
\code{\link{parse_bugzilla_rest_issues}()},
\code{\link{parse_commit_message_id}()},
\code{\link{parse_dependencies}()},
\code{\link{parse_gitlog}()},
\code{\link{parse_jira_latest_date}()},
\code{\link{parse_jira_rss_xml}()},
\code{\link{parse_jira}()},
\code{\link{parse_mbox}()},
\code{\link{parse_nvdfeed}()}

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
\code{\link{parse_dv8_architectural_flaws}()},
\code{\link{transform_dependencies_to_sdsmj}()},
\code{\link{transform_gitlog_to_hdsmj}()},
\code{\link{transform_temporal_gitlog_to_adsmj}()}
}
\concept{dv8}
\concept{parsers}
