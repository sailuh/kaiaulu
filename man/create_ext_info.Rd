% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jira.R
\name{create_ext_info}
\alias{create_ext_info}
\title{Create ext_info_cel}
\usage{
create_ext_info(
  jira_domain_url,
  issue_type,
  status,
  resolution,
  title,
  description,
  components,
  creator_name,
  reporter_name,
  assignee_name
)
}
\arguments{
\item{jira_domain_url}{URL of JIRA domain}

\item{issue_type}{description of issue_type}

\item{status}{status of issue for development}

\item{resolution}{name of resolution for issue}

\item{title}{summary of the issue}

\item{description}{description of issue}

\item{components}{components of issue, a list with component names separated by ; (ex. "x-core;x-spring" is two components)}

\item{creator_name}{name of creator of issue}

\item{reporter_name}{name of reporter reporting the issue}

\item{assignee_name}{name of person the issue is being assigned to}
}
\value{
A list named 'ext_info_cell' which contains all the parameters and its generated fake data formats
}
\description{
Creates and formats ext_info_cell list \code{\link{make_jira_issue}}.
}
\keyword{internal}
