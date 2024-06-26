% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jira.R
\name{create_components}
\alias{create_components}
\title{Create Components}
\usage{
create_components(jira_domain_url, components)
}
\arguments{
\item{jira_domain_url}{URL of JIRA domain}

\item{components}{list of names of components}
}
\value{
A list named 'components' which contains each component and its details
}
\description{
Creates the component cells for \code{\link{make_jira_issue}}.
}
\keyword{internal}
