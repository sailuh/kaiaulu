% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.R
\name{weight_scheme_sum_edges}
\alias{weight_scheme_sum_edges}
\title{Weight Edge Sum Projection Scheme}
\usage{
weight_scheme_sum_edges(projected_graph)
}
\arguments{
\item{projected_graph}{A semi-processed bipartite projection network resulting}
}
\description{
This weight scheme sums the deleted node adjacent edges when re-wired
in the projection graph. For most cases, this is the commonly used
approach in the literature.
}
\details{
For example assume author A changes file1 three times, and
author B changes file1 one time. A projection which returns
a author-author network using this function would return,
assuming this being the entire collaboration graph,
that author A and author B have a connection of 3 + 1 = 4.

Suppose now also that the entire collaboration graph contains
another file2 such that author A and author B modified 2 and 3
times respectively. Hence in the projection step, this would
further contribute a 2 + 3 = 5 weight to authors A and B. In this
graph, where file1 and file2 are present, the final weight for
authors A and B would thus be (3 + 1) + (2 + 3) = 9.
}
\concept{weight_scheme
from \code{\link{bipartite_graph_projection}} when specifying
weight_scheme_function = NA.}
