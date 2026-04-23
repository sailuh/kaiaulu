# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Network Transform ##############


#' Transform parsed mbox or parsed jira replies into a network
#'
#' @param project_reply A parsed mbox by \code{\link{parse_mbox}} or \code{\link{parse_jira_replies}}.
#' @param source A string label for the data source (e.g. "mbox", "jira", "github"). Stored as a
#'   column in both the node list and edgelist so downstream functions can filter by source if
#'   needed but can also ignore it entirely. Defaults to \code{NA_character_}.
#' @param weight A string naming a numeric column in \code{project_reply} to use as the edge
#'   weight. When \code{NULL} (default), edges are weighted by message count.
#' @export
#' @family edgelists
transform_reply_to_bipartite_network <- function(project_reply, source = NA_character_, weight = NULL){
  reply_from <- reply_subject <- NULL # due to NSE notes in R CMD check
  from_nodes   <- data.table(name = unique(project_reply[["reply_from"]]),    type = TRUE,  color = "black")
  to_nodes     <- data.table(name = unique(project_reply[["reply_subject"]]), type = FALSE, color = "lightblue")
  reply_nodes  <- rbind(from_nodes, to_nodes)
  if(is.null(weight)){
    reply_edgelist <- project_reply[, .(weight = .N), by = .(from = reply_from, to = reply_subject)]
  } else {
    weight_col <- weight
    reply_edgelist <- project_reply[, .(weight = .SD[[weight_col]]),
                                    by = .(from = reply_from, to = reply_subject),
                                    .SDcols = weight_col]
  }
  reply_edgelist[, source    := source]
  reply_edgelist[, direction := "directed"]
  reply_graph  <- model_multimodal_graph(reply_nodes, reply_edgelist, direction = "directed", is_bipartite = TRUE)
  return(reply_graph)
}
