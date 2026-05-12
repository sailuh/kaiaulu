# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Network Transform ##############


#' Transform parsed mbox or parsed jira replies into a network
#'
#' @param project_reply A parsed mbox by \code{\link{parse_mbox}} or \code{\link{parse_jira_replies}}.
#' @param weight A string naming a numeric column in \code{project_reply} to use as the edge
#'   weight. When \code{NULL} (default), edges are weighted by message count.
#' @param weight_agg A function to aggregate the weight column across multiple messages
#'   sharing the same \code{from}/\code{to} pair. Defaults to \code{mean}.
#' @export
#' @family edgelists
transform_reply_to_bipartite_network <- function(project_reply, weight = NULL, weight_agg = mean){
  reply_from <- reply_subject <- NULL # due to NSE notes in R CMD check
  from_nodes   <- data.table(name = unique(project_reply[["reply_from"]]),    type = TRUE,  color = "black")
  to_nodes     <- data.table(name = unique(project_reply[["reply_subject"]]), type = FALSE, color = "lightblue")
  reply_nodes  <- rbind(from_nodes, to_nodes)
  if(is.null(weight)){
    reply_edgelist <- project_reply[, .(weight = .N), by = .(from = reply_from, to = reply_subject)]
  } else {
    if(!weight %in% colnames(project_reply)){
      stop("Column '", weight, "' not found in project_reply.")
    }
    weight_col <- weight
    reply_edgelist <- project_reply[, .(weight = weight_agg(.SD[[weight_col]])),
                                    by = .(from = reply_from, to = reply_subject),
                                    .SDcols = weight_col]
  }
  reply_edgelist[, direction := "directed"]
  reply_graph  <- model_multimodal_graph(reply_nodes, reply_edgelist, direction = "directed", is_bipartite = TRUE)
  return(reply_graph)
}
