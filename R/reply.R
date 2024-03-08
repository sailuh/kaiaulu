# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Network Transform ##############


#' Transform parsed mbox or parsed jira replies into a network
#'
#' @param project_reply A parsed mbox by \code{\link{parse_mbox}} or \code{\link{parse_jira_replies}}.
#' @export
#' @family edgelists
transform_reply_to_bipartite_network <- function(project_reply){
  data.From <- data.Subject <- data.Date <- NULL # due to NSE notes in R CMD check

  git_graph <- model_directed_graph(project_reply[,.(from=reply_from,to=reply_subject)],
                                    is_bipartite=TRUE,
                                    color=c("black","lightblue"))
  return(git_graph)
}
