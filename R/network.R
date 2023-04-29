# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Transform parsed dependencies into a sdsmj file.
#'
#' # Creates a sdsm.json from a table of dependencies from Depends.
#'
#' @param depends_table A parsed depends project by \code{\link{parse_dependencies}}.
#' @param is_sorted whether to sort the variables (filenames) in the dsm.json files
#' @export
#' @family edgelists
#' @family dv8
#' @seealso \code{\link{parse_dependencies}}, \code{\link{transform_gitlog_to_bipartite_network}},
#' \code{\link{graph_to_dsmj}}
transform_dependencies_to_sdsmj <- function(depends_table, is_sorted=FALSE){
  # Rename "filepath" to "to"
  colnames(depends_table$nodes)[1] = "name"
  # Rename "src_filepath" to "from"
  colnames(depends_table$edgelist)[1] = "from"
  # Rename "dest_filepath" to "to"
  colnames(depends_table$edgelist)[2] = "to"

  graph_to_dsmj(depends_table, is_sorted)
}

#' Transform parsed git repo into a hdsmj file.
#'
#' # Creates a hdsm.json from a parsed gitlog table.
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}.
#' @param is_sorted whether to sort the variables (filenames) in the dsm.json files
#' @export
#' @family edgelists
#' @family dv8
#' @seealso \code{\link{parse_gitlog}}, \code{\link{graph_to_dsmj}}
transform_gitlog_to_hdsmj <- function(project_git, is_sorted=FALSE){
  # Call preliminary functions to get graph and cochange for the files
  git_bipartite <- transform_gitlog_to_bipartite_network(project_git, mode ="commit-file")
  cochange_table <- bipartite_graph_projection(git_bipartite, mode = FALSE,
                                               weight_scheme_function = weight_scheme_count_deleted_nodes)

  ## Rename "weight" to "Cochange"
  #colnames(cochange_table$edgelist)[3] = "Cochange"

  graph_to_dsmj(cochange_table, is_sorted)
}

#' Transform parsed git repo into an edgelist
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}.
#' @param mode The network of interest: author-entity, committer-entity, commit-entity, author-committer
#' @export
#' @family edgelists
transform_gitlog_to_bipartite_network <- function(project_git, mode = c("author-file","committer-file","commit-file",'author-committer')){
  author_name_email <- author_datetimetz <- commit_hash <- committer_name_email <- committer_datetimetz <- lines_added <- lines_removed <- NULL # due to NSE notes in R CMD check
  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)
  # Select and rename relevant columns. Key = commit_hash.
  project_git <- project_git[,.(author=author_name_email,
                                author_date=author_datetimetz,
                                commit_hash=commit_hash,
                                committer=committer_name_email,
                                committer_date = committer_datetimetz,
                                file = file_pathname,
                                added = lines_added,
                                removed = lines_removed)]
  if(mode == "author-file"){
    git_graph <- model_directed_graph(project_git[,.(from=author,to=file)],
                                      is_bipartite=TRUE,
                                      color=c("black","#f4dbb5"))
  }else if(mode == "committer-file"){
    git_graph <- model_directed_graph(project_git[,.(from=committer,to=file)],
                                      is_bipartite=TRUE,
                                      color=c("#bed7be","#f4dbb5"))
  }else if(mode == "commit-file"){
    git_graph <- model_directed_graph(project_git[,.(from=commit_hash,to=file)],
                                      is_bipartite=TRUE,
                                      color=c("#afe569","#f4dbb5"))
  }else if(mode == "author-committer"){
    git_graph <- model_directed_graph(project_git[,.(from=author,to=committer)],
                                      is_bipartite=TRUE,
                                      color=c("black","#bed7be"))
  }
  return(git_graph)

}
#' Create time-ordered contribution network
#'
#' @description Create a collaboration network as described by Joblin et al.
#' where an edge from developer A to developer B is created if A modifies a
#' file, and B modifies it chronologically immediately after. Note contrary
#' to the paper this definition is for files, not functions, and the weight
#' of the edges is the number of changes to a file, not churn.
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}. The
#' name column will be used to label nodes.
#' @param mode author, committer
#' @export
#' @family edgelists
#' @references M. Joblin, W. Mauerer, S. Apel,
#' J. Siegmund and D. Riehle, "From Developer Networks
#' to Verified Communities: A Fine-Grained Approach,"
#' 2015 IEEE/ACM 37th IEEE International Conference on
#' Software Engineering, Florence, 2015, pp. 563-573,
#' doi: 10.1109/ICSE.2015.73.
transform_gitlog_to_temporal_network <- function(project_git,mode = c("author","committer")){
  # The code from developer A was modified by developer B
  # from A to B
  get_consecutive_identity_id <- function(identity_id_commit_date){
    dt <- identity_id_commit_date[order(datetimetz)]
    identity_id <- dt$identity_id
    consecutive_identity_id <- data.table(from = identity_id[1:(length(identity_id) - 1)],
                                      to = identity_id[2:(length(identity_id))])
    return(consecutive_identity_id)
  }

  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)



  if(mode == "author"){

    project_git <- project_git[,.(identity_id=author_name_email,
                                  datetimetz=author_datetimetz,
                                  file_pathname)]

    # Create edgelists
    git_edgelist <- project_git[, get_consecutive_identity_id(.SD),
                                by = c("file_pathname"),
                                .SDcols = c("datetimetz", "identity_id")]

    # Filter cases where no second change was made to a given file in git log
    git_edgelist <- git_edgelist[complete.cases(git_edgelist)]

    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    graph <- model_directed_graph(git_edgelist,FALSE,color="black")


  }else if(mode == "committer"){

    project_git <- project_git[,.(identity_id = committer_name_email,
                                  datetimetz=committer_datetimetz,
                                  file_pathname)]

    # Create edgelists
    git_edgelist <- project_git[, get_consecutive_identity_id(.SD),
                                by = c("file_pathname"),
                                .SDcols = c("datetimetz", "identity_id")]

    # Filter cases where no second change was made to a given file in git log
    git_edgelist <- git_edgelist[complete.cases(git_edgelist)]

    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    graph <- model_directed_graph(git_edgelist,FALSE,color="#bed7be")
  }


  return(graph)
}
#' Transform parsed git repo into an edgelist
#'
#' @param project_git_entity A parsed git project by \code{\link{parse_gitlog_entity}}.
#' @param mode The network of interest: author-entity, committer-entity, commit-entity, author-committer
#' @export
#' @family edgelists
transform_gitlog_to_entity_bipartite_network <- function(project_git_entity, mode = c("author-entity","committer-entity","commit-entity",'author-committer')){
  author_name_email <- author_datetimetz <- commit_hash <- committer_name_email <- committer_datetimetz <- lines_added <- lines_removed <- NULL # due to NSE notes in R CMD check
  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)
  # Select and rename relevant columns. Key = commit_hash.
  project_git_entity <- project_git_entity[,.(author=author_name_email,
                                author_date=author_datetimetz,
                                commit_hash=commit_hash,
                                committer=committer_name_email,
                                committer_date = committer_datetimetz,
                                entity,
                                weight)]

  if(mode == "author-entity"){
    # Select relevant columns for nodes
    git_graph <- model_directed_graph(project_git_entity[,.(from=author,to=entity)],
                                      is_bipartite=TRUE,
                                      color=c("black","#fafad2"))
  }else if(mode == "committer-entity"){
    # Select relevant columns for nodes
    git_graph <- model_directed_graph(project_git_entity[,.(from=author,to=entity)],
                                      is_bipartite=TRUE,
                                      color=c("#bed7be","#fafad2"))
  }else if(mode == "commit-entity"){
    git_graph <- model_directed_graph(project_git_entity[,.(from=commit_hash,to=entity)],
                                      is_bipartite=TRUE,
                                      color=c("#afe569","#fafad2"))
  }else if(mode == "author-committer"){
    git_graph <- model_directed_graph(project_git_entity[,.(from=author,to=committer)],
                                      is_bipartite=TRUE,
                                      color=c("#bed7be","black"))
  }
  return(git_graph)
}
#' Create time-ordered contribution network
#'
#' @description Create a collaboration network as described by Joblin et al.
#' where an edge from developer A to developer B is created if A modifies a
#' file, and B modifies it chronologically immediately after. This implementation
#' matches the one defined by Joblin et al.
#'
#' @param project_git_entity A parsed git project by \code{\link{parse_gitlog_entity}}.
#' @param mode author, committer
#' @export
#' @family edgelists
#' @references M. Joblin, W. Mauerer, S. Apel,
#' J. Siegmund and D. Riehle, "From Developer Networks
#' to Verified Communities: A Fine-Grained Approach,"
#' 2015 IEEE/ACM 37th IEEE International Conference on
#' Software Engineering, Florence, 2015, pp. 563-573,
#' doi: 10.1109/ICSE.2015.73.
transform_gitlog_to_entity_temporal_network <- function(project_git_entity,mode = c("author","committer")){
  # The code from developer A was modified by developer B
  # from A to B
  get_consecutive_authors <- function(identity_id_commit_date){
    dt <- identity_id_commit_date[order(datetimetz)]
    identity_id <- dt$identity_id
    n_lines_changed <- dt$n_lines_changed
    consecutive_identity_id <- data.table(from = identity_id[1:(length(identity_id) - 1)],
                                      to = identity_id[2:(length(identity_id))],
                                      n_lines_changed =
                                        n_lines_changed[1:(length(identity_id) - 1)] +
                                        n_lines_changed[2:(length(identity_id))]
                                      )
    return(consecutive_identity_id)
  }

  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)

  if(mode == "author"){

    data.table::setnames(project_git_entity,
            c("author_datetimetz"),
            c("datetimetz"))
    project_git_entity[,identity_id := author_name_email]

    # Create edgelists
    git_edgelist <- project_git_entity[, get_consecutive_authors(.SD),
                                       by = c("entity_definition_name"),
                                       .SDcols = c("datetimetz", "identity_id","n_lines_changed")]
    # Filter cases where no second change was made to a given file in git log
    git_edgelist <- git_edgelist[complete.cases(git_edgelist)]

    graph <- model_directed_graph(git_edgelist,FALSE,color="black")

  }else if(mode == "committer"){

    data.table::setnames(project_git_entity,
                         c("committer_datetimetz"),
                         c("datetimetz"))
    project_git_entity[,identity_id := committer_name_email]

    # Create edgelists
    git_edgelist <- project_git_entity[, get_consecutive_authors(.SD),
                                       by = c("entity_definition_name"),
                                       .SDcols = c("datetimetz", "identity_id","n_lines_changed")]
    # Filter cases where no second change was made to a given file in git log
    git_edgelist <- git_edgelist[complete.cases(git_edgelist)]

    graph <- model_directed_graph(git_edgelist,FALSE,color="#bed7be")

  }

  return(graph)
}
#' Transform parsed cveid and nvdfeed into a network
#'
#' @param project_cve A parsed cve edgelist by \code{\link{transform_commit_message_id_to_network}}.
#' @param nvd_feed  Parsed  nvdfeed by \code{\link{parse_nvdfeed}}.
#' @export
#' @family edgelists
transform_cve_cwe_file_to_network <- function(project_cve,nvd_feed){
  commit_message_id <- cwe_id <- name <- color <- src <- dest <- weight <- NULL # due to NSE notes in R CMD check

  cve_nodes <- project_cve[["nodes"]]
  cve_edgelist <- project_cve[["edgelist"]]
  # Find the edges from CVE ids to CWE ids
  cwe_edgelist <- merge(
    cve_edgelist,
    nvd_feed,
    by.x="from",
    by.y = "cve_id",
    all.x = TRUE)[,.(from,cwe_id)]
  # Edges from CVE ids without a matching CWE should be removed
  cwe_edgelist <- cwe_edgelist[!is.na(cwe_id)]
  # Add all new CWE IDs to the list of nodes with a different color
  # Type is dropped, as graph viz tools can't distinguish between 3 types of nodes
  cve_nodes <- cve_nodes[,.(name,color)]
  cwe_nodes <- data.table(name=unique(cwe_edgelist$cwe_id),
                          color="#D44942")
  # Set Union Nodes
  cve_cwe_file_nodes <- rbind(cve_nodes,cwe_nodes)
  # Network will be 3 modal, rename columns to avoid confusion
  colnames(cve_edgelist) <- c("src","dest","weight")
  colnames(cwe_edgelist) <- c("src","dest")
  # For each cve id, only 1 edge is added, hence weight is always 1
  cwe_edgelist$weight <- rep(1,nrow(cwe_edgelist))
  # Set union the cve and cwe edgelists
  cve_cwe_file_edgelist <- rbind(cve_edgelist,cwe_edgelist)
  # Return the set union as nodes and edgelist.
  cve_cwe_file_network <- list()
  cve_cwe_file_network[["nodes"]] <- cve_cwe_file_nodes
  cve_cwe_file_network[["edgelist"]] <- cve_cwe_file_edgelist
  return(cve_cwe_file_network)
}
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
#' Transform parsed git repo commit messages id and files into an edgelist
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}.
#' @param commit_message_id_regex the regex to extract the id from the commit message
#' @export
#' @family edgelists
transform_commit_message_id_to_network <- function(project_git, commit_message_id_regex){
  commit_message_id <- NULL # due to NSE notes in R CMD check
  # Extract the id according to the parameter regex
  project_git$commit_message_id <- data.table(stringi::stri_match_first_regex(project_git$commit_message,
                                                                              pattern = commit_message_id_regex))

  # Keep only the edges which contain the commit message id

  project_git <- project_git[!is.na(commit_message_id),.(commit_message_id,
                                                         file_pathname)]

  git_graph <- model_directed_graph(project_git[,.(from=commit_message_id,to=file_pathname)],
                                    is_bipartite=TRUE,
                                    color=c("#0052cc","#f4dbb5"))
  return(git_graph)

}
#' Transform parsed dependencies into a network
#'
#' @param depends_parsed A parsed mbox by \code{\link{parse_dependencies}}.
#' @param weight_types The weight types as defined in Depends.
#'
#' @export
#' @family edgelists
transform_dependencies_to_network <- function(depends_parsed,weight_types=NA){
  src <- dest <- weight <- NULL # due to NSE notes in R CMD check
  # Can only include types user wants if Depends found them at least once on codebase

  nodes <- depends_parsed[["nodes"]]
  edgelist <- depends_parsed[["edgelist"]]

  weight_types <- intersect(names(edgelist)[3:ncol(edgelist)],weight_types)
  dependency_edgelist <- edgelist[,.(src_filepath,dest_filepath)]
  if(any(is.na(weight_types))){
    dependency_edgelist$weight <- rowSums(edgelist[,3:ncol(edgelist),with=FALSE])
  }else{
    dependency_edgelist$weight <- rowSums(edgelist[,weight_types,with=FALSE])
  }
  # Remove dependencies not chosen by user
  dependency_edgelist <- dependency_edgelist[weight != 0]
  setnames(dependency_edgelist,
           old=c("src_filepath","dest_filepath"),
           new=c("from","to"))
  # Select relevant columns for nodes
  dependency_nodes <- nodes
  setnames(x=dependency_nodes,
           old="filepath",
           new="name")
  # Color files yellow
  dependency_nodes <- data.table(name=dependency_nodes$name,color="#f4dbb5")
  # Return the parsed JSON output as nodes and edgelist.
  file_network <- list()
  file_network[["nodes"]] <- dependency_nodes
  file_network[["edgelist"]] <- dependency_edgelist
  return(file_network)
}
#' Transform parsed R dependencies into a graph
#' @param r_dependencies_edgelist A parsed R folder by \code{\link{parse_r_dependencies}}.
#' @param dependency_type The type of dependency to be parsed: Function or File
#' @export
transform_r_dependencies_to_network <- function(r_dependencies_edgelist,dependency_type=c("function","file")){
  mode <- match.arg(dependency_type)
  if(mode == "function"){
    graph <-  model_directed_graph(r_dependencies_edgelist[,.(from=src_functions_call_name,
                                                              to=src_functions_caller_name)],
                                   is_bipartite = FALSE,
                                   color = c("#fafad2"))
  }else if(mode == "file"){
    graph <-  model_directed_graph(r_dependencies_edgelist[,.(from=src_functions_call_filename,
                                                              to=src_functions_caller_filename)],
                                   is_bipartite = FALSE,
                                   color = c("#f4dbb5"))

  }
  return(graph)
}

# Various imports
utils::globalVariables(c("."))
#' @importFrom data.table :=
#' @importFrom data.table setnames
NULL
