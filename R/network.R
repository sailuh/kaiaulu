# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Transform parsed git repo into an edgelist
#'
#' @param project_git A parsed git project by \code{parse_gitlog}.
#' @param mode The network of interest: author-file, commit-file, or author-comitter
#' @export
#' @family edgelists
parse_gitlog_network <- function(project_git, mode = c("author","commit",'author-committer')){
  data.Author <- data.AuthorDate <- data.commit <- data.Commit <- data.CommitDate <- added <- removed <- NULL # due to NSE notes in R CMD check
  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)
  # Select and rename relevant columns. Key = commit_hash.
  project_git <- project_git[,.(author=data.Author,
                                author_date=data.AuthorDate,
                                commit_hash=data.commit,
                                committer=data.Commit,
                                committer_date = data.CommitDate,
                                file,added,removed)]
  if(mode == "author"){
    # Select relevant columns for nodes
    git_nodes <- c(unique(project_git$author),unique(project_git$file))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- project_git[,.(weight=.N),by=c("author","file")]
    # Color nodes authors black, and files yellow
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$author,
                                                        "black",
                                                        "#f4dbb5"))
    # bipartite graph
    git_nodes$type <- ifelse(git_nodes$name %in% git_edgelist$author,
                             TRUE,
                             FALSE)
  }else if(mode == "commit"){
    # Select relevant columns for nodes
    git_nodes <- c(unique(project_git$commit_hash),unique(project_git$file))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- project_git[,.(weight=.N),by=c("commit_hash","file")]
    # Color authors black, and commits green and files yellow
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$commit_hash,
                                                        "#afe569",
                                                        "#f4dbb5"))
    # This undirected graph is also bipartite
    git_nodes$type <-  ifelse(git_nodes$name %in% git_edgelist$commit_hash,
                              TRUE,
                              FALSE)
  }else if(mode == "author-committer"){
    # Select relevant columns for nodes
    git_nodes <- unique(c(project_git$author,project_git$committer))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- project_git[,.(weight=.N),by=c("author","committer")]
    # Color authors who appear at least once under comitter as gray. Author only roles are black as usual.
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$committer,
                                                        "#bed7be",
                                                        "black"))
    # This undirected graph is also bipartite
    git_nodes$type <-  ifelse(git_nodes$name %in% git_edgelist$author,
                              TRUE,
                              FALSE)
  }
  git_network <- list()
  git_network[["nodes"]] <- git_nodes
  git_network[["edgelist"]] <- git_edgelist
  return(git_network)

}
#' Create time-ordered contribution network
#'
#' @description Create a collaboration network as described by Joblin et al.
#' where an edge from developer A to developer B is created if A modifies a
#' file, and B modifies it chronologically immediately after. Note contrary
#' to the paper this definition is for files, not functions, and the weight
#' of the edges is the number of changes to a file, not churn.
#'
#' @param project_git A parsed git project by \code{parse_gitlog}.
#' @export
#' @family edgelists
#' @references M. Joblin, W. Mauerer, S. Apel,
#' J. Siegmund and D. Riehle, "From Developer Networks
#' to Verified Communities: A Fine-Grained Approach,"
#' 2015 IEEE/ACM 37th IEEE International Conference on
#' Software Engineering, Florence, 2015, pp. 563-573,
#' doi: 10.1109/ICSE.2015.73.
parse_gitlog_temporal_network <- function(project_git, mode = c("author","identity")){
  # The code from developer A was modified by developer B
  # from A to B
  get_consecutive_authors <- function(author_commit_date){
    dt <- author_commit_date[order(data.AuthorDate)]
    author <- dt$name
    consecutive_authors <- data.table(from = author[1:(length(author) - 1)],
                                      to = author[2:(length(author))])

    return(consecutive_authors)
  }

  mode <- match.arg(mode)

  # If identity matching was performed, use the name column will be id
  if("id" %in% colnames(project_git)){
    # Create nodes
    project_git[,name:=id]
    # Select relevant columns for nodes
    git_nodes <- unique(project_git[,.(name,raw_name)])
    git_nodes <- git_nodes[,.(raw_name = stri_c(raw_name,collapse = " | ")),
                           by="name"]
    setcolorder(git_nodes,c("name","raw_name"))
    # Color nodes authors black, and files yellow
    git_nodes[,color:="black"]

    # Create edgelists
    git_edgelist <- project_git[, get_consecutive_authors(.SD),
                                by = c("file"),
                                .SDcols = c("data.AuthorDate", "name")]
    # Filter cases where no second change was made to a given file in git log
    git_edgelist <- git_edgelist[complete.cases(git_edgelist)]
    #setcolorder(git_edgelist,c("from","to","file"))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- git_edgelist[,.(weight=.N),by=c("from","to")]
    # Assign to name the merged names or the id depending on user choice
    if(mode == "author"){
      mapping <- git_nodes$raw_name
      names(mapping) <- git_nodes$name
      git_nodes[,name:= raw_name]
      git_edgelist[,from:= mapping[as.character(from)]]
      git_edgelist[,to:= mapping[as.character(to)]]
    }
  }else{
    stop("No id column found. Did you use identity matching?")
  }

  git_network <- list(nodes=git_nodes,edgelist=git_edgelist)


  return(git_network)
}
#' Transform parsed git repo into an edgelist
#'
#' @param project_git_entity A parsed git project by \code{parse_gitlog_entity}.
#' @param mode The network of interest: author-file, commit-file, or author-comitter
#' @export
#' @family edgelists
parse_gitlog_entity_network <- function(project_git_entity, mode = c("author","commit",'author-committer')){
  data.Author <- data.AuthorDate <- data.commit <- data.Commit <- data.CommitDate <- added <- removed <- NULL # due to NSE notes in R CMD check
  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)
  # Select and rename relevant columns. Key = commit_hash.
  project_git_entity <- project_git_entity[,.(author=data.Author,
                                author_date=data.AuthorDate,
                                commit_hash=data.commit,
                                committer=data.Commit,
                                committer_date = data.CommitDate,
                                entity,
                                weight)]
  if(mode == "author"){
    # Select relevant columns for nodes
    git_nodes <- c(unique(project_git_entity$author),unique(project_git_entity$entity))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- project_git_entity[,.(weight=.N),by=c("author","entity")]
    # Color nodes authors black, and files yellow
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$author,
                                                        "black",
                                                        "#fafad2"))
    # bipartite graph
    git_nodes$type <- ifelse(git_nodes$name %in% git_edgelist$author,
                             TRUE,
                             FALSE)
  }else if(mode == "commit"){
    # Select relevant columns for nodes
    git_nodes <- c(unique(project_git_entity$commit_hash),unique(project_git_entity$file))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- project_git_entity[,.(weight=.N),by=c("commit_hash","entity")]
    # Color authors black, and commits green and files yellow
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$commit_hash,
                                                        "#afe569",
                                                        "#fafad2"))
    # This undirected graph is also bipartite
    git_nodes$type <-  ifelse(git_nodes$name %in% git_edgelist$commit_hash,
                              TRUE,
                              FALSE)
  }else if(mode == "author-committer"){
    # Select relevant columns for nodes
    git_nodes <- unique(c(project_git_entity$author,project_git_entity$committer))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- project_git_entity[,.(weight=.N),by=c("author","committer")]
    # Color authors who appear at least once under comitter as gray. Author only roles are black as usual.
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$committer,
                                                        "#bed7be",
                                                        "black"))
    # This undirected graph is also bipartite
    git_nodes$type <-  ifelse(git_nodes$name %in% git_edgelist$author,
                              TRUE,
                              FALSE)
  }
  git_network <- list()
  git_network[["nodes"]] <- git_nodes
  git_network[["edgelist"]] <- git_edgelist
  return(git_network)

}
#' Create time-ordered contribution network
#'
#' @description Create a collaboration network as described by Joblin et al.
#' where an edge from developer A to developer B is created if A modifies a
#' file, and B modifies it chronologically immediately after. This implementation
#' matches the one defined by Joblin et al.
#'
#' @param project_git_entity A parsed git project by \code{parse_gitlog_entity}.
#' @export
#' @family edgelists
#' @references M. Joblin, W. Mauerer, S. Apel,
#' J. Siegmund and D. Riehle, "From Developer Networks
#' to Verified Communities: A Fine-Grained Approach,"
#' 2015 IEEE/ACM 37th IEEE International Conference on
#' Software Engineering, Florence, 2015, pp. 563-573,
#' doi: 10.1109/ICSE.2015.73.
parse_gitlog_entity_temporal_network <- function(project_git_entity, mode = c("author","identity")){
  # The code from developer A was modified by developer B
  # from A to B
  get_consecutive_authors <- function(author_commit_date){
    dt <- author_commit_date[order(data.AuthorDate)]
    author <- dt$name
    n_lines_changed <- dt$n_lines_changed
    consecutive_authors <- data.table(from = author[1:(length(author) - 1)],
                                      to = author[2:(length(author))],
                                      n_lines_changed =
                                        n_lines_changed[1:(length(author) - 1)] +
                                        n_lines_changed[2:(length(author))]
                                      )

    return(consecutive_authors)
  }

  mode <- match.arg(mode)

  # If identity matching was performed, use the name column will be id
  if("id" %in% colnames(project_git_entity)){
    # Create nodes
    project_git_entity[,name:=id]
    # Select relevant columns for nodes
    git_nodes <- unique(project_git_entity[,.(name,raw_name)])
    git_nodes <- git_nodes[,.(raw_name = stri_c(raw_name,collapse = " | ")),
                           by="name"]
    setcolorder(git_nodes,c("name","raw_name"))
    # Color nodes authors black, and files yellow
    git_nodes[,color:="black"]

    # Create edgelists
    git_edgelist <- project_git_entity[, get_consecutive_authors(.SD),
                                by = c("entity_definition_name"),
                                .SDcols = c("data.AuthorDate", "name","n_lines_changed")]
    # Filter cases where no second change was made to a given file in git log
    git_edgelist <- git_edgelist[complete.cases(git_edgelist)]
    #setcolorder(git_edgelist,c("from","to","file"))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- git_edgelist[,.(weight=.N),by=c("from","to")]
    # Assign to name the merged names or the id depending on user choice
    if(mode == "author"){
      mapping <- git_nodes$raw_name
      names(mapping) <- git_nodes$name
      git_nodes[,name:= raw_name]
      git_edgelist[,from:= mapping[as.character(from)]]
      git_edgelist[,to:= mapping[as.character(to)]]
    }
  }else{
    stop("No id column found. Did you use identity matching?")
  }

  git_network <- list(nodes=git_nodes,edgelist=git_edgelist)


  return(git_network)
}
#' Transform parsed cveid and nvdfeed into a network
#'
#' @param project_cve A parsed cve edgelist by \code{\link{parse_commit_message_id_network}}.
#' @param nvd_feed  Parsed  nvdfeed by \code{\link{parse_nvdfeed}}.
#' @export
#' @family edgelists
parse_cve_cwe_file_network <- function(project_cve,nvd_feed){
  commit_message_id <- cwe_id <- name <- color <- src <- dest <- weight <- NULL # due to NSE notes in R CMD check

  cve_nodes <- project_cve[["nodes"]]
  cve_edgelist <- project_cve[["edgelist"]]
  # Find the edges from CVE ids to CWE ids
  cwe_edgelist <- merge(
    cve_edgelist,
    nvd_feed,
    by.x="commit_message_id",
    by.y = "cve_id",
    all.x = TRUE)[,.(commit_message_id,cwe_id)]
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
#' Transform parsed mbox into a network
#'
#' @param project_mbox A parsed mbox by \code{parse_mbox}.
#' @export
#' @family edgelists
parse_mbox_network <- function(project_mbox){
  data.From <- data.Subject <- data.Date <- NULL # due to NSE notes in R CMD check
  # Obtain the relevant columns - Author, E-mail Thread, and Timestamp
  project_mbox <- project_mbox[,.(author=data.From,thread=data.Subject,date=data.Date)]
  # Select relevant columns for nodes
  mbox_nodes <- c(unique(project_mbox$author),unique(project_mbox$thread))
  # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
  mbox_edgelist <- project_mbox[,.(weight=.N),by=c("author","thread")]
  # Color authors black, and e-mail threads lightblue
  mbox_nodes <- data.table(name=mbox_nodes,color=ifelse(mbox_nodes %in% mbox_edgelist$author,
                                                        "black",
                                                        "lightblue"))
  # bipartite graph
  mbox_nodes$type <- ifelse(mbox_nodes$name %in% mbox_edgelist$author,
                            TRUE,
                            FALSE)
  # Return the parsed JSON output as nodes and edgelist.
  mbox_network <- list()
  mbox_network[["nodes"]] <- mbox_nodes
  mbox_network[["edgelist"]] <- mbox_edgelist
  return(mbox_network)
}
#' Transform parsed git repo commit messages id and files into an edgelist
#'
#' @param project_git A parsed git project by \code{parse_gitlog}.
#' @param commit_message_id_regex the regex to extract the id from the commit message
#' @export
#' @family edgelists
parse_commit_message_id_network <- function(project_git, commit_message_id_regex){
  commit_message_id <- NULL # due to NSE notes in R CMD check
  # Extract the id according to the parameter regex
  project_git$commit_message_id <- data.table(stringi::stri_match_first_regex(project_git$data.message,
                                                                              pattern = commit_message_id_regex))

  # Keep only the edges which contain the commit message id

  project_git <- project_git[!is.na(commit_message_id),.(commit_message_id,
                                                         file)]
  # Select relevant columns for nodes
  git_nodes <- c(unique(project_git$commit_message_id),unique(project_git$file))
  # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
  git_edgelist <- project_git[,.(weight=.N),by=c("commit_message_id","file")]
  # Color nodes commit_message_id dark blue, and files yellow
  git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$commit_message_id,
                                                      "#0052cc",
                                                      "#f4dbb5"))
  # bipartite graph
  git_nodes$type <- ifelse(git_nodes$name %in% git_edgelist$commit_message_id,
                           TRUE,
                           FALSE)

  commit_message_id_network <- list()
  commit_message_id_network[["nodes"]] <- git_nodes
  commit_message_id_network[["edgelist"]] <- git_edgelist
  return(commit_message_id_network)

}
#' Transform parsed dependencies into a network
#'
#' @param depends_parsed A parsed mbox by \code{parse_dependencies}.
#' @param weight_types The weight types as defined in Depends.
#'
#' @export
#' @family edgelists
parse_dependencies_network <- function(depends_parsed,weight_types=NA){
  src <- dest <- weight <- NULL # due to NSE notes in R CMD check
  # Can only include types user wants if Depends found them at least once on codebase
  weight_types <- intersect(names(depends_parsed)[3:ncol(depends_parsed)],weight_types)
  dependency_edgelist <- depends_parsed[,.(src,dest)]
  if(any(is.na(weight_types))){
    dependency_edgelist$weight <- rowSums(depends_parsed[,3:ncol(depends_parsed),with=FALSE])
  }else{
    dependency_edgelist$weight <- rowSums(depends_parsed[,weight_types,with=FALSE])
  }
  # Remove dependencies not chosen by user
  dependency_edgelist <- dependency_edgelist[weight != 0]
  # Select relevant columns for nodes
  dependency_nodes <- unique(c(dependency_edgelist$src,dependency_edgelist$dest))
  # Color files yellow
  dependency_nodes <- data.table(name=dependency_nodes,color="#f4dbb5")
  # Return the parsed JSON output as nodes and edgelist.
  file_network <- list()
  file_network[["nodes"]] <- dependency_nodes
  file_network[["edgelist"]] <- dependency_edgelist
  return(file_network)
}
