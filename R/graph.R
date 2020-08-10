# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Create a directed graph model
#'
#' @param edgelist a 2-column data.table containing named columns
#' `from` and `to` in any order.
#' @param is_bipartite boolean specifying if network is bipartite: TRUE or FALSE
#' @param color a character vector of length 1 or 2 specifying the hexacolor
#' @return a named list list(nodes,edgelist).
#' @export
model_directed_graph <- function(edgelist,is_bipartite,color){
  # Select relevant columns for nodes
  nodes <- data.table(name=unique(c(edgelist$from,edgelist$to)))

  edgelist <- edgelist[,.(weight=.N),by=c("from","to")]

  if(is_bipartite){
    nodes$type <- ifelse(nodes$name %in% edgelist$from,
                             TRUE,
                             FALSE)
    nodes$color <- ifelse(nodes$name %in% edgelist$from,
                          color[1],
                          color[2])
  }else{
    nodes$type <- FALSE
    nodes$color <- color[1]
  }
  graph <- list(nodes=nodes,edgelist=edgelist)
  class(graph) <- c("directed_graph",class(graph))

  return(graph)
}

#' OSLOM Community Detection
#'
#' @description Wrapper for OSLOM Community Detection \url{http://oslom.org/}
#' @param oslom_bin_dir_undir_path The path to oslom dirrected or undirected network binary
#' @param edgelist An igraph edgelist object
#' @param seed An integer specifying the seed to replicate the result
#' @param n_runs the number of runs for the ﬁrst hierarchical level
#' @param is_weighted a boolean indicating wether a weight column is available in the data.table
#' @references Finding statistically significant communities in networks
#' A. Lancichinetti, F. Radicchi, J.J. Ramasco and
#' S. Fortunato PLoS ONE 6, e18961 (2011).
#' @export
community_oslom <- function(oslom_bin_dir_undir_path,edgelist,seed,n_runs,is_weighted){
  oslom_bin_dir_undir_path <- path.expand(oslom_bin_dir_undir_path)
  mapping_names <- unique(c(as.character(edgelist$from),edgelist$to))
  mapping_ids <- 1:length(mapping_names)
  names(mapping_ids) <- mapping_names
  weight_flag <- ""

  if(is_weighted){
    oslom_edgelist <- data.table(mapping_ids[edgelist$from],
                                 mapping_ids[edgelist$to],
                                 edgelist$weight)
    weight_flag <- "-w"
  }else{
    oslom_edgelist <- data.table(mapping_ids[edgelist$from],
                                 mapping_ids[edgelist$to])
  }


  fwrite(oslom_edgelist,"/tmp/oslom_edgelist.txt",
         sep = "\t",
         row.names = FALSE,
         col.names = FALSE)

  system2(
    oslom_bin_dir_undir_path,
    args = c('-f', '/tmp/oslom_edgelist.txt',weight_flag,'-seed',seed, '-r',n_runs),
    stdout = FALSE,
    stderr = FALSE
  )

  f_con <- file("/tmp/oslom_edgelist.txt_oslo_files/tp")
  tp_raw <- readLines(f_con)
  close(f_con)
  cluster_metadata <- stri_match(tp_raw,regex="#module (.+) size: (.+) bs: (.+)")

  is_node_line <- which(is.na(cluster_metadata[,1]))
  is_cluster_line <- which(!is.na(cluster_metadata[,1]))

  cluster_id <- cluster_metadata[is_cluster_line,2]
  cluster_size <- as.integer(cluster_metadata[is_cluster_line,3])
  cluster_pvalue <- cluster_metadata[is_cluster_line,4]
  cluster_nodes <- tp_raw[is_node_line]

  cluster_nodes_list <- stri_split(cluster_nodes,regex=" ")
  names(cluster_nodes_list) <- cluster_id

  # Remove "" strings added as ending element, create data.table and assign ids
  prepare_data_table <- function(name,x){
    dt <- data.table(node_id=x[[name]][x[[name]] != ""],cluster_id=name)
    return(dt)
  }
  cluster_assignment <- rbindlist(lapply(names(cluster_nodes_list),
                                         prepare_data_table,
                                         cluster_nodes_list))

  # Replace temporary id by original values
  cluster_assignment$node_id <- mapping_names[as.numeric(cluster_assignment$node_id)]
  cluster <- list()
  cluster[["assignment"]] <- cluster_assignment
  cluster[["info"]] <- data.table(cluster_id,cluster_size,cluster_pvalue)
  return(cluster)
}
