# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Create a dsm.json file given either a dependencies table from \code{\link{transform_dependencies_to_sdsmj}}
#' or a gitlog table from \code{\link{transform_gitlog_to_hdsmj}}.
#'
#' # Returns either a hdsm.json (from dependencies table) or a sdsmj.json (from gitlog table).
#'
#' @param graph the table containing the nodes and edgelist of the graph of dependencies or the gitlog.
#' @param is_sorted whether to sort the variables (filenames) in the dsm.json files
#' @export
#' @family dv8
#' @seealso \code{\link{transform_dependencies_to_sdsmj}} and \code{\link{transform_gitlog_to_hdsmj}}
#'
graph_to_dsmj <- function(graph, is_sorted=FALSE){

  # Get the nodes and edgelist tables
  nodes_table <- graph[[1]]
  edgelist_table <- graph[[2]]

  # Get and sort the file names
  variables <- sort(unique(nodes_table[["name"]]), method="radix")

  # Make indices for the file names
  variables_indices <- 1:length(variables)
  # Make named list with file names as keys and indices as values
  names(variables_indices) <- variables

  # List to hold cells
  cells_indices <- 1:nrow(edgelist_table)

  # Get parameters that will be used for the cells
  parameters <- list("Call", "Import", "Use", "Parameter", "Contain", "Create", "Extend",
                     "Return","Implement", "Cast", "Throw", "Annotation", "Cochange")
  # Only use parameters in the dataframe
  parameters <- unlist(intersect(colnames(edgelist_table), parameters))

  getCell <- function(i) {
    src <- edgelist_table[["from"]][[i]]
    dest <- edgelist_table[["to"]][[i]]

    src_index <- variables_indices[src] - 1
    dest_index <- variables_indices[dest] - 1

    values <- list()
    # Get all the parameter values for a specific cell
    for (j in 1: length(parameters)) {
      current_param <- edgelist_table[i][[parameters[[j]]]]
      if (current_param > 0){
        values[[parameters[[j]]]] <- current_param
      }
    }
    return(list(src=src_index, dest=dest_index, values=data.frame(values)))
  }

  # Sorted list
  cells <- lapply(cells_indices, getCell)
  cells_df <- data.table::data.table(jsonlite::fromJSON(jsonlite::toJSON(cells, auto_unbox = TRUE)))

  if (is_sorted == TRUE){
    data.table::setorder(cells_df, cols = "src", "dest")
  }

  # Create the final json
  hdsm_json <- list(schemaVersion="1.0", name=paste0("april16-graph", "-hdsm"), variables=variables, cells=cells_df)

  json_df <- jsonlite::fromJSON(jsonlite::toJSON(hdsm_json, auto_unbox = TRUE))

  # Do this only if it's for Cochange?
  # Unbox each of the cell values (should be values: object, not values: [object])
  json_df$cells$values <- lapply(json_df$cells$values, jsonlite::unbox)

  # Save the json to a file
  jsonlite::write_json(json_df, paste0("../rawdata/dv8/", "april16-depends-helix-graph", ".json"), auto_unbox=TRUE)
}

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

#' Apply a bipartite graph projection
#'
#' @param graph A bipartite network (see transform_*_bipartite functions).
#' @param mode Which of the two nodes the projection should be applied to. TRUE or FALSE
#' @param weight_scheme_function When not specified, bipartite_graph_projection will return an intermediate
#' projection table specifying the deleted node, from_projection, to_projection, from_weight, and to_weight.
#' This table also contains (N choose 2) rows for every "deleted_node" in the original graph.
#' When receiving as parameter \code{\link{weight_scheme_sum_edges}} or
#' \code{\link{weight_scheme_count_deleted_nodes}}, the final projection table will be returned instead.
#' @return A graph projection.
#' @export
bipartite_graph_projection <- function(graph,mode,weight_scheme_function = NULL){

  get_combinations <- function(edgelist){
    dt <- edgelist

#    print(colnames(dt))
    # Decide projection base on column available
    if("from" %in% colnames(dt)){
      #from <- unique(dt$from)
    }else{
      # since in the projection only either "to" or "from" connections will exist, relabel both as "from"
      setnames(dt,
               c("to"),
               c("from"))
      #from <- unique(dt$to)
    }
    from <- unique(dt$from)
    # If projection of isolated node, there is nothing to connect it to
    if(length(from) < 2){
      combinations <- data.table(NA_character_,NA_character_)
    }else{
      combinations <- transpose(as.data.table(combn(from,
                                                    2,
                                                    simplify=FALSE)))
    }

    setnames(combinations,
             old = c("V1","V2"),
             new = c("from_projection","to_projection"))

    # add the weight contributions before the projection deletes the node
    combinations <- merge(combinations,dt,all.x=TRUE,by.x = "from_projection", by.y="from")
    setnames(combinations,
             c("weight"),
             c("from_weight"))
    combinations <- merge(combinations,dt,all.x=TRUE,by.x = "to_projection", by.y="from")
    setnames(combinations,
             c("weight"),
             c("to_weight"))

    #combinations$weight <- combinations$from_weight + combinations$to_weight

    return(combinations)
  }


  # Filter the nodes we wish to keep in the projection
  graph[["nodes"]] <- graph[["nodes"]][type == mode]


  if(mode){

    # Calculate N Choose 2 combinations for every deleted node
    graph[["edgelist"]] <- graph[["edgelist"]][, get_combinations(.SD),
                                          by = c("to"),
                                          .SDcols = c("from","weight")]

    setnames(x = graph[["edgelist"]],
             old = c("to"),
             new = c("eliminated_node"))

  }else{

    # Calculate N Choose 2 combinations for every deleted node
    graph[["edgelist"]] <- graph[["edgelist"]][, get_combinations(.SD),
                                          by = c("from"),
                                          .SDcols = c("to","weight")]

    setnames(x = graph[["edgelist"]],
             old = c("from"),
             new = c("eliminated_node"))

  }

  # Remove from edgelist the nodes that do not connect to others (e.g. a single file commit)
  graph[["edgelist"]] <- graph[["edgelist"]][complete.cases(graph[["edgelist"]])]


  # Do not aggregate weights if no weight scheme is specified
  if(is.null(weight_scheme_function)){
    return(graph)
  }else{
    return(weight_scheme_function(graph))
  }


}

#' Weight Edge Sum Projection Scheme
#'
#' This weight scheme sums the deleted node adjacent edges when re-wired
#' in the projection graph. For most cases, this is the commonly used
#' approach in the literature.
#'
#' For example assume author A changes file1 three times, and
#' author B changes file1 one time. A projection which returns
#' a author-author network using this function would return,
#' assuming this being the entire collaboration graph,
#' that author A and author B have a connection of 3 + 1 = 4.
#'
#' Suppose now also that the entire collaboration graph contains
#' another file2 such that author A and author B modified 2 and 3
#' times respectively. Hence in the projection step, this would
#' further contribute a 2 + 3 = 5 weight to authors A and B. In this
#' graph, where file1 and file2 are present, the final weight for
#' authors A and B would thus be (3 + 1) + (2 + 3) = 9.
#'
#' @param projected_graph A semi-processed bipartite projection network resulting
#' @export
#' @family weight_scheme
#' from \code{\link{bipartite_graph_projection}} when specifying
#' weight_scheme_function = NA.
weight_scheme_sum_edges <- function(projected_graph){
  projected_graph[["edgelist"]]$weight <- projected_graph[["edgelist"]]$from_weight + projected_graph[["edgelist"]]$to_weight
  projected_graph[["edgelist"]] <- projected_graph[["edgelist"]][,.(weight=sum(weight)),by=c("from_projection","to_projection")]
  setnames(x = projected_graph[["edgelist"]],
           old = c("from_projection","to_projection"),
           new = c("from","to"))
  return(projected_graph)
}

#' Weight Number of Deleted Nodes Projection Scheme
#'
#' This weight scheme counts the number of deleted nodes for
#' every pair of adjacent edges, and uses it as the projection
#' edge weight.
#'
#' For example, assume a graph which contains commit A and
#' commit B. Additionally, assume file1, file2, and file3 to
#' connect to commit A. And file2 and file3 to connect to
#' commit B.
#'
#' If we wish to know the co-change metric, i.e. how many
#' times each pair of file were modified in the same commit,
#' then we may obtain it by counting the number of deleted nodes.
#'
#' In this example the edges  (file1,file2), (file2,file3), and
#' (file1,file3) receive a contribution of weight 1 from the
#' deleted commit A, and (file2,file3) receive a contribution of
#' weight 1 from the deleted commit B.
#'
#' Thus, the weights would be (file1,file2) = 1, (file2,file3) = 2,
#' and (file2,file3) = 1. In other words, in this example file2
#' and file3 have a co-change of 2, and the remaining pairs a
#' co-change of 1.
#'
#' @param projected_graph A semi-processed bipartite projection network resulting
#' from \code{\link{bipartite_graph_projection}} when specifying
#' weight_scheme_function = NA.
#' @export
#' @family weight_scheme
weight_scheme_count_deleted_nodes <- function(projected_graph){
  projected_graph[["edgelist"]] <- projected_graph[["edgelist"]][,.(weight=length(eliminated_node)),by=c("from_projection","to_projection")]
  setnames(x = projected_graph[["edgelist"]],
           old = c("from_projection","to_projection"),
           new = c("from","to"))
  return(projected_graph)
}

#' OSLOM Community Detection
#'
#' @description Wrapper for OSLOM Community Detection \url{http://oslom.org/}
#' @param oslom_bin_dir_undir_path The path to oslom directed or undirected network binary
#' @param graph The graph to be clustered obtained from transform_* functions
#' @param seed An integer specifying the seed to replicate the result
#' @param n_runs the number of runs for the first hierarchical level
#' @param is_weighted a boolean indicating whether a weight column is available in the data.table
#' @references Finding statistically significant communities in networks
#' A. Lancichinetti, F. Radicchi, J.J. Ramasco and
#' S. Fortunato PLoS ONE 6, e18961 (2011).
#' @export
#' @family community
community_oslom <- function(oslom_bin_dir_undir_path,graph,seed,n_runs,is_weighted){

  edgelist <- graph[["edgelist"]]

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

  # assign a unique and different from existing cluster id to all nodes which have no neighbors
  isolated_node_ids <- setdiff(graph[["nodes"]]$name,cluster[["assignment"]]$node_id)

  if(length(isolated_node_ids) > 0){
    isolated_nodes_cluster_ids <- max(as.numeric(cluster[["assignment"]]$cluster_id)) + 1
    isolated_nodes_cluster_ids <- as.character(seq.int(from=isolated_nodes_cluster_ids,
                                                       length.out = length(isolated_node_ids)))
    isolated_cluster_assignments <- data.table(node_id=isolated_node_ids,
                                               cluster_id=isolated_nodes_cluster_ids)

    cluster[["assignment"]] <- rbind(cluster[["assignment"]],
                                     isolated_cluster_assignments)

    isolated_cluster_infos <- data.table(cluster_id=isolated_nodes_cluster_ids,
                                         cluster_size=1,
                                         cluster_pvalue=NA)

    cluster[["info"]] <- rbind(cluster[["info"]],
                               isolated_cluster_infos)
  }

  # Indexes starting cluster id to 1 instead of 0 to align with R indexes
  cluster[["assignment"]]$cluster_id <- as.character(as.numeric(cluster[["assignment"]]$cluster_id) + 1)
  cluster[["info"]]$cluster_id <- as.character(as.numeric(cluster[["info"]]$cluster_id) + 1)

  return(cluster)
}

#' Re-color OSLOM Community IDs
#'
#' @description Re-color a graph color column using the OSLOM communities
#' @param network A network returned by transform_to_network_* functions.
#' @param community A community detection returned by \code{community_oslom}.
#' @references Finding statistically significant communities in networks
#' A. Lancichinetti, F. Radicchi, J.J. Ramasco and
#' S. Fortunato PLoS ONE 6, e18961 (2011).
#' @export
#' @family community
recolor_network_by_community <- function(network,community){

  # If node is repeated, it is a boundary community node, color it a unique color
  assign_boundary_color <- function(x){
    ifelse(length(x) > 1,"black",x)
  }

  color_pallete <- RColorBrewer::brewer.pal(n = 12,name = "Paired")

  node_cid_mapping <- community[["assignment"]]
  node_cid_mapping$color_community <- color_pallete[as.integer(node_cid_mapping$cluster_id)]
  # Use cluster id as index to choose color
  metadata_nodes_cid <- merge(network[["nodes"]],
                              node_cid_mapping,
                              by.x = "name",
                              by.y ="node_id",
                              all.x = TRUE) # there should be no node not assigned a cluster


  if(!is.null(metadata_nodes_cid[["type"]])){

    # Choose color of clustering instead of previous scheme
    metadata_nodes_cid <- metadata_nodes_cid[,.(name,color=color_community,type)]
    metadata_nodes_cid <- metadata_nodes_cid[,.(color=assign_boundary_color(color),type),
                                             ,by=c("name")]
  }else{
    metadata_nodes_cid <- metadata_nodes_cid[,.(name,color=color_community)]
    metadata_nodes_cid <- metadata_nodes_cid[,.(color=assign_boundary_color(color)),
                                             ,by=c("name")]
  }


  metadata_nodes_cid <- unique(metadata_nodes_cid)

  network[["nodes"]] <- metadata_nodes_cid

  return(network)

}
