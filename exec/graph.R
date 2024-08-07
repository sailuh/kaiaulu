#!/usr/local/bin/Rscript

# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


require(data.table,quietly=TRUE)
require(yaml,quietly=TRUE)
require(cli,quietly=TRUE)
require(docopt,quietly=TRUE)
require(kaiaulu,quietly=TRUE)
require(igraph,quietly=TRUE)



doc <- "
USAGE:
  graph.R bipartite help
  graph.R bipartite file <tools.yml> <project_conf.yml> <cli_conf.yml> <gitlog_file_name_path> <save_file_name_path>
  graph.R bipartite entity <tools.yml> <project_conf.yml> <cli_conf.yml> <gitlog_file_name_path> <save_file_name_path>
  graph.R temporal help
  graph.R temporal file <tools.yml> <project_conf.yml> <cli_conf.yml> <gitlog_file_name_path> <save_file_name_path>
  graph.R temporal entity <tools.yml> <project_conf.yml> <cli_conf.yml> <gitlog_file_name_path> <save_file_name_path>
  graph.R (-h | --help)
  graph.R --version

DESCRIPTION:
  Provides a suite of functions for network construction. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>. An additional <cli_conf.yml> is needed to
  specify network construction parameters such as the coice between
  entity or file network, mode for bipartite projection, number of
  lags, edge type and edge weight scheme. See kaiaulu_cli.yml for
  examples.


OPTIONS:
  -h --help         Show this screen.
  --version         Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["bipartite"]] & arguments[["help"]]){
  cli_alert_info("Creates a bipartite graph projection from a
                 parsed git (entity) log using
                 transform_gitlog_to_bipartite_network(),
                 transform_gitlog_to_entity_bipartite_network()
                 and bipartite_graph_projection().")
}else if(arguments[["bipartite"]] & arguments[["file"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  cli_path <- arguments[["<cli_conf.yml>"]]
  gitlog_path <- arguments[["<gitlog_file_name_path>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  cli <- yaml::read_yaml(cli_path)

  network_type <- cli[["graph"]][["bipartite"]][["file"]][["network_type"]]
  mode <- cli[["graph"]][["bipartite"]][["file"]][["mode"]]
  directed <- cli[["graph"]][["bipartite"]][["file"]][["directed"]]
  weight_scheme <- cli[["graph"]][["bipartite"]][["file"]][["weight_scheme"]]

  # Read git log
  project_git <- data.table::fread(gitlog_path)

  if (nrow(project_git) > 0){
    # Bipartite network
    bipartite_network <- transform_gitlog_to_bipartite_network(project_git,
                                                               mode = network_type)
    if (length(bipartite_network[["edgelist"]]) > 1){
      # Bipartite projection
      bipartite_projection <- bipartite_graph_projection(bipartite_network,
                                                         mode=mode,
                                                         weight_scheme_function=get(weight_scheme))

      # Save adjacency matrix
      graph_bipartite_projection <- igraph::graph_from_data_frame(d=bipartite_projection[["edgelist"]],
                                                              directed = directed,
                                                              vertices = bipartite_projection[["nodes"]])
      adjacency_matrix <- as_adjacency_matrix(graph_bipartite_projection,
                                              attr = "weight", sparse = F)
      adjacency_matrix <- as.data.frame(adjacency_matrix)
      rownames(adjacency_matrix) <- colnames(adjacency_matrix)

      data.table::fwrite(adjacency_matrix,save_path,row.names=T)
      cli_alert_success(paste0("Adjacency matrix for bipartite projection
                               was saved at: ",save_path))
    }else{
      cli_alert_warning(paste0("Egde list is empty."))
      write.csv(data.frame(),save_path)
    }
  }else{
    cli_alert_warning(paste0("Git log is empty."))
    write.csv(data.frame(),save_path)
  }
}else if(arguments[["bipartite"]] & arguments[["entity"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  cli_path <- arguments[["<cli_conf.yml>"]]
  gitlog_path <- arguments[["<gitlog_file_name_path>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  cli <- yaml::read_yaml(cli_path)

  network_type <- cli[["graph"]][["bipartite"]][["entity"]][["network_type"]]
  mode <- cli[["graph"]][["bipartite"]][["entity"]][["mode"]]
  directed <- cli[["graph"]][["bipartite"]][["entity"]][["directed"]]
  weight_scheme <- cli[["graph"]][["bipartite"]][["entity"]][["weight_scheme"]]

  # Read git log
  project_git <- data.table::fread(gitlog_path)

  if (nrow(project_git) > 0){
    # Bipartite network
    bipartite_network <- transform_gitlog_to_entity_bipartite_network(project_git,
                                                                      mode = network_type)
    if (length(bipartite_network[["edgelist"]]) > 1){
      # Bipartite projection
      bipartite_projection <- bipartite_graph_projection(bipartite_network,
                                                         mode=mode,
                                                         weight_scheme_function=get(weight_scheme))

      # Save adjacency matrix
      graph_bipartite_projection <- igraph::graph_from_data_frame(d=bipartite_projection[["edgelist"]],
                                                                  directed = directed,
                                                                  vertices = bipartite_projection[["nodes"]])
      adjacency_matrix <- as_adjacency_matrix(graph_bipartite_projection,
                                              attr = "weight", sparse = F)
      adjacency_matrix <- as.data.frame(adjacency_matrix)
      rownames(adjacency_matrix) <- colnames(adjacency_matrix)

      data.table::fwrite(adjacency_matrix,save_path,row.names=T)
      cli_alert_success(paste0("Adjacency matrix for bipartite projection
                               was saved at: ",save_path))
    }else{
      cli_alert_warning(paste0("Egde list is empty."))
      write.csv(data.frame(),save_path)
    }
  }else{
    cli_alert_warning(paste0("Git log is empty."))
    write.csv(data.frame(),save_path)
  }
}else if(arguments[["temporal"]] & arguments[["help"]]){
  cli_alert_info("Creates a temporal collaboration network from a
                 parsed git (entity) log using
                 transform_gitlog_to_temporal_network() and
                 transform_gitlog_to_entity_temporal_network().")
}else if(arguments[["temporal"]] & arguments[["file"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  cli_path <- arguments[["<cli_conf.yml>"]]
  gitlog_path <- arguments[["<gitlog_file_name_path>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  cli <- yaml::read_yaml(cli_path)

  network_type <- cli[["graph"]][["temporal"]][["file"]][["network_type"]]
  mode <- cli[["graph"]][["temporal"]][["file"]][["mode"]]
  directed <- cli[["graph"]][["temporal"]][["file"]][["directed"]]
  lag <- cli[["graph"]][["temporal"]][["file"]][["lag"]]
  weight_scheme <- cli[["graph"]][["temporal"]][["file"]][["weight_scheme"]]

  # Read git log
  project_git <- data.table::fread(gitlog_path)

  if (nrow(project_git) > 0){
    # Temporal network
    temporal_network <- transform_gitlog_to_temporal_network(project_git,
                                                             mode = mode, lag = lag,
                                                             weight_scheme_function = get(weight_scheme))

    if (length(temporal_network[["edgelist"]]) > 1){
    # Save adjacency matrix
    graph_temporal_network <- igraph::graph_from_data_frame(d=temporal_network[["edgelist"]],
                                                            directed = directed,
                                                            vertices = temporal_network[["nodes"]])
    adjacency_matrix <- as_adjacency_matrix(graph_temporal_network,
                                            attr = "weight", sparse = F)
    adjacency_matrix <- as.data.frame(adjacency_matrix)
    rownames(adjacency_matrix) <- colnames(adjacency_matrix)

    data.table::fwrite(adjacency_matrix,save_path,row.names=T)

    cli_alert_success(paste0("Adjacency matrix for temporal network was
                             saved at: ",save_path))
    }else{
      cli_alert_warning(paste0("Egde list is empty."))
      write.csv(data.frame(),save_path)
    }
  }else{
    cli_alert_warning(paste0("Git log is empty."))
    write.csv(data.frame(),save_path)
  }
}else if(arguments[["temporal"]] & arguments[["entity"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  cli_path <- arguments[["<cli_conf.yml>"]]
  gitlog_path <- arguments[["<gitlog_file_name_path>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  cli <- yaml::read_yaml(cli_path)

  network_type <- cli[["graph"]][["temporal"]][["entity"]][["network_type"]]
  mode <- cli[["graph"]][["temporal"]][["entity"]][["mode"]]
  directed <- cli[["graph"]][["temporal"]][["entity"]][["directed"]]
  lag <- cli[["graph"]][["temporal"]][["entity"]][["lag"]]
  weight_scheme <- cli[["graph"]][["temporal"]][["entity"]][["weight_scheme"]]

  # Read git log
  project_git <- data.table::fread(gitlog_path)

  if (nrow(project_git) > 0){
    # Temporal network
    temporal_network <- transform_gitlog_to_entity_temporal_network(project_git,
                                                                    mode = mode, lag = lag,
                                                                    weight_scheme_function = get(weight_scheme))

    if (length(temporal_network[["edgelist"]]) > 1){
      # Save adjacency matrix
      graph_temporal_network <- igraph::graph_from_data_frame(d=temporal_network[["edgelist"]],
                                                              directed = directed,
                                                              vertices = temporal_network[["nodes"]])
      adjacency_matrix <- as_adjacency_matrix(graph_temporal_network,
                                              attr = "weight", sparse = F)
      adjacency_matrix <- as.data.frame(adjacency_matrix)
      rownames(adjacency_matrix) <- colnames(adjacency_matrix)

      data.table::fwrite(adjacency_matrix,save_path,row.names=T)

      cli_alert_success(paste0("Adjacency matrix for temporal network was
                             saved at: ",save_path))
    }else{
      cli_alert_warning(paste0("Egde list is empty."))
      write.csv(data.frame(),save_path)
    }
  }else{
    cli_alert_warning(paste0("Git log is empty."))
    write.csv(data.frame(),save_path)
  }
}
