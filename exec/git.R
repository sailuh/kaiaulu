#!/usr/local/bin/Rscript

# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


require(yaml,quietly=TRUE)
require(cli,quietly=TRUE)
require(docopt,quietly=TRUE)
require(kaiaulu,quietly=TRUE)
require(data.table,quietly=TRUE)



doc <- "
USAGE:
  git.R file_changes help
  git.R file_changes <tools.yml> <project_conf.yml> <save_file_name_path>
  git.R entity_changes help
  git.R entity_changes <tools.yml> <project_conf.yml> <save_file_name_path>
  git.R network_file_co_change help
  git.R network_file_co_change <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path>
  git.R network_entity_co_change help
  git.R network_entity_co_change <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path>
  git.R network_file_authors help
  git.R network_file_authors <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path>
  git.R network_entity_authors help
  git.R network_entity_authors <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path>
  git.R (-h | --help)
  git.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Git. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["file_changes"]] & arguments[["help"]]){
  cli_alert_info("Outputs a git log to save_file_name_path using parse_gitlog().")
}else if(arguments[["file_changes"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  cli_alert_success(paste0("Git log was saved at: ",save_path))

  data.table::fwrite(project_git,save_path)
}

if(arguments[["entity_changes"]] & arguments[["help"]]){
  cli_alert_info("Outputs log of changed entities to save_file_name_path using parse_gitlog_entity. An entity is a function, class, or method in R.")
}else if(arguments[["entity_changes"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  utags_path <- get_tool_project("utags", tool)
  kinds <- get_uctags_line_types(conf)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  changed_entities <- parse_gitlog_entity(git_repo_path,utags_path,project_git,kinds,progress_bar = TRUE)

  cli_alert_success(paste0("Changed entities log was saved at: ",save_path))

  data.table::fwrite(changed_entities,save_path)
}

if(arguments[["network_entity_co_change"]] & arguments[["help"]]){
  cli_alert_info("Outputs csv of nodes and csv of edges of an entity co-change network")
}else if(arguments[["network_entity_co_change"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  node_path <- arguments[["<node_save_file_name_path>"]]
  edge_path <- arguments[["<edge_save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  utags_path <- get_tool_project("utags", tool)
  kinds <- get_uctags_line_types(conf)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  changed_entities <- parse_gitlog_entity(git_repo_path,utags_path,project_git,kinds,progress_bar = TRUE)

  project_log <- list(project_git=changed_entities)
  project_log <- identity_match(project_log,
                                name_column = c("author_name_email"),
                                assign_exact_identity,
                                use_name_only=TRUE,
                                label = "raw_name"
  )
  id_project_git <- project_log[["project_git"]]

  project_commit_network <- transform_gitlog_to_entity_bipartite_network(id_project_git,mode ="commit-entity")

  co_change_network <- bipartite_graph_projection(project_commit_network,
                                                  mode = FALSE,
                                                  weight_scheme_function = weight_scheme_sum_edges)

  data.table::fwrite(co_change_network$nodes, node_path)
  data.table::fwrite(co_change_network$edgelist, edge_path)

  cli_alert_success("Entity co-change network saved:")
  cli_alert_info(paste("Nodes saved to:", node_path))
  cli_alert_info(paste("Edges saved to:", edge_path))
}

if(arguments[["network_file_co_change"]] & arguments[["help"]]){
  cli_alert_info("Outputs csv of nodes and csv of edges of a file co-change network")
}else if(arguments[["network_file_co_change"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  node_path <- arguments[["<node_save_file_name_path>"]]
  edge_path <- arguments[["<edge_save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  project_commit_network <- transform_gitlog_to_bipartite_network(project_git,
                                                                  mode="commit-file")

  co_change_network <- bipartite_graph_projection(project_commit_network,
                                                  mode = FALSE,
                                                  weight_scheme_function = weight_scheme_sum_edges)

  data.table::fwrite(co_change_network$nodes, node_path)
  data.table::fwrite(co_change_network$edgelist, edge_path)

  cli_alert_success("File co-change network saved:")
  cli_alert_info(paste("Nodes saved to:", node_path))
  cli_alert_info(paste("Edges saved to:", edge_path))
}

if(arguments[["network_file_authors"]] & arguments[["help"]]){
  cli_alert_info("Outputs csv of nodes and csv of edges of an author file co-change network")
}else if(arguments[["network_file_authors"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  node_path <- arguments[["<node_save_file_name_path>"]]
  edge_path <- arguments[["<edge_save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  project_collaboration_network <- transform_gitlog_to_bipartite_network(project_git,
                                                                         mode="author-file")

  author_network <- bipartite_graph_projection(project_collaboration_network,
                                               mode = TRUE,
                                               weight_scheme_function = weight_scheme_sum_edges)

  data.table::fwrite(author_network$nodes, node_path)
  data.table::fwrite(author_network$edgelist, edge_path)

  cli_alert_success("File author co-change network saved:")
  cli_alert_info(paste("Nodes saved to:", node_path))
  cli_alert_info(paste("Edges saved to:", edge_path))
}

if(arguments[["network_entity_authors"]] & arguments[["help"]]){
  cli_alert_info("Outputs csv of nodes and csv of edges of an author entity co-change network")
}else if(arguments[["network_entity_authors"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  node_path <- arguments[["<node_save_file_name_path>"]]
  edge_path <- arguments[["<edge_save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  utags_path <- get_tool_project("utags", tool)
  kinds <- get_uctags_line_types(conf)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  changed_entities <- parse_gitlog_entity(git_repo_path,utags_path,project_git,kinds,progress_bar = TRUE)

  project_log <- list(project_git=changed_entities)
  project_log <- identity_match(project_log,
                                name_column = c("author_name_email"),
                                assign_exact_identity,
                                use_name_only=TRUE,
                                label = "raw_name"
  )
  id_project_git <- project_log[["project_git"]]

  project_collaboration_network <- transform_gitlog_to_entity_bipartite_network(id_project_git,
                                                                                mode = "author-entity")

  author_network <- bipartite_graph_projection(project_collaboration_network,
                                               mode = TRUE,
                                               weight_scheme_function = weight_scheme_sum_edges)

  data.table::fwrite(author_network$nodes, node_path)
  data.table::fwrite(author_network$edgelist, edge_path)

  cli_alert_success("Entity author co-change network saved:")
  cli_alert_info(paste("Nodes saved to:", node_path))
  cli_alert_info(paste("Edges saved to:", edge_path))
}
