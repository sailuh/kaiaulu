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
  git.R file_network help
  git.R file_network <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path> (--author-file | --committer-file | --commit-file | --author-committer)
  git.R entity_network help
  git.R entity_network <tools.yml> <project_conf.yml> <node_save_file_name_path> <edge_save_file_name_path> (--author-entity | --committer-entity | --commit-entity | --author-committer)
  git.R (-h | --help)
  git.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Git. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.

COMMANDS:
   file_changes                 Outputs a git log using parse_gitlog().
   entity_changes               Outputs log of changed entities using parse_gitlog_entity(). An entity is a function, class, or method in R.
   file_network                 Outputs a csv of nodes and csv of edges of a network made using the selected mode.
   entity_network               Outputs a csv of nodes and csv of edges of a network made using the selected mode.

ARGUMENTS:
  <tools.yml>                   path to tools.yml file
  <project_conf.yml>            path to configuration file for project you want to analyze
  <save_file_name_path>         file path where output will be saved
  <node_save_file_name_path>    file path where csv of nodes of the network will be saved
  <edge_save_file_name_path>    file path where csv of edges of the network will be saved

OPTIONS:
  -h --help                     Show this screen.
  --version                     Show version.
  --author-file                 Mode that outputs which authors edited which files
  --author-entity               Mode that outputs which authors edited which entities
  --committer-file              Mode that outputs which committers edited which files
  --committer-entity            Mode that outputs which committers edited which entities
  --commit-file                 Mode that outputs which files were edited in each commit
  --commit-entity               Mode that outputs which entities were edited in each commit
  --author-committer            Mode that outputs which authors made changes with which committers
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

if(arguments[["entity_network"]] & arguments[["help"]]){
  cli_alert_info("Outputs csv of nodes and csv of edges of an entity network made using the selected mode. Use git.R --help for mode descriptions.")
}else if(arguments[["entity_network"]]){

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
  # Determine mode
  if(arguments[["--commit-entity"]]){
    mode <- "commit-entity"
    project_commit_network <- transform_gitlog_to_entity_bipartite_network(id_project_git, mode=mode)
    projection_mode <- FALSE

  } else if(arguments[["--author-entity"]]){
    mode <- "author-entity"
    project_commit_network <- transform_gitlog_to_entity_bipartite_network(id_project_git, mode=mode)
    projection_mode <- TRUE

  } else if(arguments[["--committer-entity"]]){
    mode <- "committer-entity"
    project_commit_network <- transform_gitlog_to_entity_bipartite_network(id_project_git, mode=mode)
    projection_mode <- TRUE

  } else if(arguments[["--author-committer"]]){
    mode <- "author-committer"
    project_commit_network <- transform_gitlog_to_entity_bipartite_network(id_project_git, mode=mode)
    projection_mode <- TRUE
  }

  entity_network <- bipartite_graph_projection(project_commit_network,
                                                  mode = projection_mode,
                                                  weight_scheme_function = weight_scheme_sum_edges)

  data.table::fwrite(entity_network$nodes, node_path)
  data.table::fwrite(entity_network$edgelist, edge_path)

  cli_alert_success(paste(mode, " network saved:"))
  cli_alert_info(paste("Nodes saved to:", node_path))
  cli_alert_info(paste("Edges saved to:", edge_path))
}

if(arguments[["file_network"]] & arguments[["help"]]){
  cli_alert_info("Outputs csv of nodes and csv of edges of a file co-change network made using the selected mode. Use git.R --help for mode descriptions.")
}else if(arguments[["file_network"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  node_path <- arguments[["<node_save_file_name_path>"]]
  edge_path <- arguments[["<edge_save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  # Determine mode
  if(arguments[["--commit-file"]]){
    mode <- "commit-file"
    project_commit_network <- transform_gitlog_to_bipartite_network(project_git, mode=mode)
    projection_mode <- FALSE

  } else if(arguments[["--author-file"]]){
    mode <- "author-file"
    project_commit_network <- transform_gitlog_to_bipartite_network(project_git, mode=mode)
    projection_mode <- TRUE

  } else if(arguments[["--committer-file"]]){
    mode <- "committer-file"
    project_commit_network <- transform_gitlog_to_bipartite_network(project_git, mode=mode)
    projection_mode <- TRUE

  } else if(arguments[["--author-committer"]]){
    mode <- "author-committer"
    project_commit_network <- transform_gitlog_to_bipartite_network(project_git, mode=mode)
    projection_mode <- TRUE
  }

  file_network <- bipartite_graph_projection(project_commit_network,
                                                  mode = projection_mode,
                                                  weight_scheme_function = weight_scheme_sum_edges)

  data.table::fwrite(file_network$nodes, node_path)
  data.table::fwrite(file_network$edgelist, edge_path)

  cli_alert_success(paste(mode, "network saved:"))
  cli_alert_info(paste("Nodes saved to:", node_path))
  cli_alert_info(paste("Edges saved to:", edge_path))
}
