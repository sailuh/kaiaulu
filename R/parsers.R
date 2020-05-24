parse_gitlog <- function(perceval_path,git_repo_path){
  # Remove ".git"
  git_uri <- str_split(git_repo_path,pattern=".git")[[1]][1]
  # The log will be saved to the /tmp/ folder
  gitlog_path <- "/tmp/gitlog.log"
  # Execute shell command to extract gitlog using Percerval recommended format (See it's README.md.
  perceval_output <- system2("git",
                             args = c('--git-dir',
                                      git_repo_path,
                                      'log',
                                      '--raw',
                                      '--numstat',
                                      '--pretty=fuller',
                                      '--decorate=full',
                                      '--parents',
                                      '--reverse',
                                      '--topo-order',
                                      '-M',
                                      '-C',
                                      '-c',
                                      '--remotes=origin',
                                      '--all',
                                      '>' ,
                                      gitlog_path),
                             stdout = TRUE,
                             stderr = FALSE)
  # Use percerval to parse gitlog_path. --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- system2(perceval_path,
                             args = c('git', '--git-log',gitlog_path,git_uri,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)
  # Return the parsed JSON output.
  return(data.table(jsonlite::stream_in(textConnection(perceval_output))))
}
parse_mbox <- function(perceval_path,mbox_path){
  # Remove ".mbox"
  mbox_uri <- str_split(git_repo_path,pattern=".mbox")[[1]][1]
  # Use percerval to parse mbox_path. --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- system2(perceval_path,
                             args = c('mbox',mbox_uri,mbox_path,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)
  # Return the parsed JSON output as a data.table.
  return(data.table(jsonlite::stream_in(textConnection(perceval_output))))
}
parse_mbox_igraph <- function(project_mbox){
  # Obtain the relevant columns - Author, E-mail Thread, and Timestamp
  mbox_edgelist <- project_mbox[,.(author=data.From,thread=data.Subject,date=data.Date)]
  # Parse igraph from edgelist
  mbox_network <- igraph::graph_from_data_frame(mbox_edgelist,directed=TRUE)
  # Color authors black, and e-mail threads lightblue
  igraph::V(mbox_network)$color <- ifelse(igraph::V(mbox_network)$name %in% mbox_edgelist$author,
                                          "black",
                                          "lightblue")
  # Return the parsed JSON output as an igraph object.
  return(mbox_network)
}
