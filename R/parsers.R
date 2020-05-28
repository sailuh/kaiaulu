#' @export
parse_gitlog <- function(perceval_path,git_repo_path,save_path=NA){
  # Remove ".git"
  git_uri <- stri_split_regex(git_repo_path,pattern=".git")[[1]][1]
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
  # Parsed JSON output.
  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose=FALSE))

  # Parse timestamps and convert to UTC
  perceval_parsed$data.AuthorDate <- as.POSIXct(perceval_parsed$data.AuthorDate,
                                                format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")
  perceval_parsed$data.CommitDate <- as.POSIXct(perceval_parsed$data.CommitDate,
                                                format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")

  # APR very first commit is a weird single case of commit without files. We filter them here.
  is_commit_with_files <- !!sapply(perceval_parsed$data.files,length)
  perceval_parsed <- perceval_parsed[is_commit_with_files]
  # Column data.files is a data.table. Unlist, so perceval_parsed is a table instead of a table of tables.
  perceval_parsed <- perceval_parsed[, .(file=unlist(data.files[[1]]$file),
                                         added=unlist(data.files[[1]]$added),
                                         removed=unlist(data.files[[1]]$removed)),, by = list(data.Author,
                                                                                       data.AuthorDate,
                                                                                       data.commit,
                                                                                       data.Commit,
                                                                                       data.CommitDate)]
  # Parsing gitlog can take awhile, save if a path is provided
  if(!is.na(save_path)){
    saveRDS(perceval_parsed,save_path)
  }
  return(perceval_parsed)
}
#' @export
parse_gitlog_network <- function(project_git, mode = c("author","commit")){
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
    # Color nodes authors black, and e-mail threads lightblue
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$author,
                                                        "black",
                                                        "#f4dbb5"))
  }else if(mode == "commit"){
    # Select relevant columns for nodes
    git_nodes <- c(unique(project_git$commit_hash),unique(project_git$file))
    # Select relevant columns for edgelist, grouping repeated rows as the edgelist weights
    git_edgelist <- project_git[,.(weight=.N),by=c("commit_hash","file")]
    # Color authors black, and e-mail threads lightblue
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$commit_hash,
                                                          "#afe569",
                                                          "#f4dbb5"))
    # This undirected graph is also bipartite
    git_nodes$type <-  ifelse(git_nodes$name %in% git_edgelist$commit_hash,
                                           TRUE,
                                           FALSE)
  }
  git_network <- list()
  git_network[["nodes"]] <- git_nodes
  git_network[["edgelist"]] <- git_edgelist
  return(git_network)

}
#' @export
parse_mbox <- function(perceval_path,mbox_path){
  # Remove ".mbox"
  mbox_uri <- stri_split_regex(git_repo_path,pattern=".mbox")[[1]][1]
  # Use percerval to parse mbox_path. --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- system2(perceval_path,
                             args = c('mbox',mbox_uri,mbox_path,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)
  # Parsed JSON output as a data.table.
  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose=FALSE))
  return(perceval_parsed)
}
#' @export
parse_mbox_network <- function(project_mbox){
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
  # Return the parsed JSON output as nodes and edgelist.
  mbox_network <- list()
  mbox_network[["nodes"]] <- mbox_nodes
  mbox_network[["edgelist"]] <- mbox_edgelist
  return(mbox_network)
}

# Various imports
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
NULL
