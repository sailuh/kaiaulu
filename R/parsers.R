# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
    # Color nodes authors black, and files yellow
    git_nodes <- data.table(name=git_nodes,color=ifelse(git_nodes %in% git_edgelist$author,
                                                        "black",
                                                        "#f4dbb5"))
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
  }
  git_network <- list()
  git_network[["nodes"]] <- git_nodes
  git_network[["edgelist"]] <- git_edgelist
  return(git_network)

}
#' @export
parse_mbox <- function(perceval_path,mbox_path){
  # Remove ".mbox"
  mbox_uri <- stri_split_regex(mbox_path,pattern=".mbox")[[1]][1]
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
#' @export
parse_dependencies <- function(depends_jar_path,git_repo_path,language){
  # Remove ".git"
  folder_path <- stri_replace_last(git_repo_path,replacement="",regex=".git")
  project_name <- stri_split_regex(folder_path,pattern="/")[[1]]
  project_name <- project_name[length(project_name)-1]
  # Use Depends to parse the code folder.
  system2("java",
                             args = c("-jar",depends_jar_path,
                                      language,folder_path,
                                      project_name,'--dir=/tmp/',
                                      '--auto-include',
                                      '--granularity=file', '--namepattern=/',
                                      '--format=json'),
                             stdout = FALSE,
                             stderr = FALSE)
  # Construct /tmp/ file path
  output_path <- stri_c("/tmp/",project_name,".json")
  # Parsed JSON output.
  depends_parsed <- jsonlite::read_json(output_path)
  # The JSON has two main parts. The first is a vector of all file names.
  file_names <- unlist(depends_parsed[["variables"]])
  # /Users/user/git_repos/APR/xml/apr_xml_xmllite.c => "xml/apr_xml_xmllite.c"
  file_names <- stri_replace_first(file_names,replacement="",regex=folder_path)
  # The second part is the dependencies itself, which refer to the file name indices.
  dependencies <- depends_parsed[["cells"]]
  # The types of dependencies is a list of lists. First we unlist the various types.
  dependencies_types <- rbindlist(lapply(dependencies,
                                  function(x) as.data.table(x$values)),
                           fill=TRUE)
  # Fixes column types to numeric, and replace NAs by 0s, as an NA means 0 dependencies.
  dependencies_types <- data.table(sapply(dependencies_types,as.numeric))
  dependencies_types[is.na(dependencies_types)] <- 0
  # Then we unlist the src and dest files.
  dependencies_files <- rbindlist(lapply(dependencies,
                                         function(x) as.data.table(x[c("src","dest")])),
                                  fill=TRUE)
  # And finally we combine them
  depends_parsed <- cbind(dependencies_files,dependencies_types)
  # We use the file_names to re-label the files for further analysis
  # Note the +1: The json assumes a file index starts at 0. R index starts 1, hence the + 1.
  depends_parsed$src <- file_names[depends_parsed$src + 1]
  depends_parsed$dest <- file_names[depends_parsed$dest + 1]

  return(depends_parsed)
}

# Available weight_types: See the columns available from parse_dependencies.
# depends_parsed must be the output of parse_dependencies().
#' @export
parse_dependencies_network <- function(depends_parsed,weight_types=NA){
  dependency_edgelist <- depends_parsed[,.(src,dest)]
  if(is.na(weight_types)){
    dependency_edgelist$weight <- rowSums(depends_parsed[,3:ncol(depends_parsed),with=FALSE])
  }else{
    dependency_edgelist$weight <- rowSums(depends_parsed[,c(weight_types),with=FALSE])
  }
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

# Various imports
#' @importFrom stringi stri_replace_last
#' @importFrom stringi stri_replace_first
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
