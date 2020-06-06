# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Parse gitlog from Perceval
#'
#' @param perceval_path path to perceval binary
#' @param git_repo_path path to git repo (ends in .git)
#' @param save_path optional save path for .rds object
#' @export
#' @family parsers
parse_gitlog <- function(perceval_path,git_repo_path,save_path=NA){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  perceval_path <- path.expand(perceval_path)
  git_repo_path <- path.expand(git_repo_path)
  save_path <- ifelse(!is.na(save_path),path.expand(save_path),NA)

  # Use percerval to parse .git --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- system2(perceval_path,
                             args = c('git',git_repo_path,'--json-line'),
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
#' Transform parsed git repo into an edgelist
#'
#' @param project_git A parsed git project by \code{parse_gitlog}.
#' @param mode The network of interest: author-file, or commit-file
#' @export
#' @family edgelists
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
  }
  git_network <- list()
  git_network[["nodes"]] <- git_nodes
  git_network[["edgelist"]] <- git_edgelist
  return(git_network)

}
#' Parse mbox from Perceval
#'
#' @param perceval_path path to perceval binary
#' @param mbox_path path to mbox archive file (ends in .mbox)
#' @export
#' @family parsers
parse_mbox <- function(perceval_path,mbox_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  perceval_path <- path.expand(perceval_path)
  mbox_path <- path.expand(mbox_path)
  # Remove ".mbox"
  mbox_uri <- stri_replace_last(mbox_path,replacement="",regex=".mbox")
  # Use percerval to parse mbox_path. --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- system2(perceval_path,
                             args = c('mbox',mbox_uri,mbox_path,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)
  # Parsed JSON output as a data.table.
  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose=FALSE))
  # Parse timestamps and convert to UTC
  perceval_parsed$data.Date <- as.POSIXct(perceval_parsed$data.Date,
                                                format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC")
  return(perceval_parsed)
}
#' Transform parsed mbox into a network
#'
#' @param project_mbox A parsed mbox by \code{parse_mbox}.
#' @export
#' @family edgelists
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
#' Parse dependencies from Depends
#'
#' @param depends_jar_path path to depends jar
#' @param git_repo_path path to git repo (ends in .git)
#' @param language the language of the .git repo (accepts cpp, java, ruby, python, pom)
#' @export
#' @family parsers
parse_dependencies <- function(depends_jar_path,git_repo_path,language){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  depends_jar_path <- path.expand(depends_jar_path)
  git_repo_path <- path.expand(git_repo_path)
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
#' Transform parsed dependencies into a network
#'
#' @param project_mbox A parsed mbox by \code{parse_dependencies}.
#' @param weight_types The weight types as defined in Depends.
#'
#' @export
#' @family edgelists
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
#' Filter commit files by extension
#'
#' Filters a data.table containing filepaths using the specified extensions
#'
#' @param dt_file any data.table with a named column `file` containing filepaths
#' @param extension a character vector of extensions (e.g. c(py,java)) to *keep*
#' in the table
#' @return a data.table which contains only filepaths with the specified extensions
#' @export
#' @family {filters}
#' @seealso \code{\link{parse_gitlog}} and \code{\link{parse_dependencies}} to create dt_file
filter_by_file_extension <- function(dt_file,extension){
  file_extension_re <- stri_c('[.](',stri_c(extension,collapse="|"),')$')
  is_file_with_extension <- stri_detect_regex(dt_file$file,file_extension_re)
  return(dt_file[is_file_with_extension])
}
#' Filter by filepath substring
#'
#' Filters a data.table with filepaths using the specified substring (e.g. remove
#' all filepaths which contain the word 'test' anywhere in it)
#'
#' @param dt_file any data.table with a named column `file` with filepaths
#' @param substring a character vector of substrings (e.g. c(py,java)) we wish to *filter*
#' @return a data.table which contains does *not* contain filepaths with the specified words
#' @export
#' @family filters
#' @seealso \code{\link{parse_gitlog}} and \code{\link{parse_dependencies}} to create dt_file
filter_by_filepath_substring <- function(dt_file,substring){
  file_contains_re <- stri_c('(',stri_c(substring,collapse="|"),')')
  is_not_filepath_with_substring <- !stri_detect_regex(dt_file$file,file_contains_re)
  return(dt_file[is_not_filepath_with_substring])
}
#' Filter by commit interval
#'
#' Filters a data.table by with author or commit datetime using the specified start and end commits
#'
#' @param git_log any data.table with a named column `data.AuthorDate` with datetime in POSIXct
#' @param start_commit a commit hash which indicates the start of interval (the commit must exist in `git_log`)
#' @param end_commit a commit hash which indicates the end of interval (the commit must exist in `git_log`)
#' @return a data.table which contains only commits within `start_commit` and `end_commit`
#' @export
#' @family filters
#' @seealso \code{\link{parse_gitlog}} to create git_log
filter_by_commit_interval <- function(git_log,start_commit,end_commit){
  start_date <- get_date_from_commit_hash(git_log,start_commit)
  end_date <- get_date_from_commit_hash(git_log,end_commit)
  git_log <- git_log[data.AuthorDate >= start_date & data.AuthorDate <= end_date]
  return(git_log)
}
# Various imports
#' @importFrom magrittr %>%
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
