# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Parse gitlog from Perceval
#'
#' @param perceval_path path to perceval binary
#' @param git_repo_path path to git repo (ends in .git)
#' @param save_path optional save path for .rds object
#' @param from_date a string of the form "YYYY-MM-DD"
#' @param to_date a string of the form "YYYY-MM-DD"
#' @export
#' @family parsers
parse_gitlog <- function(perceval_path,git_repo_path,save_path=NA,perl_regex=NA){
  data.files <- data.Author <- data.AuthorDate <- data.commit <- data.Commit <- data.CommitDate <- data.message <- NULL # due to NSE notes in R CMD check
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  perceval_path <- path.expand(perceval_path)
  git_repo_path <- path.expand(git_repo_path)
  git_uri <-  git_repo_path
  save_path <- ifelse(!is.na(save_path),path.expand(save_path),NA)

  # Use percerval to parse .git --json line is required to be parsed by jsonlite::fromJSON.
  # The log will be saved to the /tmp/ folder
  gitlog_path <- "/tmp/gitlog.log"

  # Perceval suggested flags
  perceval_flags <-
    c(
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
      '--remotes=origin'
    )
  # Execute shell command to extract gitlog using Percerval recommended format (See it's README.md.
  if(!is.na(perl_regex)){
    flags <- c('--no-merges',
               'master',
               stri_c('--grep=', '"', perl_regex, '"'),
               '--perl-regexp',
               perceval_flags)
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
  }else{
    flags <- c(perceval_flags,'--all')
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
  }

  # Parsed JSON output.
  perceval_output <- system2(perceval_path,
                             args = c('git', '--git-log',gitlog_path,git_uri,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)

  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose = FALSE))

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
                                                                                       data.CommitDate,
                                                                                       data.message)]
  # Parsing gitlog can take awhile, save if a path is provided
  if(!is.na(save_path)){
    saveRDS(perceval_parsed,save_path)
  }
  return(perceval_parsed)
}
#' Adds a column commit_message_id containing the parsed commit message
#'
#' @param project_git A parsed git project by \code{parse_gitlog}.
#' @param commit_message_id_regex the regex to extract the id from the commit message
#' @export
#' @family parsers
parse_commit_message_id <- function(project_git, commit_message_id_regex){
  commit_message_id <- NULL # due to NSE notes in R CMD check
  # Extract the id according to the parameter regex
  project_git$commit_message_id$commit_message_id <- stringi::stri_match_first_regex(project_git$data.message,
                                                                                     pattern = commit_message_id_regex)

  return(project_git)
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
#' Parse NVD Feed CVEs, descriptions and CWE ids
#'
#' @param nvdfeed_folder_path Folderpath for nvd feed files
#' under schema 1.1 (e.g. nvdcve-1.1-2018.json)
#' @export
#' @family parsers
parse_nvdfeed <- function(nvdfeed_folder_path){
  folder_path <- path.expand(nvdfeed_folder_path)
  all_files_path <- list.files(folder_path,full.names = TRUE)

  parse_single_feed <- function(nvd_feed_json){
    all_cves <- nvd_feed_json[["CVE_Items"]]
    n_cves <- nvd_feed_json[["CVE_data_numberOfCVEs"]]
    cve_list <- vector(mode = "list", length = n_cves)
    for (i in 1:as.integer(n_cves)){
      cve <- all_cves[[i]][["cve"]]
      cve_id <- cve[["CVE_data_meta"]][["ID"]]
      cwe_metadata <- cve[["problemtype"]][["problemtype_data"]][[1]][["description"]]
      # can be missing if CVE is rejected. See CVE-1999-0020 as example on 2002 feed file.
      if(length(cwe_metadata) > 0){
        cwe_id <- cwe_metadata[[1]][["value"]]
      }else{
        cwe_id <- NA
      }
      cve_description <- cve[["description"]][["description_data"]][[1]][["value"]]
      cve_list[[i]] <- data.table(cve_id,cwe_id,cve_description)
    }
    return(rbindlist(cve_list))
  }
  n_nvdfeeds <- length(all_files_path)
  cve_list <- vector(mode = "list", length = n_nvdfeeds)
  for(i in 1:n_nvdfeeds){
    nvd_json <- jsonlite::read_json(all_files_path[i])
    cve_list[[i]] <- parse_single_feed(nvd_json)
  }
  return(rbindlist(cve_list))
}
#' Parse Java Code Refactorings
#'
#' @param rminer_path The path to RMiner binary.
#'  See \url{https://github.com/tsantalis/RefactoringMiner#running-refactoringminer-from-the-command-line}
#' @param git_repo_path path to git repo (ends in .git)
#' @param start_commit the start commit hash
#' @param end_commit the end commit hash
#' @export
#' @references Nikolaos Tsantalis, Matin Mansouri, Laleh Eshkevari,
#' Davood Mazinanian, and Danny Dig, "Accurate and Efficient Refactoring
#' Detection in Commit History," 40th
#' International Conference on Software Engineering (ICSE 2018),
#' Gothenburg, Sweden, May 27 - June 3, 2018.
parse_java_code_refactoring_json <- function(rminer_path,git_repo_path,start_commit,end_commit){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  rminer_path <- path.expand(rminer_path)
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  git_uri <- stri_replace_last(git_repo_path,replacement="",regex=".git")
  # Use percerval to parse mbox_path. --json line is required to be parsed by jsonlite::fromJSON.
  rminer_output <- system2(rminer_path,
                             args = c('-bc',git_uri,start_commit,end_commit),
                             stdout = TRUE,
                             stderr = FALSE)
  # Parsed JSON output as a data.table.
  rminer_parsed <- jsonlite::parse_json(rminer_output)
  return(rminer_parsed)
}
#' Parse File Line Metrics
#'
#' @param scc_path The path to scc binary.
#'  See \url{https://github.com/boyter/scc}
#' @param git_repo_path path to git repo (ends in .git)
#' @export
parse_line_metrics <- function(scc_path,git_repo_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  scc_path <- path.expand(scc_path)
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  folder_path <- stri_replace_last(git_repo_path,replacement="",regex=".git")
  # Use Depends to parse the code folder.
  stdout <- system2(
    scc_path,
    args = c(folder_path, '--by-file','--format','csv'),
    stdout = TRUE,
    stderr = FALSE
  )
  line_metrics <- fread(stri_c(stdout,collapse = "\n"))
  # /Users/user/git_repos/APR/xml/apr_xml_xmllite.c => "xml/apr_xml_xmllite.c"
  line_metrics$Location <- stri_replace_first(line_metrics$Location,
                                              replacement="",
                                              regex=folder_path)
  return(line_metrics)
}
#' Parse File Line Type
#'
#' @param utags_path The path to scc binary.
#'  See \url{https://github.com/universal-ctags/ctags}
#' @param git_repo_path path to git repo (ends in .git)
#' @export
parse_line_type <- function(utags_path,git_repo_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  utags_path <- path.expand(utags_path)
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  folder_path <- stri_replace_last(git_repo_path,replacement="",regex=".git")
  # Use Depends to parse the code folder.
  stdout <- system2(
    utags_path,
    args = c('-f','-','-x','-R',folder_path),
    stdout = TRUE,
    stderr = FALSE
  )
  line_types <- fread(stri_c(stdout,collapse = "\n"),sep="",
                      strip.white=TRUE,
                      header=FALSE)
  line_types <- rbindlist(lapply(stri_match_all(stdout,regex="(\\w+)[\\s]+(\\w+)[\\s]+(\\d+)[\\s]+(\\S+)[\\s]+(.+)",simplify = TRUE),data.table))
  colnames(line_types) <- c("raw_utag","token","line_type","line_number","file_path","line_content")

  # /Users/user/git_repos/APR/xml/apr_xml_xmllite.c => "xml/apr_xml_xmllite.c"
  line_types$file_path <- stri_replace_first(line_types$file_path,
                                              replacement="",
                                              regex=folder_path)
  return(line_types)
}

# Various imports
utils::globalVariables(c("."))
#' @importFrom magrittr %>%
#' @importFrom stringi stri_replace_last
#' @importFrom stringi stri_replace_first
#' @importFrom stringi stri_match_all
#' @importFrom stringi stri_match_first_regex
#' @importFrom stringi stri_detect_regex
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table .N
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
NULL
