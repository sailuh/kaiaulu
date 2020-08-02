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
#' Parse the git blame message of a file
#'
#' Create a data.table with the blame data of each line of a file in a specific commit.
#'
#' @param git_repo_path git_repo_path path to git repo (ends in .git)
#' @param commit_hash a commit hash which indicates the specific version of the file (the commit must exist in `git_log`)
#' @return a data.table which contains blame commits for each line of a file and metadata of the commits.
#' @param file_path the filepath to the file which will be blamed
#' @export
parse_git_blame <- function(git_repo_path,commit_hash,file_path){
  parse_lines_content <- function(lines_content){
    n_lines_content <- length(lines_content)
    parsed_lines <- list(
      commit_hash = NA_character_,
      line_n_original_file = NA_character_,
      line_n_final_file = NA_character_,
      author_name = NA_character_,
      author_email = NA_character_,
      author_timestamp = NA_character_,
      author_tz = NA_character_,
      committer_name = NA_character_,
      committer_email = NA_character_,
      committer_timestamp = NA_character_,
      committer_tz = NA_character_,
      committer_summary = NA_character_,
      previous_commit_hash = NA_character_,
      previous_file = NA_character_,
      filename = NA_character_,
      content = NA_character_
    )
    # Case 1. No metadata, (starts with 'commit hash'
    # followed by 'line content' -- total 2 lines)
    if(n_lines_content == 2){
      commit_line <- regex_git_blame_commit_line(lines_content[1])
      parsed_lines[["commit_hash"]] <- commit_line[2]
      parsed_lines[["line_n_original_file"]] <- commit_line[3]
      parsed_lines[["line_n_final_file"]] <- commit_line[4]
      parsed_lines[["content"]] <- lines_content[2]
    }else if(n_lines_content == 4){
      commit_line <- regex_git_blame_commit_line(lines_content[1])
      parsed_lines[["commit_hash"]] <- commit_line[2]
      previous_line <- regex_git_blame_previous_line(lines_content[2])
      parsed_lines[["previous_commit_hash"]] = previous_line[2]
      parsed_lines[["previous_file"]] = previous_line[3]
      parsed_lines[["filename"]] = regex_git_blame_filename_line(lines_content[3])[2]
      parsed_lines[["content"]] = lines_content[4]
      # All lines immediately followed by a commit line and before the next commit line are one of 3 kinds:
      # Case 2. Lines Metadata (starts with 'commit hash'
      # ends with 'previous','filename',and 'line content' -- total of 13 lines)
      # lines_content <- blame_content[1:13]
    }else if(n_lines_content == 13){
      commit_line <- regex_git_blame_commit_line(lines_content[1])
      previous_line <- regex_git_blame_previous_line(lines_content[11])
      parsed_lines[["commit_hash"]] = commit_line[2]
      parsed_lines[["line_n_original_file"]] = commit_line[3]
      parsed_lines[["line_n_final_file"]] = commit_line[4]
      parsed_lines[["author_name"]] = regex_git_blame_author_name_line(lines_content[2])[2]
      parsed_lines[["author_email"]] = regex_git_blame_author_email_line(lines_content[3])[2]
      parsed_lines[["author_timestamp"]] = regex_git_blame_author_time_line(lines_content[4])[2]
      parsed_lines[["author_tz"]] = regex_git_blame_author_tz_line(lines_content[5])[2]
      parsed_lines[["committer_name"]] = regex_git_blame_committer_name_line(lines_content[6])[2]
      parsed_lines[["committer_email"]] = regex_git_blame_committer_email_line(lines_content[7])[2]
      parsed_lines[["committer_timestamp"]] = regex_git_blame_committer_time_line(lines_content[8])[2]
      parsed_lines[["committer_tz"]] = regex_git_blame_committer_tz_line(lines_content[9])[2]
      parsed_lines[["committer_summary"]] = regex_git_blame_summary_line(lines_content[10])[2]
      parsed_lines[["previous_commit_hash"]] = previous_line[2]
      parsed_lines[["previous_file"]] = previous_line[3]
      parsed_lines[["filename"]] = regex_git_blame_filename_line(lines_content[12])[2]
      parsed_lines[["content"]] = lines_content[13]
      # Case 3. Lines Metadata (starts with 'commit hash'
      # ends with 'summary','filename', and 'line content' (no 'previous') -- total 12 lines)
      # lines_content <- blame_content[27:38]
    }else if(n_lines_content == 12){
      commit_line <- regex_git_blame_commit_line(lines_content[1])
      parsed_lines[["commit_hash"]] = commit_line[2]
      parsed_lines[["line_n_original_file"]] = commit_line[3]
      parsed_lines[["line_n_final_file"]] = commit_line[4]
      parsed_lines[["author_name"]] = regex_git_blame_author_name_line(lines_content[2])[2]
      parsed_lines[["author_email"]] = regex_git_blame_author_email_line(lines_content[3])[2]
      parsed_lines[["author_timestamp"]] = regex_git_blame_author_time_line(lines_content[4])[2]
      parsed_lines[["author_tz"]] = regex_git_blame_author_tz_line(lines_content[5])[2]
      parsed_lines[["committer_name"]] = regex_git_blame_committer_name_line(lines_content[6])[2]
      parsed_lines[["committer_email"]] = regex_git_blame_committer_email_line(lines_content[7])[2]
      parsed_lines[["committer_timestamp"]] = regex_git_blame_committer_time_line(lines_content[8])[2]
      parsed_lines[["committer_tz"]] = regex_git_blame_committer_tz_line(lines_content[9])[2]
      parsed_lines[["committer_summary"]] = regex_git_blame_summary_line(lines_content[10])[2]
      parsed_lines[["filename"]] = regex_git_blame_filename_line(lines_content[11])[2]
      parsed_lines[["content"]] = lines_content[12]
    }else{
      stop(stri_c("Do not know how to parse case with number of lines: ",
                  length(lines_content)," commit hash: ",lines_content[1]))
    }
    return(parsed_lines)
  }

  # Call function git_blame to obtain the blame message into blame_file
  blame_content <- git_blame(git_repo_path,
                             flags=c('-p','-C','-w','-M'),
                             commit_hash,
                             file_path)
  # Only commit or file lines are succeeded by content lines
  # Parse all lines which are commit hashes - Only capture first 2 digits, 3rd is inconsistent
  parsed_commit <- data.table(regex_git_blame_commit_line(blame_content))
  setnames(parsed_commit,
           old=c("V1","V2","V3","V4"),
           new = c("raw_line","commit_hash","line_n_original_file","line_n_final_file"))
  parsed_commit[,is_commit_line := !is.na(raw_line)]
  non_parsed_lines_index <- which(is.na(parsed_commit$raw_line))
  parsed_commit[non_parsed_lines_index,
                raw_line := blame_content[non_parsed_lines_index]]
  parsed_commit[,commit_hash_id := cumsum(is_commit_line)]
  parsed_commit <- parsed_commit[,parse_lines_content(raw_line),by="commit_hash_id"]
  mapping <- parsed_commit[!is.na(author_name),
                           .(commit_hash,
                             author_name,
                             author_email,
                             author_timestamp,
                             author_tz,
                             committer_name,
                             committer_email,
                             committer_timestamp,
                             committer_tz,
                             committer_summary)]
  parsed_commit <- parsed_commit[,.(commit_hash,line_n_original_file,line_n_final_file,previous_commit_hash,content)]
  parsed_commit[mapping, on = .(commit_hash), names(mapping) := mget(paste0("i.", names(mapping)))][]
  return(parsed_commit)
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
#' @param filepath path to file
#' @export
parse_line_type_file <- function(utags_path,filepath,kinds){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  utags_path <- path.expand(utags_path)
  filepath <- path.expand(filepath)
  language <- stri_trans_tolower(last(stri_split_regex(filepath,"\\.")[[1]]))
  # Entity Kinds e.g. (function, class, etc) are specified by user.
  file_kinds <- kinds[[language]]
  # Specify fields of uctags output this function will parse and show to user:
  # n = start line
  # e = end line
  # k = entity kind specified as single letter (i.e. 'f','c', etc)
  fields <- c("n","e","k")
  stdout <- system2(
    command = utags_path,
    args = c(
      stri_c("--fields=",stri_c(fields, collapse = "")),
      stri_c("--kinds-", language,"=",stri_c(file_kinds,collapse=""), collapse =""),
      '-f','-',filepath),
    stdout = TRUE,
    stderr = FALSE
  )
  parsed_tags <- data.table(
    stri_match_first_regex(stdout,
                           pattern = '^(\\S+)\\t(\\S+)\\t/\\^(.+)\\$?\\/;\"\\t(\\w)\\tline:(\\d+)\\tend:(\\d+)')
  )
  setnames(parsed_tags,
           c("raw_ctags","entity_name","filepath","line_content","entity_type","line_start","line_end"))

  parsed_tags[]
  return(parsed_tags)
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
