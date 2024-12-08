# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Parsers ##############

#' Parse Gitlog from Perceval
#'
#' Parses the `.git` file in a github repository using the Perceval library.
#'
#' @param perceval_path path to perceval binary
#' @param git_repo_path path to git repo (ends in .git)
#' @param save_path optional save path for .rds object
#' @param perl_regex a regex to filter git log entries using git's algorithm (efficient to return small datasets from large projects such as commit annotated issues)
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
      '-c'
    )
  # Execute shell command to extract gitlog using Percerval recommended format (See it's README.md).
  if(!is.na(perl_regex)){
    flags <- c('--no-merges',
               'master',
               stringi::stri_c('--grep=', '"', perl_regex, '"'),
               '--perl-regexp',
               perceval_flags)
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
    if(is.null(gitlog_call_message)){
      stop(stringi::stri_c("Unable to generate git log from this repository.",
                           " Perhaps the path specified was incorrect or the repository has no commits?"))
    }
  }else{
    flags <- perceval_flags
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
    if(is.null(gitlog_call_message)){
      stop(stringi::stri_c("Unable to generate git log from this repository.",
                           " Perhaps the path specified was incorrect or the repository has no commits?"))
    }
  }

  # Parsed JSON output.
  perceval_output <- system2(perceval_path,
                             args = c('git', '--git-log',gitlog_path,git_uri,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)

  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose = FALSE))

  if(nrow(perceval_parsed) == 0){
    stop("The repository specified has no commits.")
  }

  # APR very first commit is a weird single case of commit without files. We filter them here.
  is_commit_with_files <- !!sapply(perceval_parsed$data.files,length)
  perceval_parsed <- perceval_parsed[is_commit_with_files]
  # Column data.files is a data.table. Unlist, so perceval_parsed is a table instead of a table of tables.

  # Only when a file is renamed, Perceval will add a field "newfile". Normalize the list so every
  # element contain newfile, so the subsequent step can correctly tabulate "newfile" field.
  add_new_files_to_table <- function(data_files_row){
    commit_change_table <- data.table(data_files_row)

    # In some cases for APR (e.g. 873ca8616235529ceb222c8dd428c2d0e23824b6 and
    # b43bbf82e946864de970bf792c5d0907ae142dba, Perceval can only parse added, file  and removed.
    # If a file is modified, Perceval will include a #newfiles field, leading to a total of 7 fields.
    # To be safe, fill with NA any column that is missing.
    if(!("action" %in% colnames(commit_change_table))) commit_change_table$action <- NA_character_
    if(!("added" %in% colnames(commit_change_table))) commit_change_table$added <- NA_character_
    if(!("indexes" %in% colnames(commit_change_table))) commit_change_table$indexes <- NA_character_
    if(!("modes" %in% colnames(commit_change_table))) commit_change_table$modes <- NA_character_
    if(!("newfile" %in% colnames(commit_change_table))) commit_change_table$newfile <- NA_character_
    if(!("removed" %in% colnames(commit_change_table))) commit_change_table$removed <- NA_character_

    commit_change_table <- commit_change_table[,.(action,
                                                  added,
                                                  file,
                                                  indexes,
                                                  modes,
                                                  newfile,
                                                  removed)]
    return(commit_change_table)
  }
  perceval_parsed$data.files <- lapply(perceval_parsed$data.files,add_new_files_to_table)

  perceval_parsed <- perceval_parsed[, .(file=unlist(data.files[[1]]$file),
                                         added=unlist(data.files[[1]]$added),
                                         removed=unlist(data.files[[1]]$removed),
                                         newfile=unlist(data.files[[1]]$newfile)),, by = list(data.Author,
                                                                                              data.AuthorDate,
                                                                                              data.commit,
                                                                                              data.Commit,
                                                                                              data.CommitDate,
                                                                                              data.message)]


  setnames(perceval_parsed,
           c("data.Author","data.AuthorDate","data.commit","data.Commit","data.CommitDate","data.message",
             "file","added","removed","newfile"),
           c("author_name_email","author_datetimetz","commit_hash","committer_name_email","committer_datetimetz",
             "commit_message","file_pathname","lines_added","lines_removed","file_pathname_renamed"))

  # When newfile is provided, replace "file" with "newfile"
  # This avoids situations where a file is renamed, and never again modified, to not be included
  # in the list of files
  perceval_parsed[!is.na(file_pathname_renamed)]$file_pathname <- perceval_parsed[!is.na(file_pathname_renamed)]$file_pathname_renamed

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
  project_git$commit_message_id$commit_message_id <- stringi::stri_match_first_regex(project_git$commit_message,
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
    }else if(n_lines_content == 3){
      commit_line <- regex_git_blame_commit_line(lines_content[1])
      parsed_lines[["commit_hash"]] <- commit_line[2]
      parsed_lines[["filename"]] = regex_git_blame_filename_line(lines_content[2])[2]
      parsed_lines[["content"]] = lines_content[3]
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
      stop(stri_c("Do not know how to parse git blame case with " ,
                  length(lines_content)," number of lines: ",
                  "\n\tCase occurs in commit hash: ",commit_hash,
                  "\n\tfile_path: ",file_path,
                  "\n\tline content starting in: ",lines_content[1]))
    }
    return(parsed_lines)
  }

  # Call function git_blame to obtain the blame message into blame_file
  blame_content <- git_blame(git_repo_path,
                             flags=c('-p','-C','-w','-M'),
                             commit_hash,
                             file_path)
  # If git blame fails, return NA
  if(any(is.null(blame_content))){return(NULL)}
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

#' Parse Git log entities by line additions
#'
#' @description Refines the parsed git log to include
#' information of what entities a developer changed
#' when performing a commit. Changed entities are obtained
#' by examining if a changed line is within the start and
#' end line of any of the available Universal Ctags types
#' specified in `kinds`.
#'
#' An entity is defined and detected by Universal Ctags
#' by language. The list of available `kinds` is
#' currently Classes ('c'), Functions ('f'), and
#' Methods ('m'), which can be specified
#' to the parameter `kinds` as follows:
#'
#' \code{list(
#' java=c('c','m'),
#' python=c('c','f'),
#' cpp=c('c','f'),
#' c=c('f')
#' )}
#'
#' For example,
#' if the kind is 'f', the output will be all line addition
#' changes to
#' functions per commit in the project. If the kind is
#' 'c', then all changes to classes per commit will be
#' provided.
#'
#' Any combination of types can be provided per
#' language, which will result in the output containing the
#' union of all changes per commit made by developers to these
#' entities. Note because Ctags assigns a type per line changed,
#' if a change is done to a method of a class, then the changed
#' line will be assigned only the method, and not both method
#' and class.
#'
#' The enumerated `kinds` will be used as needed, and therefore
#' it is fine to specify languages not included in the project
#' to save time.
#' However, files analyzed must have their language specified.
#' Therefore, ensure \code{filter_by_file_extension} is properly
#' used on the parameter `project_git_log`.
#' This decision is by design: `kinds` vary per language, and may
#' substantially impact the output of this function, affecting the
#' analysis. Therefore, no default settings are provided to encourage
#' both \code{filter_by_file_extension} and `kinds` parameters are properly documented in a project
#' configuration file to facilitate reproducibility.
#'
#' Other entity types will be added in a later version.
#'
#' Please note this function will blame every file in a git log
#' to parse the data. Even for a 200 MB project git log this can
#' take one or more hours. Also, because this function relies on
#' git blame, only line addition changes will be captured.
#' Line deletions will -not- be captured. For example, if a
#' developer removes a line of a function through a commit,
#' this data will not be available in this function output.
#'
#' See Joblin'17 Chapter 3.1.1.1 for background and
#' conceptual details.
#'
#' @param git_repo_path path to git repo (ends in .git)
#' @param project_git_log A parsed git project by \code{parse_gitlog}.
#' @param utags_path The path to utags binary.
#' @param kinds A named list of character vectors of the form:
#' list(extension_1 = c('type_i','type_j',...),
#' extension_2 = c('type_i','type_k')). See examples.
#' @param progress_bar a boolean specifying if a progress bar should be shown.
#'
#' @references Mitchell Joblin (2017). Structural
#' and Evolutionary Analysis of Developer Networks.
#' (Doctoral dissertation, University of Passau, Germany).
#'
#'@examples
#'\dontrun{
#' # Obtain additions only to functions
#' kinds <- list(
#' java = c('m'),
#' python = c('f'),
#' cpp = c('c', 'f'),
#' c = c('f')
#' # Parse Project Git Log
#' project_git_log <- parse_gitlog(perceval_path, git_repo_path)
#' # Filter Files
#' project_git_log <- project_git_log  %>%
#'   filter_by_file_extension(file_extensions, "file")  %>%
#'   filter_by_filepath_substring(substring_filepath, "file")
#' # Parse Function Additions
#' changed_functions <- parse_gitlog_entity(git_repo_path,
#'                                         utags_path,
#'                                         project_git_log,
#'                                         kinds)
#'}
#' @export
parse_gitlog_entity <- function(git_repo_path,utags_path,project_git_log,kinds,progress_bar = FALSE){
  blamed_git_log <- function(git_repo_path,utags_path,git_log_commit_hash,git_log_file_path){
    blamed_file <- parse_git_blame(git_repo_path,
                                   git_log_commit_hash,
                                   git_log_file_path)


    if (any(is.null(blamed_file))){return(NULL)}

    line_changes <- blamed_file[commit_hash == git_log_commit_hash]
    line_changes[,line_n_final_file:= as.integer(line_n_final_file)]

    extension <- stri_trans_tolower(last(stri_split_regex(git_log_file_path,"\\.")[[1]]))
    file_path <- make_temporary_file(blamed_file$content,extension = stri_c(".",extension,collapse=""))
    tags <- parse_line_type_file(utags_path,file_path,kinds)
    tags <- tags[complete.cases(tags)]
    unlink(file_path)

    tags[,c("line_start", "line_end") :=
           list(as.integer(line_start), as.integer(line_end))]
    setkeyv(tags,c("line_start","line_end"))
    # Which changed lines modified a line within an entity of interest?
    # Filter line changes unrelated and join the columns.
    line_changes_tagged <- line_changes[tags,
                                        .(x.commit_hash,
                                          i.entity_name,
                                          i.entity_type,
                                          x.line_n_final_file,
                                          i.line_start,
                                          i.line_end,
                                          author_name,
                                          author_email,
                                          author_timestamp,
                                          author_tz,
                                          committer_name,
                                          committer_email,
                                          committer_timestamp,
                                          committer_tz,
                                          committer_summary
                                        ),
                                        on = .(line_n_final_file >= line_start,
                                               line_n_final_file <= line_end),
                                        allow.cartesian=TRUE
    ][!is.na(x.commit_hash) & !duplicated(x.line_n_final_file)]
    setnames(line_changes_tagged,
             old=c("x.commit_hash",
                   "i.entity_name",
                   "i.entity_type",
                   "x.line_n_final_file",
                   "i.line_start",
                   "i.line_end"),
             new=c("commit_hash",
                   "entity_definition_name",
                   "entity_type",
                   "changed_line_number",
                   "entity_definition_line_start",
                   "entity_definition_line_end"))
    # At line granularity, a lot of data is generated.
    # We are only concerned if authors changed an entity of interest.
    # Hence we capture the n_lines_changed for an entity by the author
    # in a single commit in `n_lines_changed` and simplify the table:
    entity_changes <- line_changes_tagged[,.(n_lines_changed = length(changed_line_number))
                                          ,by=c("commit_hash",
                                                "entity_definition_name",
                                                "entity_type",
                                                "entity_definition_line_start",
                                                "entity_definition_line_end",
                                                "author_name",
                                                "author_email",
                                                "author_timestamp",
                                                "author_tz",
                                                "committer_name",
                                                "committer_email",
                                                "committer_timestamp",
                                                "committer_tz",
                                                "committer_summary")]


    # Adds additional columns for naming consistency with parse_gitlog()

    entity_changes[,c("author_name_email",
                      "author_datetimetz",
                      "committer_name_email",
                      "commit_hash",
                      "committer_datetimetz",
                      "entity",
                      "weight"):=list(stri_c(author_name,author_email,sep= " "),
                                      as.POSIXct(as.integer(author_timestamp),
                                                 origin="1970-01-01",tz = "UTC"),
                                      stri_c(committer_name,committer_email,sep= " "),
                                      commit_hash,
                                      as.POSIXct(as.integer(committer_timestamp),
                                                 origin="1970-01-01",tz = "UTC"),
                                      entity_definition_name,
                                      n_lines_changed
                      )]

    return(entity_changes)
  }
  nrow_project_git_log <- nrow(project_git_log)
  project_git_log[,row_id := seq_len(nrow_project_git_log)]
  setkey(project_git_log,row_id)

  if(progress_bar){
    progress_bar <- txtProgressBar(min = 0,
                                   max = nrow_project_git_log,
                                   style = 3)

    changed_entities <- project_git_log[,
                                        blamed_git_log(git_repo_path,
                                                       utags_path,
                                                       {
                                                         setTxtProgressBar(progress_bar, .GRP);
                                                         git_log_commit_hash=commit_hash
                                                       },
                                                       git_log_file_path=file_pathname),
                                        by = row_id]

  }else{
    changed_entities <- project_git_log[,
                                        blamed_git_log(git_repo_path,
                                                       utags_path,
                                                       git_log_commit_hash=commit_hash,
                                                       git_log_file_path=file_pathname),
                                        by = row_id]
  }

  return(changed_entities)
}

############## Git Cmd ##############

#' Performs a git checkout on specified repo
#'
#' @param commit_hash The commit hash the repo should be checkout
#' @param git_repo_path The git repo path
#' @param new_branch Boolean indicating if we want to create a new branch (default is FALSE)
#' @return Any error message generated by git
#' @export
git_checkout <- function(commit_hash,git_repo_path,new_branch = FALSE){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  folder_path <- stri_replace_last(git_repo_path,replacement="",regex=".git")

  # if new branch is TRUE, we add "-b" in args to create new branch, else is normal git checkout
  if(new_branch) {
    error <- system2('git',
                     args = c('--git-dir',
                              git_repo_path,
                              '--work-tree',
                              folder_path,
                              'checkout',
                              '-b',
                              commit_hash),
                     stdout = TRUE,
                     stderr = FALSE)
  } else {
    error <- system2('git',
                     args = c('--git-dir',
                              git_repo_path,
                              '--work-tree',
                              folder_path,
                              'checkout',
                              commit_hash),
                     stdout = TRUE,
                     stderr = FALSE)
  }
  return(error)
}
#' Gets the current commit hash head of the git repo
#'
#' @param git_repo_path The git repo path
#' @return A commit hash character
#' @export
git_head <- function(git_repo_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  git_repo_path <- path.expand(git_repo_path)
  head <- system2('git',
                  args = c('--git-dir',
                           git_repo_path,
                           'rev-parse',
                           'HEAD'),
                  stdout = TRUE,
                  stderr = FALSE)
  return(head)
}
#' Saves gitlog to a path
#'
#' Saves the `.git` of a github repository as a gitlog at the specified path
#'
#' @param git_repo_path The git repo path
#' @param flags Optional flags for git log command
#' @param save_path the filepath to save the file
#' @export
git_log <- function(git_repo_path,flags,save_path){
  out <- tryCatch(
    {
      # Main Execution
      system2(
        "git",
        args = c(
          '--git-dir',
          git_repo_path,
          'log',
          flags,
          '>' ,
          save_path
        ),
        stdout = TRUE,
        stderr = FALSE
      )
    },
    error = function(cond){
      #message(stringi::stri_c("An error ocurred when generating the git log from this repository.",
      #                        " Perhaps the path specified was incorrect or the repository has no commits?"))
      #message(cond)
      return(NULL)
    },
    warning = function(cond){
      #message(stringi::stri_c("A warning ocurred when generating the git log from this repository.",
      #" Perhaps the path specified was incorrect or the repository has no commits?"))
      #message(cond)
      return(NULL)
    }
  )
  return(out)
}
#' Git blame wrapper
#'
#' @param git_repo_path The git repo pat
#' @param flags Optional flags for git log command
#' @param commit_hash The commit hash of the file we will blame
#' @param file_path The file we will blame
#' @export
git_blame <- function(git_repo_path,flags,commit_hash,file_path){

  # Some commit hashes, like APR's project git 572
  # throws error 128 from git
  # 6154ab7b1e862927c90ae6afa4dc6c57ee657ceb

  # This example changes function signature and a line inside
  # https://github.com/apache/apr/commit/ffdad353ac4b4bc2868603338e8ca50db90923a8
  blamed_file <- tryCatch({
    system2(
      "git",
      args = c(
        '--git-dir',
        git_repo_path,
        'blame',
        flags,
        commit_hash,
        file_path
      ),
      stdout = TRUE,
      stderr = FALSE
    )
  },
  warning = function(e) {
    #message(e)
    return(NULL)
  })
  # Blamed file was deleted by the commit
  if(is.character(blamed_file) & length(blamed_file) == 0){return(NULL)}
  return(blamed_file)


}

#' Creates a sample git log with one commit
#'
#' This is a SetUp helper function for Kaiaulu unit tests
#' that manipulates git logs.
#'
#' A folder kaiaulu_sample is created in /tmp by default. A file,
#' hello.R with a single print is then added to the folder.
#' Git init is performed, the file is git add, and commit to
#' the git log.
#'
#'
#' @param folder_path An optional path to where the sample .git should be created.
#' @return The path to the sample .git file.
#' @export
#' @family {unittest}
git_create_sample_log <- function(folder_path="/tmp"){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  folder_path <- path.expand(folder_path)
  folder_path <- io_make_folder(folder_path, "kaiaulu_sample")

  file_path <- file.path(folder_path,"hello.R")

  io_make_file(file_path, "print('hello world!')")

  git_init(folder_path)

  git_repo <- file.path(folder_path,'.git')

  git_add(git_repo, folder_path, file_path)

  git_commit(git_repo, folder_path, "hello world commit", "fakeAuthor", "fakeEmail")

  return(git_repo)
}

#' Removes sample folder and git log
#'
#' This is a TearDown helper function for Kaiaulu unit tests
#' that manipulates git logs.
#'
#' A folder kaiaulu_sample is assumed to have been created by \code{\link{git_create_sample_log}}, and is deleted by this function.
#'
#' @param folder_path An optional path to where the sample .git should be created.
#' @return The path to the sample .git file.
#' @export
#' @family {unittest}
git_delete_sample_log <- function(folder_path="/tmp"){
  folder_path <- path.expand(folder_path)
  folder_path <- file.path(folder_path,"kaiaulu_sample")
  error <- system2('rm',
                   args = c('-r',
                            folder_path),
                   stdout = TRUE,
                   stderr = FALSE)
}



#' Git Init
#'
#' Initializes a new Git repository in the specified folder.
#' The Git Init command creates a hidden `.git` folder.
#'
#' @param folder_path The path to the folder where the Git repository should be initialized.
#' @return The path to the newly created Git repository.
#' @export
git_init <- function(folder_path) {
  error <- system2('git',
                   args = c('init', folder_path),
                   stdout = TRUE,
                   stderr = FALSE)

  git_repo <- file.path(folder_path, '.git')

  return(git_repo)
}

#' Git Commit
#'
#' Executes the `git commit` command on the target
#' git repository.
#'
#' @param git_repo The git repo path.
#' @param folder_path The worktree path.
#' @param commit_msg The commit associated with the commit.
#' @param author The author associated with the commit.
#' @param email The email of the author associated with the commit.
#' @return The path to the sample .git file.
#' @export
git_commit <- function(git_repo, folder_path, commit_msg, author, email) {
  error <- system2('git',
                   args = c('--git-dir',
                            git_repo,
                            '--work-tree',
                            folder_path,
                            'commit',
                            '-m',
                            shQuote(commit_msg),
                            '--author',
                            paste0('\"',author,"<",email,">\"")),
                   stdout = TRUE,
                   stderr = FALSE)
  return(git_repo)
}

#' Git Add
#'
#' Performs the `git add` command, which stages a given
#' file in the repo.
#'
#' @param git_repo The git repo path
#' @param folder_path The worktree path
#' @param filepath The filepath we want to add
#' @return The path to the sample .git file.
#' @export
git_add <- function(git_repo, folder_path, filepath) {
  error <- system2('git',
                   args = c('--git-dir',
                            git_repo,
                            '--work-tree',
                            folder_path,
                            'add',
                            filepath),
                   stdout = TRUE,
                   stderr = FALSE)
  return(git_repo)
}

#' Git Mv
#'
#' Performs a `git mv`, which serves to
#' rename a folder or file. Specifically,
#' The `git mv` command combines `mv` and `git add`
#' command for the new folder/file name and the old
#' folder/file name in one command.
#'
#' @param git_repo The git repo path
#' @param folder_path The worktree path
#' @param old_name The name of the file/folder that you are going to change or move
#' @param new_name The new name of the file/folder
#' @export
git_mv <- function(git_repo, folder_path, old_name, new_name) {
  # Construct the command to rename the file using 'mv'

  error <- system2('git',
                   args = c('--git-dir',
                            git_repo,
                            '--work-tree',
                            folder_path,
                            'mv',
                            old_name,
                            new_name),
                   stdout = TRUE,
                   stderr = FALSE)
}



############## Network Transform ##############


#' Create time-ordered contribution network
#'
#' @description Create a collaboration network as described by Joblin et al.
#' where an edge from developer A to developer B is created if A modifies a
#' file, and B modifies it chronologically immediately after. Note contrary
#' to the paper this definition is for files, not functions, and the weight
#' of the edges is the number of changes to a file, not churn.
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}. The
#' name column will be used to label nodes.
#' @param mode author, committer
#' @param lag either the string "one_lag" or "all_lag". See \code{\link{temporal_graph_projection}}
#' @param weight_scheme_function the weight scheme function. See \code{\link{temporal_graph_projection}}
#' @export
#' @family edgelists
#' @references M. Joblin, W. Mauerer, S. Apel,
#' J. Siegmund and D. Riehle, "From Developer Networks
#' to Verified Communities: A Fine-Grained Approach,"
#' 2015 IEEE/ACM 37th IEEE International Conference on
#' Software Engineering, Florence, 2015, pp. 563-573,
#' doi: 10.1109/ICSE.2015.73.
transform_gitlog_to_temporal_network <- function(project_git,mode = c("author","committer"),lag = "one_lag",weight_scheme_function = weight_scheme_sum_edges){


  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)



  if(mode == "author"){

    git_graph <- copy(project_git)
    setnames(git_graph,
             old = c("author_name_email",
                     "file_pathname",
                     "author_datetimetz"),
             new = c("from",
                     "to",
                     "datetimetz"))

    git_graph <- model_directed_graph(git_graph,is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = FALSE)


  }else if(mode == "committer"){

    git_graph <- copy(project_git)
    setnames(git_graph,
             old = c("committer_name_email",
                     "file_pathname",
                     "committer_datetimetz"),
             new = c("from",
                     "to",
                     "datetimetz"))

    git_graph <- model_directed_graph(git_graph,is_bipartite = TRUE, color = c("black","#bed7be"), aggregate_duplicate = FALSE)




  }

  temporal_projection <- temporal_graph_projection(git_graph,mode=TRUE,timestamp_column ="datetimetz",
                                                   weight_scheme_function = weight_scheme_function,
                                                   lag = lag)


  return(temporal_projection)
}
#' Transform parsed git repo into an edgelist
#'
#' @param project_git_entity A parsed git project by \code{\link{parse_gitlog_entity}}.
#' @param mode The network of interest: author-entity, committer-entity, commit-entity, author-committer
#' @export
#' @family edgelists
transform_gitlog_to_entity_bipartite_network <- function(project_git_entity, mode = c("author-entity","committer-entity","commit-entity",'author-committer')){
  author_name_email <- author_datetimetz <- commit_hash <- committer_name_email <- committer_datetimetz <- lines_added <- lines_removed <- NULL # due to NSE notes in R CMD check
  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)
  # Select and rename relevant columns. Key = commit_hash.
  project_git_entity <- project_git_entity[,.(author=author_name_email,
                                              author_date=author_datetimetz,
                                              commit_hash=commit_hash,
                                              committer=committer_name_email,
                                              committer_date = committer_datetimetz,
                                              entity,
                                              weight)]

  if(mode == "author-entity"){
    # Select relevant columns for nodes
    git_graph <- model_directed_graph(project_git_entity[,.(from=author,to=entity)],
                                      is_bipartite=TRUE,
                                      color=c("black","#fafad2"))
  }else if(mode == "committer-entity"){
    # Select relevant columns for nodes
    git_graph <- model_directed_graph(project_git_entity[,.(from=author,to=entity)],
                                      is_bipartite=TRUE,
                                      color=c("#bed7be","#fafad2"))
  }else if(mode == "commit-entity"){
    git_graph <- model_directed_graph(project_git_entity[,.(from=commit_hash,to=entity)],
                                      is_bipartite=TRUE,
                                      color=c("#afe569","#fafad2"))
  }else if(mode == "author-committer"){
    git_graph <- model_directed_graph(project_git_entity[,.(from=author,to=committer)],
                                      is_bipartite=TRUE,
                                      color=c("#bed7be","black"))
  }
  return(git_graph)
}
#' Create time-ordered contribution network
#'
#' @description Create a collaboration network as described by Joblin et al.
#' where an edge from developer A to developer B is created if A modifies a
#' file, and B modifies it chronologically immediately after. This implementation
#' matches the one defined by Joblin et al.
#'
#' @param project_git_entity A parsed git project by \code{\link{parse_gitlog_entity}}.
#' @param mode author, committer
#' @param lag either the string "one_lag" or "all_lag". See \code{\link{temporal_graph_projection}}
#' @param weight_scheme_function the weight scheme function. See \code{\link{temporal_graph_projection}}
#' @export
#' @family edgelists
#' @references M. Joblin, W. Mauerer, S. Apel,
#' J. Siegmund and D. Riehle, "From Developer Networks
#' to Verified Communities: A Fine-Grained Approach,"
#' 2015 IEEE/ACM 37th IEEE International Conference on
#' Software Engineering, Florence, 2015, pp. 563-573,
#' doi: 10.1109/ICSE.2015.73.
transform_gitlog_to_entity_temporal_network <- function(project_git_entity,mode = c("author","committer"),lag = "one_lag",weight_scheme_function=weight_scheme_sum_edges){

  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)

  git_entity <- copy(project_git_entity)


  if(mode == "author"){
    setnames(git_entity,
             old = c("author_name_email",
                     "entity_definition_name",
                     "author_datetimetz"),
             new = c("from",
                     "to",
                     "datetimetz"))

    git_entity$weight <- git_entity$n_lines_changed
    git_graph <- model_directed_graph(git_entity,is_bipartite = TRUE,
                                      color = c("black","#fafad2"),
                                      aggregate_duplicate = FALSE)


  }else{
    setnames(git_graph,
             old = c("committer_name_email",
                     "entity_definition_name",
                     "committer_datetimetz"),
             new = c("from",
                     "to",
                     "datetimetz"))
    git_graph[["edgelist"]]$weight <- git_graph[["edgelist"]]$n_lines_changed
    git_graph <- model_directed_graph(git_graph,is_bipartite = TRUE,
                                      color = c("black","#bed7be"),
                                      aggregate_duplicate = FALSE)
  }
  git_graph[["edgelist"]] <- git_graph[["edgelist"]][,.(from,to,weight,datetimetz)]

  temporal_projection <- temporal_graph_projection(git_graph,mode=TRUE,timestamp_column ="datetimetz",
                                                   weight_scheme_function = weight_scheme_function,
                                                   lag = lag)

  return(temporal_projection)
}

#' Transform parsed git repo commit messages id and files into an edgelist
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}.
#' @param commit_message_id_regex the regex to extract the id from the commit message
#' @export
#' @family edgelists
transform_commit_message_id_to_network <- function(project_git, commit_message_id_regex){
  commit_message_id <- NULL # due to NSE notes in R CMD check
  # Extract the id according to the parameter regex
  project_git$commit_message_id <- data.table(stringi::stri_match_first_regex(project_git$commit_message,
                                                                              pattern = commit_message_id_regex))

  # Keep only the edges which contain the commit message id

  project_git <- project_git[!is.na(commit_message_id),.(commit_message_id,
                                                         file_pathname)]

  git_graph <- model_directed_graph(project_git[,.(from=commit_message_id,to=file_pathname)],
                                    is_bipartite=TRUE,
                                    color=c("#0052cc","#f4dbb5"))
  return(git_graph)

}

