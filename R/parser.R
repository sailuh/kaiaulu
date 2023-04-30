# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Parse Bugzilla data obtained from Perceval traditional Bugzilla backend
#'
#' @param bugzilla_json json object containing Bugzilla data
#' @param comments if true, the comments are parsed along with the issues
#' @seealso \code{\link{download_bugzilla_perceval_traditional_issue_comments}} a downloader function to download bugzilla data with perceval
#' @return data table with parsed bugzilla data
#' @export
#' @family parsers
parse_bugzilla_perceval_traditional_issue_comments <- function(bugzilla_json, comments=FALSE){
  # Get table from the json
  json_issue_comments <- data.table(jsonlite::stream_in(textConnection(bugzilla_json), verbose = FALSE))

  # Comments list parser function. Comments may occur on any json issue.
  bugzilla_parse_comment <- function(comment, bug_id){
    num_comments <- length(comment[["commentid"]])

    parsed_comment <- list()

    # First comment is issue description, so we start indexing at 2
    # Add the bug id as a column
    parsed_comment[["issue_key"]] <- bug_id[[1]]
    parsed_comment[["comment_id"]] <- comment[["commentid"]][[2]][[1]]
    parsed_comment[["comment_author_id"]] <- comment[["who"]][[2]][[1]]
    parsed_comment[["comment_author_name"]] <- comment[["who"]][[2]][[1]]
    parsed_comment[["comment_body"]] <- comment[["thetext"]][[2]][[1]]
    parsed_comment[["comment_created_datetimetz"]] <- comment[["bug_when"]][[2]][[1]]
    parsed_comment[["comment_count"]] <- comment[["comment_count"]][[2]][[1]]
    parsed_comment[["comment_is_private"]] <- comment[["isprivate"]][[2]][[1]]

    parsed_comments <- list()
    parsed_comments <- append(list(parsed_comments), list(parsed_comment))

    # If there's more than one comment, parse it.
    if (num_comments > 2) {
      for (i in 3:num_comments){
        parsed_comment <- list()
        # Add the bug id as a column
        parsed_comment[["issue_key"]] <- bug_id[[1]]
        parsed_comment[["comment_id"]] <- comment[["commentid"]][[i]][[1]]
        #print("comment_id")
        parsed_comment[["comment_author_id"]] <- comment[["who"]][[i]][[1]]
        #print("comment_author_id")
        parsed_comment[["comment_author_name"]] <- comment[["who"]][[i]][[2]]
        #print("comment_author_name")
        parsed_comment[["comment_body"]] <- comment[["thetext"]][[i]][[1]]
        #print("comment_body")
        parsed_comment[["comment_created_datetimetz"]] <- comment[["bug_when"]][[i]][[1]]
        #print("comment_created_datetimetz")
        parsed_comment[["comment_count"]] <- comment[["comment_count"]][[i]][[1]]
        #print("comment_count")
        parsed_comment[["comment_is_private"]] <- comment[["isprivate"]][[i]][[1]]
        #print("comment_is_private")

        parsed_comments <- append(parsed_comments, list(parsed_comment))
      }
    }
    return(parsed_comments)
  }

  # Issue parser function
  bugzilla_parse_issue <- function(i) {
    # Parse all relevant *issue* fields
    issue_comment <- json_issue_comments

    parsed_issue <- data.table(
      issue_key = issue_comment[["data.bug_id"]][[i]],
      issue_summary = issue_comment[["data.short_desc"]][[i]],
      issue_type = issue_comment[["category"]][[i]],
      issue_status = issue_comment[["data.bug_status"]][[i]],
      issue_resolution = issue_comment[["data.resolution"]][[i]],
      issue_components = issue_comment[["data.component"]][[i]],
      issue_description = issue_comment[["data.long_desc"]][[i]][["thetext"]][[1]],
      issue_classification = issue_comment[["data.classification"]][[i]], ##### ADDING

      issue_created_datetimetz = issue_comment[["data.creation_ts"]][[i]],
      issue_updated_datetimetz = issue_comment[["data.delta_ts"]][[i]],

      issue_assignee_id = issue_comment[["data.assigned_to"]][[i]][["__text__"]],
      issue_assignee_name = issue_comment[["data.assigned_to"]][[i]][["name"]],

      issue_reporter_id = issue_comment[["data.reporter"]][[i]][["__text__"]],
      issue_reporter_name = issue_comment[["data.reporter"]][[i]][["name"]],

      issue_target_milestone = issue_comment[["data.target_milestone"]][[i]],
      issue_rep_platform = issue_comment[["data.rep_platform"]][[i]],
      issue_status_whiteboard = issue_comment[["data.status_whiteboard"]][[i]],
      issue_keywords = issue_comment[["data.keywords"]][[i]],
      issue_version = issue_comment[["data.version"]][[i]],
      issue_severity = issue_comment[["data.bug_severity"]][[i]],
      issue_priority = issue_comment[["data.priority"]][[i]],
      issue_op_system = issue_comment[["data.op_sys"]][[i]],
      issue_product = issue_comment[["data.product"]][[i]]
    )

    return(parsed_issue)
  }

  # Number of issues in table
  n_issues <- length(unique(json_issue_comments[["data.bug_id"]]))

  # Prepare two lists which will contain data.tables for all issues and all comments
  # Both tables can share the issue_key, so they can be joined if desired.
  all_issues <- list()
  all_comments <- list()

  # Get the issues and comments in a for loop
  for (i in 1:n_issues) {
    # Parse an issue
    all_issues[[i]] <- bugzilla_parse_issue(i)
    comments_i <- json_issue_comments[["data.long_desc"]][[i]]
    if (length(comments_i[["commentid"]]) > 1){
      # Parse the comments associated with the issue
      all_comments <- append(all_comments, bugzilla_parse_comment(comments_i, json_issue_comments[["data.bug_id"]][[i]]))
    }
  }

  # Convert list of issues & list of comments into tables
  all_issues <- rbindlist(all_issues,fill=TRUE)
  all_comments <- rbindlist(all_comments,fill=TRUE)

  # Rename column names for the issues (remove the .__text__)
  colnames(all_issues) <- gsub(".__text__", "", colnames(all_issues), fixed = TRUE)

  # Return output
  if (comments==TRUE) {
    # Merge the issues and comments table
    all_issue_comments <- merge.data.table(all_issues, all_comments, by = "issue_key", all.x = TRUE)
    # Order by datetime
    data.table::setorder(all_issue_comments, cols = "issue_created_datetimetz")
    # Return table of issues and comments
    return(all_issue_comments)

  } else {
    # Return just the issues in the table
    # Order by datetime
    data.table::setorder(all_issues, cols = "issue_created_datetimetz")
    return(all_issues)
  }
}


#' Parse Bugzilla data obtained from Perceval REST API Bugzilla backend
#'
#' @param bugzilla_json json object containing Bugzilla data
#' @param comments if true, the comments are parsed along with the issues
#' @seealso \code{\link{download_bugzilla_perceval_rest_issue_comments}} a donwoloader function download bugzilla data with perceval
#' @return data table
#' @export
#' @family parsers
parse_bugzilla_perceval_rest_issue_comments <- function(bugzilla_json, comments=FALSE){
  # Get table from the json
  json_issue_comments <- data.table(jsonlite::stream_in(textConnection(bugzilla_json), verbose = FALSE))

  # Comments list parser function. Comments may occur on any json issue.
  bugzilla_parse_comment <- function(comment){
    num_comments <- length(comment[["commentid"]])

    parsed_comment <- list()

    # First comment is issue description, so we start indexing at 2
    # Add the bug id as a column
    parsed_comment[["issue_key"]] <- comment[["bug_id"]][[1]]
    parsed_comment[["comment_id"]] <- comment[["id"]][[2]][[1]]
    parsed_comment[["comment_author_id"]] <- comment[["creator_id"]][[2]][[1]]
    parsed_comment[["comment_author_name"]] <- comment[["creator"]][[2]][[1]]
    parsed_comment[["comment_body"]] <- comment[["text"]][[2]][[1]]
    parsed_comment[["comment_created_datetimetz"]] <- comment[["creation_time"]][[2]][[1]]
    parsed_comment[["comment_count"]] <- comment[["count"]][[2]][[1]]
    parsed_comment[["comment_is_private"]] <- comment[["is_private"]][[2]][[1]]

    parsed_comments <- list()
    parsed_comments <- append(list(parsed_comments), list(parsed_comment))

    # If there's more than one comment, parse it.
    if (num_comments > 2) {
      for (i in 3:num_comments){
        parsed_comment <- list()
        # Add the bug id as a column
        parsed_comment[["issue_key"]] <- bug_id[[1]]
        parsed_comment[["comment_id"]] <- comment[["id"]][[i]][[1]]
        parsed_comment[["comment_author_id"]] <- comment[["creator_id"]][[i]][[1]]
        parsed_comment[["comment_author_name"]] <- comment[["creator"]][[i]][[1]]
        parsed_comment[["comment_body"]] <- comment[["text"]][[i]][[1]]
        parsed_comment[["comment_created_datetimetz"]] <- comment[["creation_time"]][[i]][[1]]
        parsed_comment[["comment_count"]] <- comment[["count"]][[i]][[1]]
        parsed_comment[["comment_is_private"]] <- comment[["is_private"]][[i]][[1]]

        parsed_comments <- append(parsed_comments, list(parsed_comment))
      }
    }
    return(parsed_comments)
  }

  # Issue parser function
  bugzilla_parse_issue <- function(i) {
    # Parse all relevant *issue* fields
    issue_comment <- json_issue_comments

    parsed_issue <- data.table(
      issue_key = issue_comment[["data.id"]][[i]][[1]],
      issue_summary = issue_comment[["data.summary"]][[i]][[1]],
      issue_type = issue_comment[["category"]][[i]][[1]],
      issue_status = issue_comment[["data.status"]][[i]][[1]],
      issue_resolution = issue_comment[["data.resolution"]][[i]][[1]],
      issue_components = issue_comment[["data.component"]][[i]][[1]],
      issue_description = issue_comment[["data.description"]][[i]][[1]],
      issue_classification = issue_comment[["data.classification"]][[i]][[1]],

      issue_created_datetimetz = issue_comment[["data.creation_time"]][[i]],
      issue_creator_id = issue_comment[["data.creator_detail.id"]][[i]][[1]],
      issue_creator_name = issue_comment[["data.creator"]][[i]],
      issue_creator_real_name = issue_comment[["data.creator_detail.real_name"]][[i]][[1]],
      issue_creator_active = issue_comment[["data.creator_detail.active"]][[i]][[1]],
      issue_creator_email = issue_comment[["data.creator_detail.email"]][[i]][[1]],
      issue_creator_insider = issue_comment[["data.creator_detail.insider"]][[i]][[1]],

      issue_assignee_id = issue_comment[["data.assigned_to_detail.id"]][[i]][[1]],
      issue_assignee_name = issue_comment[["data.assigned_to"]][[i]][[1]],
      issue_assignee_real_name = issue_comment[["data.assigned_to_detail.real_name"]][[i]][[1]],
      issue_assignee_active = issue_comment[["data.assigned_to_detail.active"]][[i]][[1]],
      issue_assignee_email = issue_comment[["data.assigned_to_detail.email"]][[i]][[1]],
      issue_assignee_insider = issue_comment[["data.assigned_to_detail.insider"]][[i]][[1]],

      issue_target_milestone = issue_comment[["data.target_milestone"]][[i]][[1]],
      issue_rep_platform = issue_comment[["data.platform"]][[i]][[1]],
      issue_status_whiteboard = issue_comment[["data.whiteboard"]][[i]][[1]],
      # In some cases, keywords may be equal to character(0), in which case issue_keywords should be set to NA to prevent a warning
      issue_keywords = ifelse(length(issue_comment[["data.keywords"]][[i]]) > 0, issue_comment[["data.keywords"]][[i]], NA),
      issue_version = issue_comment[["data.version"]][[i]][[1]],
      issue_severity = issue_comment[["data.severity"]][[i]][[1]],
      issue_priority = issue_comment[["data.priority"]][[i]][[1]],
      issue_op_system = issue_comment[["data.op_sys"]][[i]][[1]],
      issue_product = issue_comment[["data.product"]][[i]][[1]]
    )

    return(parsed_issue)
  }

  # Number of issues in table
  n_issues <- length(unique(json_issue_comments[["data.id"]]))

  # Prepare two lists which will contain data.tables for all issues and all comments
  # Both tables can share the issue_key, so they can be joined if desired.
  all_issues <- list()
  all_comments <- list()

  # Get the issues and comments in a for loop
  for (i in 1:n_issues) {
    # Parse an issue
    all_issues[[i]] <- bugzilla_parse_issue(i)
    comments_i <- json_issue_comments[["data.comments"]][[i]]

    if (length(comments_i[["bug_id"]]) > 1){
      # Parse the comments associated with the issue
      all_comments <- append(all_comments, bugzilla_parse_comment(comments_i))
    }
  }

  # Convert list of issues & list of comments into tables
  all_issues <- rbindlist(all_issues,fill=TRUE)
  all_comments <- rbindlist(all_comments,fill=TRUE)

  # Return output
  if (comments==TRUE) {
    # Merge the issues and comments table
    all_issue_comments <- merge.data.table(all_issues, all_comments, by = "issue_key", all.x = TRUE)
    # Order by datetime
    data.table::setorder(all_issue_comments, cols = "issue_created_datetimetz")
    # Return table of issues and comments
    return(all_issue_comments)

  } else {
    # Return just the issues in the table
    # Order by datetime
    data.table::setorder(all_issues, cols = "issue_created_datetimetz")
    return(all_issues)
  }
}


#' Parse gitlog from Perceval
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
               stri_c('--grep=', '"', perl_regex, '"'),
               '--perl-regexp',
               perceval_flags)
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
  }else{
    flags <- perceval_flags
    gitlog_call_message <- git_log(git_repo_path,flags,gitlog_path)
  }

  # Parsed JSON output.
  perceval_output <- system2(perceval_path,
                             args = c('git', '--git-log',gitlog_path,git_uri,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)

  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose = FALSE))

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
#' Parse mbox from Perceval
#'
#' Parses an mbox file, which consists of emails in a mailbox, using the Perceval library.
#' Note .mbox files do not have a consistent number of fields (e.g. Reply Cc.). Due to that,
#' the resulting table of parse mbox may have a different number of columns depending on the
#' data used. This function only ensures if columns of interest are available, then they are
#' consistently renamed for clarity.
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

  columns_of_interest <- c("data.Message.ID","data.In.Reply.To","data.Date","data.From","data.To","data.Cc","data.Subject","data.body.plain","data.body")
  columns_rename <- c("reply_id","in_reply_to_id","reply_datetimetz","reply_from","reply_to","reply_cc","reply_subject","reply_body","reply_body")
  is_available_column <- columns_of_interest %in% colnames(perceval_parsed)

  columns_of_interest <- columns_of_interest[is_available_column]

  perceval_parsed <- perceval_parsed[,..columns_of_interest]

  data.table::setnames(x = perceval_parsed,
                       old = colnames(perceval_parsed),
                       new = columns_rename[is_available_column])

  return(perceval_parsed)
}
#' Parse Jira issue and comments
#'
#' @param json_path path to jira json (issues or issues with comments) obtained using `download_jira_data.Rmd`.
#' @return A named list of two named elements ("issues", and "comments"), each containing a data.table.
#' Note the comments element will be empty if the downloaded json only contain issues.
#' @export
#' @family parsers
parse_jira <- function(json_path){

  json_issue_comments <- jsonlite::read_json(json_path)

  # Comments list parser. Comments may occur on any json issue.
  jira_parse_comment <- function(comment){
    parsed_comment <- list()
    parsed_comment[["comment_id"]] <- comment[["id"]]

    parsed_comment[["comment_created_datetimetz"]] <- comment[["created"]][[1]]
    parsed_comment[["comment_updated_datetimetz"]] <- comment[["updated"]][[1]]

    parsed_comment[["comment_author_id"]] <- comment[["author"]][["name"]][[1]]
    parsed_comment[["comment_author_name"]] <- comment[["author"]][["displayName"]][[1]]
    parsed_comment[["comment_author_timezone"]] <- comment[["author"]][["timeZone"]][[1]]

    parsed_comment[["comment_author_update_id"]] <- comment[["updateAuthor"]][["name"]][[1]]
    parsed_comment[["comment_author_update_name"]] <- comment[["updateAuthor"]][["displayName"]][[1]]
    parsed_comment[["comment_author_update_timezone"]] <- comment[["updateAuthor"]][["timeZone"]][[1]]

    parsed_comment[["comment_body"]] <- comment[["body"]][[1]]

    return(parsed_comment)
  }

  # names(json_issue_comments) => "base_info","ext_info"
  # length([["base_info]]) == length([["ext_info]]) == n_issues.
  # Choose either and store the total number of issues
  n_issues <- length(json_issue_comments[["ext_info"]])

  # Prepare two lists which will contain data.tables for all issues and all comments
  # Both tables can share the issue_key, so they can be joined if desired.
  all_issues <- list()
  all_issues_comments <- list()

  for(i in 1:n_issues){

    # The only use of "base_info" is to obtain the issue_key
    issue_key <- json_issue_comments[["base_info"]][[i]][["key"]]

    # All other information is contained in "ext_info"
    issue_comment <- json_issue_comments[["ext_info"]][[i]]

    # Parse all relevant *issue* fields
    all_issues[[i]] <- data.table(
      issue_key = issue_key,

      issue_summary = issue_comment[["summary"]][[1]],
      issue_type = issue_comment[["issuetype"]][["name"]][[1]],
      issue_status = issue_comment[["status"]][["name"]][[1]],
      issue_resolution = issue_comment[["resolution"]][["name"]][[1]],
      issue_components = unlist(sapply(issue_comment[["components"]],"[[","name")),
      issue_description = issue_comment[["description"]],

      issue_created_datetimetz = issue_comment[["created"]][[1]],
      issue_updated_datetimetz = issue_comment[["updated"]][[1]],
      issue_resolution_datetimetz = issue_comment[["resolutiondate"]],

      issue_creator_id = issue_comment[["creator"]][["name"]][[1]],
      issue_creator_name = issue_comment[["creator"]][["displayName"]][[1]],
      issue_creator_timezone = issue_comment[["creator"]][["timeZone"]][[1]],

      issue_assignee_id = issue_comment[["assignee"]][["name"]][[1]],
      issue_assignee_name = issue_comment[["assignee"]][["displayName"]][[1]],
      issue_assignee_timezone = issue_comment[["assignee"]][["timeZone"]][[1]],

      issue_reporter_id = issue_comment[["reporter"]][["name"]][[1]],
      issue_reporter_name = issue_comment[["reporter"]][["displayName"]][[1]],
      issue_reporter_timezone = issue_comment[["reporter"]][["timeZone"]][[1]]
    )

    # Comments
    # For each issue, comment/comments contain 1 or more comments. Parse them
    # in a separate table.
    root_of_comments_list <- json_issue_comments[["ext_info"]][[i]][["comment"]]
    # If root_of_comments_list does not exist, then this is an issue only json, skip parsing
    if(length(root_of_comments_list) > 0){
      comments_list <- json_issue_comments[["ext_info"]][[i]][["comment"]][["comments"]]
      # Even on a json with comments, some issues may not have comments, check if comments exist:
      if(length(comments_list) > 0){
        # Parse all comments into issue_comments
        issue_comments <- rbindlist(lapply(comments_list,
                                           jira_parse_comment))
        # Add issue_key column to the start of the table
        issue_comments <- cbind(data.table(issue_key=issue_key),issue_comments)
        all_issues_comments[[i]] <- issue_comments
      }
    }
  }
  all_issues <- rbindlist(all_issues,fill=TRUE)
  all_issues_comments <- rbindlist(all_issues_comments,fill=TRUE)

  parsed_issues_comments <- list()
  parsed_issues_comments[["issues"]] <- all_issues
  parsed_issues_comments[["comments"]] <- all_issues_comments

  return(parsed_issues_comments)
}
#' Format Parsed Jira to Replies
#'
#' Combines the JIRA issue author and description to the comments author and
#' description into a reply table suitable for communication analysis.
#' @param parsed_jira A project's jira including issues and comments from \code{\link{parse_jira}}.
#' @return A reply table.
#' @export
parse_jira_replies <- function(parsed_jira){


  project_jira_issues <- parsed_jira[["issues"]]
  project_jira_issues <- project_jira_issues[,.(reply_id=issue_key,
                                           in_reply_to_id=NA_character_,
                                           reply_datetimetz=issue_created_datetimetz,
                                           reply_from=issue_creator_name,
                                           reply_to=NA_character_,
                                           reply_cc=NA_character_,
                                           reply_subject=issue_key,
                                           reply_body=issue_description)]


  project_jira_comments <- parsed_jira[["comments"]]
  project_jira_comments <- project_jira_comments[,.(reply_id=comment_id,
                                  in_reply_to_id=NA_character_,
                                  reply_datetimetz=comment_created_datetimetz,
                                  reply_from=comment_author_name,
                                  reply_to=NA_character_,
                                  reply_cc=NA_character_,
                                  reply_subject=issue_key,
                                  reply_body=comment_body)]

  project_jira <- rbind(project_jira_issues,
                        project_jira_comments)

  return(project_jira)
}
#' Parse GitHub Issue and Pull Request Comments
#'
#' Parses Issue, Pull Request, and Comments Endpoints into a reply table.
#' See example usage in the download_github_comments.Rmd vignette.
#'
#' @param github_replies_folder_path The folder path to where the github api json files have been downloaded.
#' @return A single reply table which combines the communication from the three jsons.
#' @export
parse_github_replies <- function(github_replies_folder_path){

  issues_json_folder_path <- paste0(github_replies_folder_path,"/issue/")
  pull_requests_json_folder_path <- paste0(github_replies_folder_path,"/pull_request/")
  comments_json_folder_path <- paste0(github_replies_folder_path,"/issue_or_pr_comment/")
  commit_json_folder_path <- paste0(github_replies_folder_path,"/commit/")

  # Tabulate Issues
  all_issue <- lapply(list.files(issues_json_folder_path,
                                 full.names = TRUE),jsonlite::read_json)
  all_issue <- lapply(all_issue,
                      github_parse_project_issue)
  all_issue <- rbindlist(all_issue,fill=TRUE)

  # Tabulate PRs
  all_pr <- lapply(list.files(pull_requests_json_folder_path,
                              full.names = TRUE),jsonlite::read_json)
  all_pr <- lapply(all_pr,
                   github_parse_project_pull_request)
  all_pr <- rbindlist(all_pr,fill=TRUE)

  # Tabulate Comments
  all_issue_or_pr_comments <- lapply(list.files(comments_json_folder_path,
                                                full.names = TRUE),jsonlite::read_json)
  all_issue_or_pr_comments <- lapply(all_issue_or_pr_comments,
                                     github_parse_project_issue_or_pr_comments)
  all_issue_or_pr_comments <- rbindlist(all_issue_or_pr_comments,fill=TRUE)


  all_issue <- all_issue[,.(reply_id=issue_id,
                            in_reply_to_id=NA_character_,
                            reply_datetimetz=created_at,
                            reply_from=issue_user_login,
                            reply_to=NA_character_,
                            reply_cc=NA_character_,
                            reply_subject=issue_number,
                            reply_body=body)]

  # Note because GitHub API treats PRs as Issues, then pr_number <=> issue_number
  all_pr <- all_pr[,.(reply_id=pr_id,
                      in_reply_to_id=NA_character_,
                      reply_datetimetz=created_at,
                      reply_from=pr_user_login,
                      reply_to=NA_character_,
                      reply_cc=NA_character_,
                      reply_subject=pr_number,
                      reply_body=body)]

  all_issue_or_pr_comments <- all_issue_or_pr_comments[,.(reply_id=comment_id,
                                                          in_reply_to_id=NA_character_,
                                                          reply_datetimetz=created_at,
                                                          reply_from=comment_user_login,
                                                          reply_to=NA_character_,
                                                          reply_cc=NA_character_,
                                                          reply_subject=issue_url,
                                                          reply_body=body)]

  issue_or_pr_comments_reply_subject <- stringi::stri_split_regex(all_issue_or_pr_comments$reply_subject,
                                                                  "/")
  all_issue_or_pr_comments$reply_subject <- sapply(issue_or_pr_comments_reply_subject,"[[",8)

  replies <- rbind(all_issue,
                   all_pr,
                   all_issue_or_pr_comments)

  # We can then parse the commit messages, and format so we have a look-up table of authors
  # and committers name, e-mail, and github ID:

  all_commits <- lapply(list.files(commit_json_folder_path,
                                   full.names = TRUE),jsonlite::read_json)
  all_commits <- lapply(all_commits,
                        github_parse_project_commits)
  all_commits <- rbindlist(all_commits,fill=TRUE)

  all_github_authors <- all_commits[,.(github_login=author_login,
                                       name_email = stringi::stri_c(commit_author_name,
                                                                    " ",
                                                                    commit_author_email))]

  all_github_committers <- all_commits[,.(github_login=committer_login,
                                          name_email = stringi::stri_c(commit_committer_name,
                                                                       " ",
                                                                       commit_committer_email))]

  all_github_developers <- rbind(all_github_authors,all_github_committers)

  # For simplicity here, when the same GitHub id contains
  # multiple e-mails, we choose one. In the future, we will
  # consider including all e-mails.
  all_github_developers <- all_github_developers[,.(name_email=name_email[1]),by="github_login"]

  # Replace `reply_from` by name<space>email when information is available (i.e.)
  # the github id modified as author or commiter at least one file.
  replies <- merge(replies,all_github_developers,
                   all.x=TRUE,
                   by.x="reply_from",
                   by.y="github_login")

  replies[!is.na(name_email)]$reply_from <-  replies[!is.na(name_email)]$name_email
  replies[,name_email:=NULL]

  return(replies)
}
#' Parse dependencies from Depends
#'
#' @param depends_jar_path path to depends jar
#' @param git_repo_path path to git repo (ends in .git)
#' @param output_dir path to output directory (formatted output_path/)
#' @param language the language of the .git repo (accepts cpp, java, ruby, python, pom)
#' @export
#' @family parsers
parse_dependencies <- function(depends_jar_path,git_repo_path,language,output_dir="/tmp/"){
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
                   project_name,'--dir',
                   output_dir,
                   '--auto-include',
                   '--granularity=file', '--namepattern=/',
                   '--format=json'),
          stdout = FALSE,
          stderr = FALSE)
  # Construct /output_dir/ file path
  output_path <- stri_c(output_dir, project_name,".json")
  # Parsed JSON output.
  depends_parsed <- jsonlite::read_json(output_path)
  # The JSON has two main parts. The first is a vector of all file names.
  file_names <- unlist(depends_parsed[["variables"]])
  # Depends will create full filepaths, but folder_path may be a relative path.
  # We must guarantee our folder_path is also a full path in order to turn all
  # full filepaths generated by Depends into relative paths.
  # "../rawdata/git_repo/helix/" => "/Users/cvp/Desktop/kaiaulu/rawdata/git_repo/helix"
  normalized_folder_path <- normalizePath(folder_path)
  # /Users/user/git_repos/APR/xml/apr_xml_xmllite.c => "xml/apr_xml_xmllite.c"
  file_names <- stri_replace_first(file_names,replacement="",regex=normalized_folder_path)
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

  edgelist <- depends_parsed
  data.table::setnames(x = edgelist,
                       old = c("src","dest"),
                       new = c("src_filepath","dest_filepath"))
  nodes <- data.table(filepath=file_names)
  graph <- list(nodes=nodes,edgelist=edgelist)

  return(graph)
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
#' @param utags_path The path to utags binary.
#'  See \url{https://github.com/universal-ctags/ctags}
#' @param filepath path to file
#' @param kinds the entity kinds utags should identify per line.
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
#' Parse R File and Function Dependencies
#'
#' @param folder_path The path to an R folder path
#' @export
parse_r_dependencies <- function(folder_path){

  all_filepaths <- list.files(file.path(path.expand(folder_path)),recursive=TRUE,pattern="R",full.names=TRUE)
  parsed_r_files <- lapply(all_filepaths,parse_rfile_ast)


  parsed_r_files <- lapply(parsed_r_files,function(x)
    x[,filepath:= stri_replace_first(filepath,regex = stri_c(folder_path,"/"),replacement = "")])

  definitions <- lapply(parsed_r_files,parse_r_function_definition)
  definitions <- rbindlist(definitions)
  unique_definitions <- unique(definitions)


  parse_r_network <- function(parsed_r_file,unique_definitions){
    function_edgelist <- parse_r_function_dependencies(parsed_r_file,unique_definitions)
    return(function_edgelist)
  }

  edgelists <- lapply(parsed_r_files,parse_r_network,unique_definitions)

  filter_by_ownership <- function(edgelist,unique_definitions){
    edgelist <- edgelist[src_functions_call_name %in% unique_definitions$src_functions_name &
                           src_functions_caller_name %in% unique_definitions$src_functions_name]
    return(edgelist)
  }

  edgelists <- lapply(edgelists,filter_by_ownership,unique_definitions)
  edgelists <- rbindlist(edgelists)

  names(parsed_r_files) <- sapply(stri_split_regex(all_filepaths,"/"),data.table::last)

  return(edgelists)
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
