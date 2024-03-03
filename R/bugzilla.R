# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Downloader ##############


#' Download Bugzilla issues using the Bugzilla REST API
#'
#' Downloads bugzilla issues into a folder, where each file is a json containing a page of issues.
#' Returns a list of bugzilla bug ids for use with \code{\link{download_bugzilla_rest_comments}}.
#'
#' Note that some bugzilla sites limit the bugs that can retrieved in a single GET request.
#' The limit_upperbound you set to retrieve a number of bugs for each page/file may be greater
#' than the limit set for bug retrieval by the bugzilla site, in which case, this function
#' will adjust the limit accordingly to ensure all bugs are retrieved.
#'
#' @param bugzilla_site URL to specific bugzilla site
#' @param start_timestamp when to start bug retrieval (ex. 2023-01-01T00:14:57Z)
#' @param save_folder_path the full *folder* path where the bugzilla issues will be stored
#' @param limit_upperbound the number of issues saved in each page file. Some bugzilla sites have limits set on how many bugs
#' can be retrieved in one GET request, in which case, the limit set by the bugzilla site will be used in place of
#' limit_upperbound to ensure full bug retrieval.
#' @seealso \code{\link{download_bugzilla_rest_comments}} a downloader function to download the issue comments
#' @return a vector of bug ids
#' @export
download_bugzilla_rest_issues <- function(bugzilla_site, start_timestamp, save_folder_path, limit_upperbound=500){
  # Format link to retrieve data using Bugzilla REST API
  bugzilla_site <- paste(bugzilla_site, "/rest", sep="")

  # Make sure folder path is correctly formatted
  if (stringi::stri_sub(save_folder_path,-1) != "/"){
    save_folder_path <-paste0(save_folder_path, "/")
  }

  # Holds the bug ids
  bug_ids <- c()
  # Defines what bug to start from in bugs retrieved.
  offset <- 0
  # Defines name of the file. Each page contains 500 bugs.
  page <- 0
  # Defines the limit.
  limit <- limit_upperbound

  # Get the issues starting from the specified date
  issues <- httr::GET(paste0(bugzilla_site, "/bug", "?creation_time=", start_timestamp, "&limit=", limit, "&offset=", offset))

  # Save the issues and the bug ids if there are issues.
  if (length(httr::content(issues)$bugs) > 0){
    jsonlite::write_json(jsonlite::fromJSON(httr::content(issues, "text")),
                         paste0(save_folder_path, page, ".json"), auto_unbox=TRUE)
    json_issues <- rawToChar(httr::content(issues, as = "raw"))

    # Get the ids of the bugs to download comments associated with these bugs in
    # download_bugzilla_comments_from_rest_api
    current_bug_ids <- jsonlite::fromJSON(json_issues)[["bugs"]][["id"]]
    bug_ids <- c(bug_ids, current_bug_ids)
    page <- page + 1

    # Get the table for the first page of bugs to check the number of rows
    # and determine the actual value of the limit parameter.
    page1_bug_table <- parse_bugzilla_rest_issues(save_folder_path)
    # Get the limit we should use.
    limit <- nrow(page1_bug_table)
    # Add to the offset based on the limit.
    offset <- offset + limit

  }

  # Keep retrieving bugs until we get all the pages.
  while(length(httr::content(issues)$bugs) > 0){
    issues <- httr::GET(paste0(bugzilla_site, "/bug", "?creation_time=", start_timestamp,
                               "&limit=", limit, "&offset=", offset))

    # There are no more bugs, so we break.
    if (length(httr::content(issues)$bugs) == 0){
      break
    }

    # Write the bugs to a file
    jsonlite::write_json(jsonlite::fromJSON(httr::content(issues, "text")),
                         paste0(save_folder_path, page, ".json"), auto_unbox=TRUE)
    json_issues <- rawToChar(httr::content(issues, as = "raw"))

    # Get the ids of the bugs to download comments associated with these bugs in
    # download_bugzilla_comments_from_rest_api
    current_bug_ids <- jsonlite::fromJSON(json_issues)[["bugs"]][["id"]]
    bug_ids <- c(bug_ids, current_bug_ids)
    offset <- offset + limit
    page <- page + 1
  }

  return(bug_ids)
}

#' Download Bugzilla comments using the Bugzilla REST API
#'
#' Downloads comments associated with each bug id returned from \code{\link{download_bugzilla_rest_issues}}.
#' Each file saved contains a group of comments associated with a particular bug, where the filename is the corresponding bug id.
#'
#' @param bugzilla_site URL to specific bugzilla site
#' @param bug_ids the ids of the bugs to extract comments for from \code{\link{download_bugzilla_rest_issues}}
#' @param save_folder_path the full *folder* path where the bugzilla comments will be stored
#' @seealso \code{\link{download_bugzilla_rest_issues}} a downloader function to download the bugzilla issues data
#' @export
download_bugzilla_rest_comments <- function(bugzilla_site, bug_ids, save_folder_path){
  # Format link to retrieve data using Bugzilla REST API
  bugzilla_site <- paste(bugzilla_site, "/rest", sep="")

  # Make sure folder path is correctly formatted
  if (stringi::stri_sub(save_folder_path,-1) != "/"){
    save_folder_path <-paste0(save_folder_path, "/")
  }

  # Get the comments associated with the bug_ids and save each group of comments for a bug
  # to a json file at the specified save_folder_path
  for (i in 1:length(bug_ids)){
    comments <- httr::GET(paste(bugzilla_site, "/bug/", bug_ids[i], "/comment", sep=""),
                          httr::write_disk(file.path(paste0(save_folder_path, bug_ids[i], ".json")), overwrite = TRUE))
  }
}

#' Download Bugzilla issues and comments using Perceval traditional backend.
#'
#' @param perceval_path path to perceval binary
#' @param bugzilla_site link to specific bugzilla site
#' @param datetime fetch bugs updated since this date (in any ISO 8601 format, e.g., 'YYYY-MM-DD HH:mm:SS+|-HH:MM'))
#' @param save_file_path the file path, name and extension (should be .json) to save the file.
#' @param max_bugs the maximum number of bugs requested on the same query. Note: Some sites might have restrictions on the number of bugs in one request.
#' @seealso \code{\link{parse_bugzilla_perceval_traditional_issue_comments}} a parser function to parse bugzilla data
#' @return path to downloaded json file.
#' @export
download_bugzilla_perceval_traditional_issue_comments <- function(perceval_path, bugzilla_site, datetime, save_file_path, max_bugs=500){
  perceval_path <- path.expand(perceval_path)
  save_file_path <- path.expand(save_file_path)
  json_data <- system2(perceval_path,
                       args = c('bugzilla', bugzilla_site, '--json-line', '--from-date', paste0('"',datetime,'"'),
                                "--output",save_file_path,"--max-bugs", max_bugs),
                       stdout = TRUE,
                       stderr = FALSE)
  return(save_file_path)
}

#' Download Bugzilla issues and comments using Perceval REST API backend.
#'
#' Note that for the Bugzilla REST API backend, Bugzilla sites may limit the number of bugs that can be retrieved at one time.
#' Thus, the max_bugs parameter needs to be set correctly to ensure all bugs are retrieved and that
#' the json data is not broken. If you get an error trying to parse the data downloaded with this
#'
#' @param perceval_path path to perceval binary
#' @param bugzilla_site link to specific bugzilla site
#' @param datetime fetch bugs updated since this date (in any ISO 8601 format, e.g., 'YYYY-MM-DD HH:mm:SS+|-HH:MM'))
#' @param save_file_path the file path, name and extension (should be .json) to save the file.
#' @param max_bugs the maximum number of bugs requested on the same query. This acts as the limit parameter
#' in the Bugzilla REST API. Bugzilla sites may have specific limits set, so make sure to change the max_bugs
#' parameter accordingly to correctly download the data when using the "bugzillarest" backend.
#' @seealso \code{\link{parse_bugzilla_perceval_rest_issue_comments}} a parser function to parse bugzilla data
#' @return json object with bugzilla data
#' @export
download_bugzilla_perceval_rest_issue_comments <- function(perceval_path, bugzilla_site, datetime, save_file_path, max_bugs=500){
  perceval_path <- path.expand(perceval_path)
  save_file_path <- path.expand(save_file_path)
  json_data <- system2(perceval_path,
                       args = c('bugzillarest', bugzilla_site, '--json-line',
                                '--from-date', paste0('"',datetime,'"'),
                                "--output",save_file_path,
                                "--max-bugs", max_bugs),
                       stdout = TRUE,
                       stderr = FALSE)
  return(save_file_path)
}

#' Download project data (issues and comments) from bugzilla site
#' Note: The first comment in every issue is the issue description
#' @param bugzilla_site URL to specific bugzilla site
#' @param start_timestamp when to start bug retrieval (ex. 2023-01-01T00:14:57Z)
#' @param save_folder_path the full *folder* path where the bugzilla issues will be stored
#' @param limit_upperbound the number of issues saved in each page file. Some bugzilla sites have limits set on how many bugs
#' can be retrieved in one GET request, in which case, the limit set by the bugzilla site will be used in place of
#' limit_upperbound to ensure full bug retrieval.
#' @seealso \code{\link{parse_bugzilla_rest_issues_comments}} a parser function to parse Bugzilla issues and comments data
#' @export
download_bugzilla_rest_issues_comments <- function(bugzilla_site, start_timestamp, save_folder_path, limit_upperbound = 500) {
  # Format link to retrieve data using Bugzilla REST API
  bugzilla_site <- paste(bugzilla_site, "/rest", sep="")

  # Make sure folder path is correctly formatted
  if (stringi::stri_sub(save_folder_path,-1) != "/"){
    save_folder_path <-paste0(save_folder_path, "/")
  }

  # Defines what bug to start from in bugs retrieved.
  offset <- 0
  # Defines name of the file. Each page contains 500 bugs.
  page <- 0
  # Defines the limit.
  limit <- limit_upperbound
  # Initialize to keep request or not
  keep_request <- TRUE

  while(keep_request){
    # Get request to get the project data
    issues <- httr::GET(paste0(bugzilla_site, "/bug", "?creation_time=", start_timestamp, "&include_fields=_default,comments", "&limit=", limit, "&offset=", offset))

    # Check if the limit being restrict or not
    if(as.integer(httr::content(issues)$limit) != limit){
      limit <- as.integer(httr::content(issues)$limit)
    }

    # Check if there is any issue created after specific date
    if(httr::content(issues)$total_matches > 0){
      issues_content <- httr::content(issues, "text")
      issues_content <- jsonlite::fromJSON(issues_content)
      jsonlite::write_json(issues_content, file.path(save_folder_path, paste0(page, ".json")), auto_unbox = TRUE)
      page <- page + 1
      offset <- offset + limit
    } else{
      keep_request <- FALSE
    }
  }
}

############## Parsers ##############

#' Parse Bugzilla data obtained from Perceval traditional Bugzilla backend
#'
#' @param bugzilla_json_path json path for downloaded bugzilla JSON from \code{\link{download_bugzilla_perceval_traditional_issue_comments}}
#' @param comments if true, the comments are parsed along with the issues
#' @seealso \code{\link{download_bugzilla_perceval_traditional_issue_comments}} a downloader function to download bugzilla data with perceval
#' @return data table with parsed bugzilla data
#' @export
#' @family parsers
parse_bugzilla_perceval_traditional_issue_comments <- function(bugzilla_json_path, comments=FALSE){
  bugzilla_json_path <- path.expand(bugzilla_json_path)
  # Get table from the json
  json_issue_comments <- data.table(jsonlite::stream_in(file(bugzilla_json_path), verbose = FALSE))

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
#' @param bugzilla_json_path json path for downloaded bugzilla JSON from \code{\link{download_bugzilla_perceval_rest_issue_comments}}
#' @param comments if true, the comments are parsed along with the issues
#' @seealso \code{\link{download_bugzilla_perceval_rest_issue_comments}} a donwoloader function download bugzilla data with perceval
#' @return data table
#' @export
#' @family parsers
parse_bugzilla_perceval_rest_issue_comments <- function(bugzilla_json_path, comments=FALSE){
  # Get table from the json
  bugzilla_json_path <- path.expand(bugzilla_json_path)
  json_issue_comments <- data.table(jsonlite::stream_in(file(bugzilla_json_path), verbose = FALSE))

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

#' Parse Bugzilla issues data obtained from json files from Bugzilla crawler
#'
#' @param issues_folder_path path to the issue folder that contains json file with Bugzilla data inside
#' @seealso \code{\link{download_bugzilla_rest_issues_comments}} a downloader function to parse Bugzilla issues and comments data
#' @return data table with parsed Bugzilla issues data
#' @export
#' @family parsers
parse_bugzilla_rest_issues <- function(issues_folder_path){
  json_file_paths <- list.files(issues_folder_path)

  result <- data.table::data.table(list())
  expected_columns <- c("id",
                        "summary",
                        "issue_type",
                        "status",
                        "resolution",
                        "creation_time",
                        "last_change_time",
                        "creator_detail.id",
                        "creator_detail.real_name",
                        "component",
                        "assigned_to_detail.id",
                        "assigned_to_detail.real_name",
                        "target_milestone",
                        "platform",
                        "whiteboard",
                        "product",
                        "version",
                        "severity",
                        "priority",
                        "op_sys",
                        "classification",
                        "keywords")

  expected_columns_names <- c("issue_key",
                              "issue_summary",
                              "issue_type",
                              "issue_status",
                              "issue_resolution",
                              "issue_created_datetimez",
                              "issue_updated_datetimez",
                              "issue_reporter_id",
                              "issue_reporter_name",
                              "issue_components",
                              "issue_assignee_id",
                              "issue_assignee_name",
                              "issue_target_milestone",
                              "issue_rep_platform",
                              "issue_status_whiteboard",
                              "issue_product",
                              "issue_version",
                              "issue_severity",
                              "issue_priority",
                              "issue_op_system",
                              "issue_classification",
                              "issue_keywords")

  # Check if files exist in given folder or not
  if(length(json_file_paths) > 0){

    # Loop over the json file in given folder
    for(json_file in json_file_paths){
      json_file_path <- file.path(issues_folder_path, json_file)
      json_object <- jsonlite::fromJSON(json_file_path)

      if(length(json_object$faults) > 0){
        # Get all the faults from json file
        faults <- data.table::data.table(json_object$faults)
        # Add issue type
        faults[, issue_type := "faults"]
        # Add the faults to the result data.table
        result <- rbindlist(list(result, faults), fill = TRUE)[, ..expected_columns]
      }

      if(length(json_object$bugs) > 0){
        # Get all the bugs from json file
        bugs <- data.table::data.table(json_object$bugs)
        # Add issue type
        bugs[, issue_type := "bugs"]
        # Add the bugs to the result data.table
        result <- rbindlist(list(result, bugs), fill = TRUE)[, ..expected_columns]
      }
    }
  }

  # Rename the columns of data.table
  setnames(result, expected_columns_names)

  return(result)
}

#' Parse Bugzilla comments data obtained from json files from Bugzilla crawler \code{\link{parse_bugzilla_rest_comments}}
#'
#' @param comments_folder_path path to the comments folder that contains json file with Bugzilla data inside
#' @return data table with parsed Bugzilla comments data
#' @export
#' @family parsers
parse_bugzilla_rest_comments <- function(comments_folder_path){
  json_file_paths <- list.files(comments_folder_path)

  expected_columns <- c("bug_id",
                        "id",
                        "creation_time",
                        "creator",
                        "creator_id",
                        "text",
                        "count",
                        "is_private")

  expected_columns_names <- c("issue_key",
                              "comment_id",
                              "comment_created_datetimez",
                              "comment_author_name",
                              "comment_author_id",
                              "comment_body",
                              "comment_count",
                              "comment_is_private")

  result <- data.table::data.table(list())

  # Check if files exist in given folder or not
  if(length(json_file_paths) > 0){

    # Loop over the json file in given folder
    for(json_file in json_file_paths){
      json_file_path <- file.path(comments_folder_path, json_file)
      json_object <- jsonlite::fromJSON(json_file_path)

      if(length(json_object$bugs) > 0){
        # Get all the issue keys from json file
        issue_keys <- names(json_object$bugs)

        # Get all the comments from json file
        comments <- json_object$bugs[[issue_keys]]$comments

        # Add the comments to the result data.table
        result <- rbindlist(list(result, comments), fill = TRUE)[, ..expected_columns]
      }
    }
  }

  # Rename the columns of data.table
  setnames(result, expected_columns_names)

  return(result)
}

#' Parse Bugzilla issues and comments data table
#'
#' @param bugzilla_folder_path path to the folder that contains json file with Bugzilla data inside
#' @seealso \code{\link{parse_bugzilla_rest_issues}} a parser function to parse Bugzilla issues data
#' @seealso \code{\link{download_bugzilla_rest_issues_comments}} a downloader function to parse Bugzilla issues and comments data
#' @return data table with Bugzilla issue data and Bugzilla comments data
#' @export
#' @family parsers
parse_bugzilla_rest_issues_comments <- function(bugzilla_folder_path){
  json_file_paths <- list.files(bugzilla_folder_path)
  bugzilla_issues <- parse_bugzilla_rest_issues(bugzilla_folder_path)
  bugzilla_comments <- data.table::data.table(list())
  result <- data.table::data.table(list())

  expected_comments_columns <- c("bug_id",
                                 "id",
                                 "creation_time",
                                 "creator",
                                 "creator_id",
                                 "text",
                                 "count",
                                 "is_private")

  expected_comments_columns_names <- c("issue_key",
                                       "comment_id",
                                       "comment_created_datetimez",
                                       "comment_author_name",
                                       "comment_author_id",
                                       "comment_body",
                                       "comment_count",
                                       "comment_is_private")

  # Check if files exist in given folder or not
  if(length(json_file_paths) > 0){

    # Loop over the json file in given folder
    for(json_file in json_file_paths){
      json_file_path <- file.path(bugzilla_folder_path, json_file)
      json_object <- jsonlite::fromJSON(json_file_path)

      if(length(json_object$bugs$comments) > 0){
        comments <- json_object$bugs$comments

        for(comment in comments){
          # Get all the bugs from json file
          comment <- data.table::data.table(comment)
          bugzilla_comments <- rbindlist(list(bugzilla_comments, comment), fill = TRUE)[, ..expected_comments_columns]
        }
      }
    }
  }
  setnames(bugzilla_comments, expected_comments_columns_names)

  # Merge data table by issue key
  result <- data.table::merge.data.table(bugzilla_issues, bugzilla_comments, by="issue_key", all=TRUE)

  return(result)
}
