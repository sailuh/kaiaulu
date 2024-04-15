# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Downloaders ##############

#' Download JIRA Issues and/or Comments
#'
#' Download JIRA issues and/or comments using [rest/api/2/search](https://developer.atlassian.com/cloud/jira/platform/rest/v2/api-group-issue-search/#api-rest-api-2-search-post) endpoint
#' to the specified `save_folder_path`.
#'
#' The folder assumes the following convention: "(PROJECTKEY)_(uniextimestamp_lowerbound)_(unixtimestamp_upperbound).json"
#' For example: "SAILUH_1231234_2312413.json".
#'
#' Comments data are added when the field `comment` is included.
#'
#' If a project requires authentication and authentication fails, the function will end without downloading any data.
#'
#' If the number of results per page returned is less than the number specified, the max_results value will adjust to that value.
#'
#' @param domain Custom JIRA domain URL
#' @param username the JIRA username
#' @param password the JIRA password/API token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_folder_path Path that files will be saved in .json format
#' @param max_results (optional) the [maximum number of results](https://confluence.atlassian.com/jirakb/how-to-use-the-maxresults-api-parameter-for-jira-issue-search-rest-api-1251999998.html)
#' to download per page. Default is 50.
#' @param max_total_downloads Maximum downloads per function call.
#' This parameter specifies how many issues should be downloaded. Subsequent API calls will stop if they
#' reach or surpass this value.
#' @param search_query an optional API parameter that alters the GET request.
#' See \code{\link{download_jira_issues_by_date}} and \code{\link{download_jira_issues_by_issue_key}}
#' source code for examples.
#' @param verbose Set verbose=TRUE to print execution details.
#' @export
#' @family jira
#' @family downloaders
#' @seealso \code{\link{download_jira_issues_by_date}} to download JIRA issues and/or comments by date range
#' @seealso \code{\link{download_jira_issues_by_issue_key}} to download JIRA issues and/or comments by issue key range
#' @seealso \code{\link{parse_jira}} to parse jira data with or without comments
#' @seealso  \code{\link{refresh_jira_issues}} to obtain more recent data from any of the downloader functions
download_jira_issues <- function(domain,
                                          jql_query,
                                          fields,
                                          username = NULL,
                                          password = NULL,
                                          save_folder_path,
                                          max_results = 50,
                                          max_total_downloads = 5000,
                                          search_query = NULL,
                                          verbose = FALSE) {

  # Ensure the domain starts with https:// for secure communication.
  if (!grepl("^https?://", domain)) {
    domain <- paste0("https://", domain)
  }

  # append search_query to jql_query if present
  if (!is.null(search_query)){
    jql_query <- paste(jql_query, search_query)
    if(verbose){
      message(jql_query)
    }
  }

  # Initialize variables for pagination
  start_at <- 0
  total <- max_results
  all_issues <- list()
  # This variable counts your download count. This is important to not exceed the max downloads per hour
  download_count <- 0
  time <- Sys.time()
  if(verbose){
    message("Starting Downloads at ", time)
  }
  # Loop that downloads each issue into a file
  repeat{

    # Check update our download count and see if it is approaching the limit
    if (download_count + max_results > max_total_downloads) {
      # error message
      time <- Sys.time()
      if(verbose){
        message("Cannot download as max_total_downloads will be exceeded. Try again at a later time. Download ended at ", time)
      }
        break
    } # Resume downloading

    # Construct the API endpoint URL
    url <- httr::parse_url(domain)

    #Authenticate if username and password are provided
    if(!is.null(username)&&!is.null(password)) {
      # Use the username/password for authentication
      auth <- httr::authenticate(as.character(username), as.character(password), "basic")
      #message("successfully authenticated")
    } else {
      if(verbose){
        message("No username or password present or are formatted incorrectly.")
      }
      auth <- NULL
    }

    url<-httr::modify_url(url = url,
                          scheme = if(is.null(url$scheme)){"https"},
                          path = c(url$path,"/rest/api/2/search"),
                          query=list(jql = jql_query,
                                     fields = paste(fields, collapse = ","),
                                     startAt = start_at,
                                     maxResults = max_results))

    url <- httr::parse_url(url)
    url_built <- httr::build_url(url)

    # Make the API call
    if(!is.null(username)&&!is.null(password)){
      response <- httr::GET(url_built,
                            if(verbose){httr::verbose()},
                            auth,
                            httr::user_agent("github.com/sailuh/kaiaulu"))
    }else{
        response <- httr::GET(url_built,
                              if(verbose){httr::verbose()},
                              httr::user_agent("github.com/sailuh/kaiaulu"))
    }

    if(verbose){
      message("Requested: ", response$request$url)
    }

    # Stop if there's an HTTP error
    if (httr::http_error(response)) {
      stop("API request failed: ", httr::http_status(response)$message)
    }

    # Extract issues. for iteration of naming convention and checks
    r_object_content <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                                           simplifyVector = FALSE)
    # The number of issues downloaded
    issue_count <- length(r_object_content$issues)
    # save the raw content for a writeLines later
    raw_content <- httr::content(response, "text", encoding = "UTF-8")

    # Check to make sure that the api is downloading the correct amount of issues specified by max_results
    # This checks for only the first page (if download_count ==0)
    # If the total number of issues retrieved is less than max_results, then of course issue_count
    # will be < maxResults so we check to make sure this is not true (total >= max_results)
    if ((download_count == 0) && (max_results != issue_count)) {
      if(verbose){
        message(". max_results specified: ", max_results)
        message(". Number of issues retrieved: ", issue_count)
        message(". Something went wrong with the API request. Changing max_results to ", issue_count)
      }
      max_results <- issue_count
    }

    if (issue_count > 0){

    # Construct JSON file name based on refresh convention


    # The file name prefix is the JIRA project key. The from and to unix timestamps are then added.
    file_name <- stringi::stri_extract_first_regex(str=jql_query, pattern="(?<=project=')[:alpha:]+")

    # Extract 'created' dates
    created_dates <- sapply(r_object_content$issues, function(issue) issue$fields$created)

    # Convert to POSIXct date objects
    date_objects <- as.POSIXct(created_dates, format="%Y-%m-%dT%H:%M:%S", tz="UTC")

    # Find the greatest and smallest date
    latest_date <- max(date_objects)
    latest_date_unix <- as.numeric(latest_date)
    oldest_date <- min(date_objects)
    oldest_date_unix <- as.numeric(oldest_date)

    # Append oldest and latest dates to the file name
    file_name <- paste0(file_name, "_", oldest_date_unix)
    file_name <- paste0(file_name, "_", latest_date_unix, ".json")



    # Add path where file will be saved
    file_name <- file.path(save_folder_path,file_name)

    # Print the latest and oldest dates and file name
    if (verbose){
      message("Latest date:", latest_date_unix)
      message("Oldest date:", oldest_date_unix)
      message("File name: ", file_name)
    }
    }

    # write the files if issues present
    if (issue_count > 0){
      writeLines(raw_content, file_name)
    } else {
      if(verbose){
        message("You are all caught up!")
      }
    }

    # update download_count and optional print statements
    download_count <- download_count + issue_count
    if (verbose && (issue_count > 0)){
      message("saved file to ", file_name)
      message("Saved ", download_count, " total issues")
    }


    #updates start_at for next loop
    if (issue_count < max_results) {
      break
    } else {
      start_at <- start_at + max_results
    }
  }

  # Final verbose output
  if (verbose) {
    message("Success! Fetched and saved issues.")
  }

  # Returns the content so that it can be saved to a variable via function call
  return(NULL)
}


#' Download JIRA Issues and/or Comments by Date Range
#'
#' Wraps around \code{\link{download_jira_issues}} providing JQL query parameters for specifying date ranges.
#' Only issues created in the specified date range will be downloaded.
#'
#' Acceptable formats for `date_lower_bound` and `date_upper_bound` are:
#'
#' * "yyyy/MM/dd HH:mm"
#' * "yyyy-MM-dd HH:mm"
#' * "yyyy/MM/dd"
#' * "yyyy-MM-dd"
#'
#' For example: `date_lower_bound=2023/11/16 21:00` (an issue ocurring at the exact specified time will also be downloaded).
#'
#' For further details on the `created` JQL Query see [the associated JIRA API documentation](https://support.atlassian.com/jira-software-cloud/docs/jql-fields/#Created).
#'
#' @param domain Custom JIRA domain URL
#' @param username the JIRA username
#' @param password the JIRA password/API token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_folder_path Path that files will be saved in .json format
#' @param max_results (optional) the [maximum number of results](https://confluence.atlassian.com/jirakb/how-to-use-the-maxresults-api-parameter-for-jira-issue-search-rest-api-1251999998.html)
#' to download per page. Default is 50.
#' @param max_total_downloads Maximum downloads per function call.
#' This parameter specifies how many issues should be downloaded. Subsequent API calls will stop if they
#' reach or surpass this value.
#' See \code{\link{download_jira_issues_by_date}} and \code{\link{download_jira_issues_by_issue_key}}
#' source code for examples.
#' @param date_lower_bound Optional. Specify the lower bound date time (e.g. 2023/11/16 21:00)
#' @param date_upper_bound Optional. Specify the upper bound date time (e.g. 2023/11/17 21:00)
#' @param verbose Set verbose=TRUE to print execution details.
#' @export
#' @family jira
#' @family downloaders
#' @seealso \code{\link{download_jira_issues_by_issue_key}} to download JIRA issues and/or comments by issue key range
#' @seealso \code{\link{download_jira_issues}} for more flexibility in specifying the JQL query
#' @seealso \code{\link{parse_jira}} to parse jira data with or without comments
#' @seealso  \code{\link{refresh_jira_issues}} to obtain more recent data from any of the downloader functions
download_jira_issues_by_date <- function(domain,
                                         jql_query,
                                         fields,
                                         username = NULL,
                                         password = NULL,
                                         save_folder_path,
                                         max_results,
                                         max_total_downloads,
                                         date_lower_bound = NULL,
                                         date_upper_bound = NULL,
                                         verbose){
  created_query <- ""
  if (!is.null(date_lower_bound)){
    created_query <- paste0(created_query, "AND created >= '", date_lower_bound, "' ")
  }
  if (!is.null(date_upper_bound)){
    created_query <- paste0(created_query, "AND created <= '", date_upper_bound, "' ")
  }
  if(verbose){
    message("Appending ", created_query, " to api request.")
  }

  download_jira_issues(domain = domain,
                       jql_query = jql_query,
                       fields = fields,
                       username = username,
                       password = password,
                       save_folder_path = save_folder_path,
                       max_results = max_results,
                       max_total_downloads = max_total_downloads,
                       search_query = created_query,
                       verbose = verbose)
}

#' Download JIRA Issues and/or Comments by Issue Key Range
#'
#' #' Wraps around \code{\link{download_jira_issues}} providing jql query parameters for specifying issue key ranges.
#' Only issues created in the specified date range inclusive will be downloaded.
#'
#' The acceptable format for `issue_key_lower_bound` and `issue_key_upper_bound` is: <project key>-<issue number>
#'
#' For example: `issue_key_lower_bound=SAILUH-1` (SAILUH-1 will also be downloaded).
#'
#' For further details on the `issueKey` JQL Query see [the associated JIRA API documentation](https://support.atlassian.com/jira-software-cloud/docs/jql-fields/#Issue-key)
#'
#'
#' @param domain Custom JIRA domain URL
#' @param username the JIRA username
#' @param password the JIRA password/API token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_folder_path Path that files will be saved in .json format
#' @param max_results (optional) the [maximum number of results](https://confluence.atlassian.com/jirakb/how-to-use-the-maxresults-api-parameter-for-jira-issue-search-rest-api-1251999998.html)
#' to download per page. Default is 50.
#' @param max_total_downloads Maximum downloads per function call.
#' This parameter specifies how many issues should be downloaded. Subsequent API calls will stop if they
#' reach or surpass this value.
#' See \code{\link{download_jira_issues_by_date}} and \code{\link{download_jira_issues_by_issue_key}}
#' source code for examples.
#' @param issue_key_lower_bound Optional. Specify the lower bound issue key (e.g. SAILUH-1)
#' @param issue_key_upper_bound Optional. Specify the upper bound issue key (e.g. SAILUH-3)
#' @param verbose Set verbose=TRUE to print execution details.
#' @export
#' @family jira
#' @family downloaders
#' @seealso \code{\link{download_jira_issues_by_date}} to download JIRA issues and/or comments by date range
#' @seealso \code{\link{download_jira_issues}} for more flexibility in specifying the JQL query
#' @seealso \code{\link{parse_jira}} to parse jira data with or without comments
#' @seealso  \code{\link{refresh_jira_issues}} to obtain more recent data from any of the downloader functions
download_jira_issues_by_issue_key <- function(domain,
                                              jql_query,
                                              fields,
                                              username = NULL,
                                              password = NULL,
                                              save_folder_path,
                                              max_results,
                                              max_total_downloads,
                                              issue_key_lower_bound = NULL,
                                              issue_key_upper_bound = NULL,
                                              verbose){
  created_query <- ""
  if (!is.null(issue_key_lower_bound)){
    created_query <- paste0(created_query, "AND issueKey >= ", issue_key_lower_bound)
  }
  if (!is.null(issue_key_upper_bound)){
    created_query <- paste0(created_query, " AND issueKey <= ", issue_key_upper_bound)
  }
  if(verbose){
    message("Appending ", created_query, " to api request.")
  }

  download_jira_issues(domain = domain,
                       jql_query = jql_query,
                       fields = fields,
                       username = username,
                       password = password,
                       save_folder_path = save_folder_path,
                       max_results = max_results,
                       max_total_downloads = max_total_downloads,
                       search_query = created_query,
                       verbose)
}

#' Refresh JIRA Issues and/or Comments
#'
#' Uses the adopted file name convention by \code{\link{download_jira_issues}} to identify
#' the latest downloaded JIRA issue key KEY-i, and calls
#' \code{\link{download_jira_issues_by_issue_key}} with lower bound KEY-(i+1) to download all
#' newer issues.
#'
#' If the directory is empty, then all issues will be downloaded. This function can therefore
#' be used in the specified folder to continuously refresh available issues and/or comments
#' data.
#'

#' @param domain Custom JIRA domain URL
#' @param username the JIRA username
#' @param password the JIRA password/API token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_folder_path Path that files will be saved in .json format
#' @param max_results (optional) the [maximum number of results](https://confluence.atlassian.com/jirakb/how-to-use-the-maxresults-api-parameter-for-jira-issue-search-rest-api-1251999998.html)
#' to download per page. Default is 50.
#' @param max_total_downloads Maximum downloads per function call.
#' This parameter specifies how many issues should be downloaded. Subsequent API calls will stop if they
#' reach or surpass this value.
#' @param verbose Set verbose=TRUE to print execution details.
#' @export
#' @family downloaders
#' @family jira
#' @seealso \code{\link{download_jira_issues_by_date}} to download JIRA issues and/or comments by date range
#' @seealso \code{\link{download_jira_issues_by_issue_key}} to download JIRA issues and/or comments by issue key range
#' @seealso \code{\link{download_jira_issues}} for more flexibility in specifying the JQL query
#' @seealso \code{\link{parse_jira}} to parse jira data with or without comments
#' @seealso \code{\link{parse_jira_latest_date}} to retrieve the file path of the latest issue key
refresh_jira_issues <- function(domain,
                                jql_query,
                                fields,
                                username = NULL,
                                password = NULL,
                                save_folder_path,
                                max_results,
                                max_total_downloads,
                                verbose){

  # List all files and subdirectories in the directory
  existing_issues <- list.files(path = save_folder_path)

  # If the folder is empty, then start by downloading all issues.
  if(length(existing_issues) == 0) {
    if(verbose){
      message("The folder is empty. Downloading all issues. \n")
    }
    download_jira_issues(domain = domain,
                         jql_query = jql_query,
                         fields = fields,
                         username = username,
                         password = password,
                         save_folder_path = save_folder_path,
                         max_results = max_results,
                         max_total_downloads = max_total_downloads,
                         search_query = NULL,
                         verbose = verbose)
  } else {
    # If folder is not empty, find the highest issue key, and resume download after it.
    # First, get the file name with the last downloaded 'issueKey' value
    file_name_with_greatest_issue_key <- parse_jira_latest_date(save_folder_path)

    # Prepare the path and filename
    issue_refresh <- file.path(save_folder_path, file_name_with_greatest_issue_key)

    # Check if the file exists
    if(file.exists(issue_refresh)) {
    # Check if the file is empty by checking its size
    } else {
      if(verbose){
        stop("The file does not exist.\n")
      }
    }
    if(verbose){
      message("Filename with highest date (and therefore latest issue key is inside): ", issue_refresh)
    }

    # Read the JSON file
    json_data <- jsonlite::fromJSON(txt = issue_refresh, simplifyVector = FALSE)

    # Extract the Maximum issue key value
    # Start with a low value assuming no negative numbers
    max_numeric_part <- -1
    max_key <- ""

    for (issue in json_data$issues) {
      # Extract the key for the current issue
      current_key <- issue$key

      # Extract the numeric part of the key
      # Assuming the key format is "PROJECTNAME-NUMBER"
      numeric_part <- as.numeric(sub("^[^-]+-", "", current_key))

      # Check if the numeric part is greater than the current maximum
      if (numeric_part > max_numeric_part) {
        # Update the maximum numeric part and the corresponding key
        max_numeric_part <- numeric_part
        max_key <- current_key
      }
    }

    # Print the key with the maximum numeric part
    if(verbose){
      message("The greatest issue key value is ", max_key)
    }

    # Construct the search query to append to the JIRA API request
    search_query <- paste0("AND issueKey > ", max_key)
    if(verbose){
      message("Appending ", search_query, " to JQL query")
    }

    # Call the downloader with appended query
    download_jira_issues(domain = domain,
                        jql_query = jql_query,
                        fields = fields,
                        username = username,
                        password = password,
                        save_folder_path = save_folder_path,
                        max_results = max_results,
                        max_total_downloads = max_total_downloads,
                        search_query = search_query,
                        verbose = verbose)
  }
}


############## Parsers ##############

#' Parse JIRA Issues and Comments
#'
#' Parses JIRA issues without or with comments contained in a folder following a standardized file nomenclature.
#' as obtained from \code{\link{download_jira_issues}}. A named list with two elements (issues, comments) is returned
#' containing the issue table and optionally comments table.
#'
#' The following fields are expected on the raw data:
#'
#' issuekey, issuetype, components, creator, created, description, reporter, status, resolution
#' resolutiondate, assignee, updated, comment, priority, votes, watches, versions, fixVersions, labels
#'
#' which are the default parameters of \code{\link{download_jira_issues}}. If the `comment` field is
#' specified, then the comments table is included.
#'
#' If a field is not present in an issue, then its value will be NA.
#'
#'
#' @param json_folder_path is a folder path containing a set of jira_issues as json files.
#' @return A named list of two named elements ("issues", and "comments"), each containing a data.table.
#' @export
#' @family parsers
parse_jira <- function(json_folder_path){

  file_list <- list.files(json_folder_path)

  if (identical(file_list, character(0))){
    stop(stringi::stri_c("cannot open the connection"))
  }

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

  # Issues parser
  jira_parse_issues <- function(jira_file){

    json_issue_comments <- jsonlite::read_json(jira_file)

    n_issues <- length(json_issue_comments[["issues"]])

    # Prepare two lists which will contain data.tables for all issues and all comments
    # Both tables can share the issue_key, so they can be joined if desired.
    all_issues <- list()
    all_issues_comments <- list()

    for(i in 1:n_issues){

      # This is the issue key
      issue_key <- json_issue_comments[["issues"]][[i]][["key"]][[1]]

      # All other information is contained in "fields"
      issue_comment <- json_issue_comments[["issues"]][[i]][["fields"]]

      # Parse all relevant *issue* fields
      all_issues[[i]] <- data.table(
        issue_key = issue_key,

        issue_summary = issue_comment[["summary"]][[1]],
        issue_parent = issue_comment[["parent"]][["name"]][[1]],
        issue_type = issue_comment[["issuetype"]][["name"]][[1]],
        issue_status = issue_comment[["status"]][["statusCategory"]][["name"]][[1]],
        issue_resolution = issue_comment[["resolution"]][["name"]][[1]],
        issue_components = stringi::stri_c(unlist(sapply(issue_comment[["components"]],"[[","name")),collapse = ";"),
        issue_description = issue_comment[["description"]][[1]],
        issue_priority = issue_comment[["priority"]][["name"]][[1]],
        issue_affects_versions = stringi::stri_c(unlist(sapply(issue_comment[["versions"]],"[[","name")),collapse = ";"),
        issue_fix_versions = stringi::stri_c(unlist(sapply(issue_comment[["fixVersions"]],"[[","name")),collapse = ";"),
        issue_labels = stringi::stri_c(unlist(sapply(issue_comment[["labels"]],"[[",1)),collapse = ";"),
        issue_votes = issue_comment[["votes"]][["votes"]][[1]],
        issue_watchers = issue_comment[["watches"]][["watchCount"]][[1]],

        issue_created_datetimetz = issue_comment[["created"]][[1]],
        issue_updated_datetimetz = issue_comment[["updated"]][[1]],
        issue_resolution_datetimetz = issue_comment[["resolutiondate"]][[1]],

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
      root_of_comments_list <- json_issue_comments[["issues"]][[i]][["fields"]][["comment"]]
      # If root_of_comments_list does not exist, then this is an issue only json, skip parsing
      if(length(root_of_comments_list) > 0){
        comments_list <- json_issue_comments[["issues"]][[i]][["fields"]][["comment"]][["comments"]]
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

  issues_holder <- list()
  comments_holder <- list()

  for(filename in file_list){
    current_json <- paste0(json_folder_path, "/", filename)
    parsed_data <- jira_parse_issues(current_json)
    issues_holder <- append(issues_holder, list(parsed_data[["issues"]]))
    comments_holder <- append(comments_holder, list(parsed_data[["comments"]]))
  }

  issues_holder <- rbindlist(issues_holder, fill=TRUE)
  comments_holder <- rbindlist(comments_holder, fill=TRUE)

  return_info <- list()
  return_info[["issues"]] <- issues_holder
  return_info[["comments"]] <- comments_holder

  return(return_info)
}
#' Parse JIRA current issue
#'
#' Returns the file containing the most current issue in the specified folder.
#'
#' The folder assumes the following convention: "(PROJECTKEY)_(uniextimestamp_lowerbound)_(unixtimestamp_upperbound).json"
#' For example: "SAILUH_1231234_2312413.json". This nomenclature is defined by \code{\link{download_jira_issues}}.
#'
#' @param json_folder_path path to save folder containing JIRA issue and/or comments json files.
#' @return The name of the jira issue file with the latest created date that was created/downloaded for
#' use by the Jira downloader refresher
#' @export
#' @family parsers
parse_jira_latest_date <- function(json_folder_path){
  file_list <- list.files(json_folder_path)
  time_list <- list()

  # Checking if the save folder is empty
  if (identical(file_list, character(0))){
    stop(stringi::stri_c("cannot open the connection"))
  }

  for (j in file_list){
    j <- sub(".*_(\\w+)\\.[^.]+$", "\\1", j)
    j <- as.numeric(j)
    time_list <- append(time_list, j)
  }

  overall_latest_date <- as.character(max(unlist(time_list)))

  latest_issue_file <- grep(overall_latest_date, file_list, value = TRUE)

  return(latest_issue_file)
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
#' Parse Jira Issue RSS XML Feed
#'
#' This function can be used to parse our prior TSE JIRA Issue Feed supplemental material.
#' For new JIRA datasets, please refer to \code{\link{parse_jira}}.
#'
#' @param jira_issues_folderpath path to folder of XML issues (see references in this function docs)
#' @return A data.table containing the parsed XML feed (not the fields are not exhaustive. Refer
#' to the raw data for all possible fields.)
#' @export
#' @family parsers
#' @references W. Mauerer, M. Joblin, D. A. Tamburri, C. Paradis, R. Kazman and S. Apel,
#' "In Search of Socio-Technical Congruence: A Large-Scale Longitudinal Study,"
#' in IEEE Transactions on Software Engineering, vol. 48, no. 8, pp. 3159-3184,
#' 1 Aug. 2022, doi: 10.1109/TSE.2021.3082074.
parse_jira_rss_xml <- function(jira_issues_folderpath){
  #jira_issues_path <- path.expand(jira_issues_path)
  #jira_issue_path <- "../../../../tse_motif_2021/dataset/jira/apex/APEXCORE_2015-7-2_2015-7-9.xml"
  jira_issues_path <- list.files(jira_issues_folderpath,full.names = TRUE)

  parse_jira_rss_issue_item <- function(jira_issue){
    issue_key <- XML::xmlValue(jira_issue[["key"]])
    issue_created <- XML::xmlValue(jira_issue[["created"]])
    issue_updated <- XML::xmlValue(jira_issue[["updated"]])
    issue_resolved <- XML::xmlValue(jira_issue[["resolved"]])
    issue_type <- XML::xmlValue(jira_issue[["type"]])
    issue_status <- XML::xmlValue(jira_issue[["status"]])
    issue_resolution <- XML::xmlValue(jira_issue[["resolution"]])

    return(data.table(issue_key=issue_key,
                      issue_created=issue_created,
                      issue_updated=issue_updated,
                      issue_resolved=issue_resolved,
                      issue_type=issue_type,
                      issue_status=issue_status,
                      issue_resolution=issue_resolution))
  }
  parse_jira_rss_issue <- function(jira_issue_path){
    jira_issue_root <- XML::xmlRoot(XML::xmlTreeParse(jira_issue_path))
    jira_issue_channel <- XML::xmlChildren(jira_issue_root)[[1]]
    jira_issues <- jira_issue_channel[names(jira_issue_channel) %in% "item"]
    issues_dt <- rbindlist(lapply(jira_issues,parse_jira_rss_issue_item))
    return(issues_dt)
  }
  issues_dt <- rbindlist(lapply(jira_issues_path,parse_jira_rss_issue))
}


############## Fake Generator ##############

#' Create JIRA Issue
#'
#' Creates a single JIRA Issue as a list, which can be saved as a JSON with or without comments.
#'
#' @param jira_domain_url URL of JIRA domain (e.g. "https://project.org/jira")
#' @param issue_key issue key of JIRA issue (e.g. "PROJECT-68" or "GERONIMO-6723")
#' @param project_key key of the project that contains the JIRA issue (e.g. "SPARK" or "GERONIMO")
#' @param summary summary of the issue (e.g. "Site Keeps Crashing")
#' @param description more detailed description of issue (e.g. "The program keeps crashing because this reason")
#' @param issue_type type of JIRA issue (e.g. "New Feature", "Task", "Bug")
#' @param resolution name of resolution for issue (e.g. "Fixed")
#' @param priority the name of the priority of the issue (e.g. "Major", "Minor", "Trivial")
#' @param status status of issue for development (e.g. "In Progress")
#' @param labels the labels of the project (e.g. "message", "mail", "jira")
#' @param components list of components of issue (e.g. c("PS", "Tests"))
#' @param affects_versions list of affected versions (e.g. c("3.1.6", "4.1.0"))
#' @param fix_versions list of fixed versions (e.g. c("3.1.5", "4.0.0"))
#' @param assignee_name name of person the issue is being assigned to (e.g. "Joe Schmo")
#' @param creator_name name of creator of issue (e.g. "John Doe")
#' @param reporter_name name of reporter of issue (e.g. "Jane Doe")
#' @param comments character list where each element is a comment string (e.g. c("This is first comment", "This is second comment"))
#' @return A list which represents the JIRA JSON in memory
#' @export
#' @family {unittest}
make_jira_issue <- function(jira_domain_url, issue_key, project_key, summary, description, issue_type,
                            resolution, priority, status, labels, components, affects_versions, fix_versions,
                            assignee_name, creator_name, reporter_name, comments = NULL) {

  # Create an issue with the given parameters as a list. If comments are specified, then add comments to the list
  fields <- list(
    parent = create_parent(jira_domain_url, issue_key, status, priority, issue_type),
    fixVersions = create_fix_versions(jira_domain_url, fix_versions),
    resolution = create_resolution(name = resolution),
    priority = create_priority(jira_domain_url, priority),
    labels = labels,
    versions = create_versions(jira_domain_url, affects_versions),
    assignee = create_assignee(jira_domain_url, assignee_name),
    status = create_status(jira_domain_url, status),
    components = create_components(jira_domain_url, components),
    creator = create_creator(jira_domain_url, creator_name),
    reporter = create_reporter(jira_domain_url, reporter_name),
    votes = create_votes(jira_domain_url, issue_key),
    issuetype = create_issue_type(jira_domain_url, issue_type),
    project = create_project(jira_domain_url, project_key),
    resolutiondate = "2007-08-13T19:12:33.000+0000",
    watches = create_watches(jira_domain_url, issue_key),
    created = "2007-07-08T06:07:06.000+0000",
    updated = "2008-05-12T08:01:39.000+0000",
    description = description,
    summary = summary
  )

  if (!is.null(comments) && length(comments) > 0) {

    fields[["comment"]][["comments"]] <- create_issue_comments(comments)
    fields[["comment"]][["maxResults"]] <- length(fields[["comment"]][[1]])
    fields[["comment"]][["total"]] <- length(fields[["comment"]][[1]])
    fields[["comment"]][["startAt"]] <- 0
  }

  # generate a random id number
  id <- sample(10000000: 99999999, 1)

  # append the id to the Jira doman URL
  self_url <- paste0(jira_domain_url, "/rest/api/2/issue", id)

  # fill in the keys for the issue and append the 'fields' list
  issue <- list(
    expand = "schema, names",
    id = as.character(id),
    self = self_url,
    key = issue_key,
    fields = fields
  )

  return(issue)

}

#' Make Jira Issue Tracker
#'
#' Create a full JIRA Issue Tracker with multiple issues using
#'\code{\link{make_jira_issue}}.
#'
#' @param issues list of issues obtained from cell for \code{\link{make_jira_issue}}.
#' @param save_filepath the filepath where the JSON should be saved, including
#' file name and extension.
#' @return The `save_filepath` specified.
#' @export
make_jira_issue_tracker <- function(issues, save_filepath) {

  # validate input
  if (!is.list(issues)) {
    stop("The issues parameter should be a list of issues.")
  }

  export_issues <- list(
    expand = "schema,names",
    startAt = 0,
    maxResults = 50,
    total = length(issues),
    issues = issues
  )

  jsonlite::write_json(export_issues, save_filepath, auto_unbox=TRUE)
  return(save_filepath)
}

#' Create Issue Comments
#'
#' Create issue comments cell for \code{\link{make_jira_issue}}.
#' Other parameters associated to the comments, such as the author
#' and update author are currently hardcoded.
#'
#' @param comments A character list containing the comment body.
#' @return A list named 'comments_list' that has a list of comments
#' @keywords internal
create_issue_comments <- function(comments) {
  comments_list <- list()

  # go through and make comment for each body in comment_bodies
  # only comment bodies changes for comments, the rest of comments information is hard coded below
  for (body in comments) {
    comment <- list(
      self = "https://example.com/jira/rest/api/2/issue/10001/comment/1000",
      id = sample(1:1000, 1),
      author = list(
        self = "https://example.com/jira/rest/api/2/user?username=user1",
        name = "user1",
        key = "user1",
        avatarUrls = list(
          "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user1",
          "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user1",
          "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user1",
          "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user1"
        ),
        displayName = "User One",
        active = TRUE,
        timeZone = "Etc/UTC"
      ),
      body = body,
      updateAuthor = list(
        self = "https://example.com/jira/rest/api/2/user?username=user2",
        name = "user2",
        key = "user2",
        avatarUrls = list(
          "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user2",
          "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user2",
          "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user2",
          "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user2"
        ),
        displayName = "User Two",
        active = TRUE,
        timeZone = "America/New_York"
      ),
      created = "2021-01-01T10:00:00.000+0000",
      updated = "2021-01-01T12:00:00.000+0000"
    )
    comments_list[[length(comments_list) + 1]] <- comment
  }

  return(comments_list)
}

#' Create Issue Type
#'
#' Create issue type cell for \code{\link{make_jira_issue}}. This represents the 'Type'
#' label in JIRA
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_type name of the issue type (e.g. New Feature)
#' @return A list named 'issue_type' that represents the issue type of the JIRA issue
#' @keywords internal
create_issue_type <- function(jira_domain_url, issue_type) {

  issue_id <- sample(1:10, 1)
  self_url <- paste0(jira_domain_url, "/rest/api/", issue_id, "/issuetype/", issue_id)

  issue_type <- list(
    self = self_url,
    id = issue_id,
    description = "A new feature of the product, which has yet to be developed.",
    iconUrl = "https://domain.org/jira/secure/viewavatar?size=xsmall&avatarId=21141&avatarType=issuetype",
    name = issue_type,
    subtask = FALSE,
    avatarId = 21141
  )

  return(issue_type)
}

#' Create Components
#'
#' Creates the component cells for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param components list of names of components
#' @return A list named 'components' which contains each component and its details
#' @keywords internal
create_components <- function(jira_domain_url, components) {
  components_list <- list()

  # for loop to create a component for each component name
  for (name in components) {

    id <- sample(10000: 99999999, 1)

    self_url <- paste0(jira_domain_url, "/rest/api/2/component/", id)

    component <- list(
      self = self_url,
      id = as.character(id),
      name = name,
      description = "This is the description for the component"
    )

    # add component to list which will be returned at the end
    components_list[[length(components_list) + 1]] <- component
  }

  return(components_list)
}

#' Create Creator
#'
#' Creates the issue creator cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param creator_name name of creator
#' @return A list named 'creator' that has creator's information
#' @keywords internal
create_creator <- function(jira_domain_url, creator_name) {
  self_url <- paste0(jira_domain_url, "/rest/api/2/user?username=", creator_name)

  avatarUrls = list(
    "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user1",
    "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user1",
    "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user1",
    "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user1"
  )

  creator <- list(
    self = self_url,
    name = "user_id",
    key = "user_id",
    avatarUrls = avatarUrls,
    displayName = creator_name,
    active = TRUE,
    timeZone = "Etc/UTC"
  )

  return(creator)
}

#' Create Reporter
#'
#' Creates a reporter cell for \code{\link{make_jira_issue}}
#'
#' @param jira_domain_url URL of JIRA domain
#' @param reporter_name name of reporter
#' @return A list named 'reporter' which contains the reporter's information
#' @keywords internal
create_reporter <- function(jira_domain_url, reporter_name) {

  self_url <- paste0(jira_domain_url, "/rest/api/2/user?username=", reporter_name)

  avatarUrls = list(
    "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user1",
    "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user1",
    "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user1",
    "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user1"
  )

  reporter <- list(
    self = self_url,
    name = "user_id",
    key = "user_id",
    avatarUrls = avatarUrls,
    displayName = reporter_name,
    active = TRUE,
    timeZone = "Etc/UTC"
  )

  return(reporter)
}

#' Create Resolution
#'
#' Creates a resolution cell for \code{\link{make_jira_issue}}.
#'
#' @param self_url URL of API endpoint
#' @param id ID associated with resolution
#' @param description Description of resolution
#' @param name Name of Resolution
#' @return A list named 'resolution' which contains the resolution's information
#' @keywords internal
create_resolution <- function(self_url = "https://domain.org/jira/rest/api/2/resolution/1",
                              id = "1",
                              description = "A fix for this issue is checked into the tree and tested.",
                              name = "Fixed") {
  resolution <- list(
    self = self_url,
    id = id,
    description = description,
    name = name
  )

  return(resolution)
}

#' Create Assignee
#'
#' Creates an assignee cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param assignee_name name of assignee
#' @return A list named 'assignee' which contains the assignee's information
#' @keywords internal
create_assignee <- function(jira_domain_url, assignee_name) {

  self_url <- paste0(jira_domain_url, "/rest/api/2/user?username=", assignee_name)

  avatarUrls = list(
    "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user1",
    "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user1",
    "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user1",
    "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user1"
  )

  assignee <- list(
    self = self_url,
    name = "user_id",
    key = "user_id",
    avatarUrls = avatarUrls,
    displayName = assignee_name,
    active = TRUE,
    timeZone = "Etc/UTC"
  )

  return(assignee)
}

#' Create Status
#'
#' Creates a status cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param status description of status
#' @return A list named 'status' containing the status of the issue
#' @keywords internal
create_status <- function(jira_domain_url, status) {

  status_id <- sample(1:10, 1)
  status_category_id <- sample(1:10, 1)

  self_url <- paste0(jira_domain_url, "/rest/api/2/status/", status_id)
  statusCategory_self_url <- paste0(jira_domain_url, "/rest/api/2/statuscategory/", status_category_id)

  status <- list(
    self = self_url,
    description = "The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened.",
    iconUrl = "https://domain.org/jira/images/icons/statuses/closed.png",
    name = status,
    id = as.character(status_id),
    statusCategory = list(
      self = statusCategory_self_url,
      id = status_category_id,
      key = "done",
      colorName = "green",
      name = "Done"
    )
  )

  return(status)
}

#' Create Fix Version
#'
#' Create a fixVersions cell for \code{\link{make_jira_issue}}. This represents the
#' 'Fixed Version/s' label in JIRA
#'
#' @param jira_domain_url URL of JIRA domain
#' @param fix_versions list of fixed versions for the issue
#' @return A list named 'fixVersions' with a list of fixed versions and version information
#' @keywords internal
create_fix_versions <- function(jira_domain_url, fix_versions) {

  fixVersions_list <- list()

  for(fix_version in fix_versions){
    id <- sample(10000000: 99999999, 1)
    self_url <- paste0(jira_domain_url, "/rest/api/2/version/", id)

    version <- list(
      self = self_url,
      id = as.character(id),
      description = "This is a description of the fixVersion",
      name = fix_version,
      archived = FALSE,
      released = TRUE,
      releaseDate = "2021-01-01T10:00:00.000+0000"
    )

    fixVersions_list[[length(fixVersions_list) + 1]] <- version
  }

  return(fixVersions_list)
}

#' Create Priority
#'
#' Create a priority cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param priority the name of the priority of the issue (Major, Minor, Trivial)
#' @return A list named 'priority' containing the priority of the issue
#' @keywords internal
create_priority <- function(jira_domain_url, priority) {

  id <- sample(1:10, 1)

  self_url <- paste0(jira_domain_url, "/rest/api/2/priority/", id)

  priority <- list(
    self = self_url,
    iconUrl = "https://issues.apache.org/jira/images/icons/priorities/major.svg",
    name = priority,
    id = as.character(id)
  )

  return(priority)
}

#' Create Parent
#'
#' Create a parent cell for \code{\link{make_jira_issue}}. Currently, the parent has the same
#' issue_key, status, priority, and issue_type as the base issue
#'
#' @param jira_domain_url URL of JIRA domain
#' @param parent_issue_key issue key of the parent issue of the current JIRA issue
#' @param status status of issue for development
#' @param priority the name of the priority of the issue
#' @param issue_type type of JIRA issue
#' @return A list named 'parent' that contains a parent issue and it's fields
#' @keywords internal
create_parent <- function(jira_domain_url, issue_key, status, priority, issue_type) {

  id <- sample(10000000: 99999999, 1)

  self_url <- paste0(jira_domain_url, "/rest/api/2/issue/", id)

  fields <- list(
    summary = "This is a summary",
    status = create_status(jira_domain_url, status),
    priority = create_priority(jira_domain_url, priority),
    issuetype = create_issue_type(jira_domain_url, issue_type)
  )

  parent <- list(
    id = as.character(id),
    key = issue_key,
    self = self_url,
    fields = fields
  )

  return(parent)
}

#' Create Project
#'
#' Create a project cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param project_key key of the project that contains the JIRA issue (e.g. "SPARK" or "GERONIMO")
#' @return A list named 'project' that contains the project's information
#' @keywords internal
create_project <- function(jira_domain_url, project_key) {

  id <- sample(10000000: 99999999, 1)

  self_url <- paste0(jira_domain_url, "/rest/api/2/project/", id)

  avatarUrls = list(
    "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user1",
    "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user1",
    "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user1",
    "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user1"
  )

  project <- list(
    self = self_url,
    id = as.character(id),
    key = project_key,
    name = project_key,
    projectTypeKey = "software",
    avartarUrls = avatarUrls
  )
}

#' Create Versions
#'
#' Create a versions cell for \code{\link{make_jira_issue}}. This cell represents
#' the 'Affects Version/s' label in JIRA
#'
#' @param jira_domain_url URL of JIRA domain
#' @param affects_versions list of version names for the issue
#' @return A list named 'versions' with a list of versions
#' @keywords internal
create_versions <- function(jira_domain_url, affects_versions) {

  versions_list <- list()

  for(affects_version in affects_versions){
    id <- sample(10000000: 99999999, 1)
    self_url <- paste0(jira_domain_url, "/rest/api/2/version/", id)

    version <- list(
      self = self_url,
      id = as.character(id),
      description = "This is a description of the version",
      name = affects_version,
      archived = FALSE,
      released = TRUE,
      releaseDate = "2024-01-01T10:00:00.000+0000"
    )

    versions_list[[length(versions_list) + 1]] <- version
  }

  return(versions_list)
}

#' Create Votes
#'
#' Create a votes cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_key issue key of JIRA issue
#' @return A list named 'votes' that has the number of votes for the issue
#' @keywords internal
create_votes <- function(jira_domain_url, issue_key) {

  self_url <- paste0(jira_domain_url, "/rest/api/2/issue/", issue_key, "votes")

  votes <- list(
    self = self_url,
    votes = 10,
    hasVoted = FALSE
  )

  return(votes)
}

#' Create Watches
#'
#' Create a watches cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_key issue key of JIRA issue
#' @return A list named 'watches' that has the number of watchers for the issue
#' @keywords internal
create_watches <- function(jira_domain_url, issue_key) {

  self_url <- paste0(jira_domain_url, "/rest/api/2/issue/", issue_key, "watchers")

  watches <- list(
    self = self_url,
    watchCount = 15,
    isWatching = FALSE
  )

  return(watches)
}
