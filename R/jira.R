# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Create JirAgileR Issue
#'
#' Creates a single JIRA Issue as a list, which can be saved as a JSON with or without comments.
#' Note the JSON follows the format used by JirAgileR package when downloading
#' JSONs, instead of the format specified by JIRA.
#'
#' @param jira_domain_url URL of JIRA domain (e.g. "https://project.org/jira")
#' @param issue_key issue key of JIRA issue (e.g. "PROJECT-68" or "GERONIMO-6723)
#' @param issue_type type of JIRA issue (e.g. "New Feature", "Task", "Bug")
#' @param status status of issue for development (e.g. "In Progress")
#' @param resolution name of resolution for issue (e.g. "Fixed")
#' @param title summary of the issue (e.g. "Site Keeps Crashing")
#' @param description more detailed description of issue (e.g. "The program keeps crashing because this reason")
#' @param components components of issue separate by ; (e.g. "x-core;x-spring")
#' @param creator_name name of creator of issue (e.g. "John Doe")
#' @param reporter_name name of reporter of issue (e.g. "Jane Doe")
#' @param assignee_name name of person the issue is being assigned to (e.g. "Joe Schmo")
#' @param comments character vector where each element is a comment string (e.g. c("This is first comment", "This is second comment"))
#' @return A list which represents the JirAgileR JSON in memory
#' @export
#' @family {unittest}
make_jira_issue <- function(jira_domain_url, issue_key, issue_type, status, resolution, title, description, components, creator_name, reporter_name, assignee_name, comments = NULL) {

  # An issue in the JirAgileR format contains an `base_info_cell`
  # and an `ext_info_cell`. This function calls the appropriate
  # internal functions to define both blocks, and add
  issues <- list()

  # Create `base_info_cell`
  base_info_cell <- create_base_info(jira_domain_url, issue_key)
  issues[["base_info"]][[1]] <- base_info_cell

  # Create `ext_info_cell`
  ext_info_cell <- create_ext_info(jira_domain_url, issue_type, status, resolution, title, description, components, creator_name, reporter_name, assignee_name)

  # Create `comment` if specified
  if (!is.null(comments) && length(comments) > 0) {

    ext_info_cell[["comment"]][["comments"]] <- create_issue_comments(comments)
    ext_info_cell[["comment"]][["maxResults"]] <- length(ext_info_cell[["comment"]][[1]])
    ext_info_cell[["comment"]][["total"]] <- length(ext_info_cell[["comment"]][[1]])
    ext_info_cell[["comment"]][["startAt"]] <- 0
  }

  issues[["ext_info"]][[1]] <- ext_info_cell

  #folder_path <- "/tmp"
  #jira_json_path <- file.path(folder_path,"fake_issues.json")
  #jsonlite::write_json(issues,file.path(folder_path,"fake_issues.json"))

  return(issues)

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
make_jira_issue_tracker <- function(issues,save_filepath) {

  # validate input
  if (!is.list(issues)) {
    stop("The issues parameter should be a list of issues.")
  }

  issue_tracker <- list()
  issue_tracker_base_list <- list()
  issue_tracker_ext_list <- list()

  # Flatten function to format base_info properly for tracker format
  flatten_base_info <- function(base_info) {
    flattened_base_info <- list(
      id = as.character(base_info[[1]][[1]]),
      self = base_info[[1]][[2]],
      key = base_info[[1]][[3]],
      JirAgileR_id = as.numeric(base_info[[1]][[4]])
    )

    # return final flattened base_info list
    return(flattened_base_info)
  }

  # Flatten functions to format ext_info properly for tracker format
  flatten_ext_info <- function(ext_info) {
    flattened_ext_info <- list()

    # title
    flattened_ext_info$title <- ext_info[[1]][[1]][[1]]

    # issue_type
    flatten_issuetype <- function(issuetype) {
      flattened_issuetype <- list()

      flattened_issuetype$self <- issuetype[[1]][[1]][[1]]
      flattened_issuetype$id <- as.character(issuetype[[2]][[1]][[1]])
      flattened_issuetype$description <- issuetype[[3]][[1]][[1]]
      flattened_issuetype$iconUrl <- issuetype[[4]][[1]]
      flattened_issuetype$name <- issuetype[[5]][[1]]
      flattened_issuetype$subtask <- issuetype[[6]][[1]]
      flattened_issuetype$avatarId <- issuetype[[7]][[1]]

      return(flattened_issuetype)
    }

    flattened_ext_info$issuetype <- flatten_issuetype(ext_info[[1]][[2]])

    # components
    flatten_components <- function(components) {
      flattened_components <- list()

      for(i in seq_along(components)) {
        component <- components[[i]]
        flattened_component <- list()
        flattened_component$self <- component[[1]][[1]]
        flattened_component$id <- component[[2]][[1]]
        flattened_component$name <- component[[3]][[1]]
        flattened_components[[i]] <- flattened_component
      }

      return(flattened_components)
    }

    flattened_ext_info$components <- flatten_components(ext_info[[1]][[3]])

    # creator
    flattened_ext_info$creator <- ext_info[[1]][[4]]

    # created
    flattened_ext_info$created <- ext_info[[1]][[5]][[1]]

    # description
    flattened_ext_info$description <- ext_info[[1]][[6]]

    # reporter
    flatten_reporter <- function(reporter) {
      flattened_reporter <- list()
      flattened_reporter$self <- reporter[[1]][[1]]
      flattened_reporter$name <- reporter[[2]][[1]]
      flattened_reporter$key <- reporter[[3]][[1]]
      flattened_reporter$avatarUrls <- reporter[[4]]
      flattened_reporter$displayName <- reporter[[5]][[1]]
      flattened_reporter$active <- reporter[[6]][[1]]
      flattened_reporter$timeZone <- reporter[[7]][[1]]

      return(flattened_reporter)
    }

    flattened_ext_info$reporter <- flatten_reporter(ext_info[[1]][[7]])

    # resolution
    flatten_resolution <- function(resolution) {
      flattened_resolution <- list()
      flattened_resolution$self <- resolution[[1]][[1]]
      flattened_resolution$id <- resolution[[2]][[1]]
      flattened_resolution$description <- resolution[[3]][[1]]
      flattened_resolution$name <- resolution[[4]][[1]]

      return(flattened_resolution)
    }

    flattened_ext_info$resolution <- flatten_resolution(ext_info[[1]][[8]])

    #resolutiondate
    flattened_ext_info$resolutiondate <- ext_info[[1]][[9]]

    # comments
    if (length(ext_info[[1]]) >= 13) {
      flattened_ext_info$comment <- ext_info[[1]][[13]]
    }

    # assignee
    flatten_assignee <- function(assignee) {
      flattened_assignee <- list()
      flattened_assignee$self <- assignee[[1]][[1]]
      flattened_assignee$name <- assignee[[2]][[1]]
      flattened_assignee$key <- assignee[[3]][[1]]
      flattened_assignee$avatarUrls <- assignee[[4]]
      flattened_assignee$displayName <- assignee[[5]][[1]]
      flattened_assignee$active <- assignee[[6]][[1]]
      flattened_assignee$timeZone <- assignee[[7]][[1]]

      return(flattened_assignee)
    }

    flattened_ext_info$assignee <- flatten_reporter(ext_info[[1]][[10]])

    # updated
    flattened_ext_info$updated <- ext_info[[1]][[11]][[1]]

    # status
    flatten_status <- function(status) {
      flattened_status <- list()
      flattened_status$self <-status[[1]][[1]]
      flattened_status$description <- status[[2]][[1]]
      flattened_status$iconUrl <- status[[3]][[1]]
      flattened_status$name <- status[[4]]
      flattened_status$id <- status[[5]][[1]]

      statusCategory <- list()
      statusCategory$self <- status[[6]][[1]][[1]]
      statusCategory$id <- status[[6]][[2]][[1]]
      statusCategory$key <- status[[6]][[3]][[1]]
      statusCategory$colorName <- status[[6]][[4]][[1]]
      statusCategory$name <- status[[6]][[5]][[1]]

      flattened_status$statusCategory <- statusCategory

      return(flattened_status)
    }

    flattened_ext_info$status <- flatten_status(ext_info[[1]][[12]])

    # return final flattened ext_info list
    return(flattened_ext_info)
  }

  # loop through each issue
  for(issue in issues) {
    # get base & ext info for each issue
    base_info <- flatten_base_info(issue[["base_info"]])
    ext_info <- flatten_ext_info(issue[["ext_info"]])

    # combine each new base & ext list to respective list
    issue_tracker_base_list <- c(issue_tracker_base_list, list(base_info))
    issue_tracker_ext_list <- c(issue_tracker_ext_list, list(ext_info))
  }

  # combine both lists to create final tracker
  issue_tracker[["base_info"]] <- issue_tracker_base_list
  issue_tracker[["ext_info"]] <- issue_tracker_ext_list

  jsonlite::write_json(issue_tracker,save_filepath)

  return(save_filepath)
}


#' Create base_info_cell
#'
#' Creates and formats base_info_cell list for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_key key for JIRA issue
#' @return A list named 'base_info_cell' containing all information for base cell in json file
create_base_info <- function(jira_domain_url, issue_key) {
  id <- sample(1:10, 1)
  JirAgileR_id <- sample(1:10, 1)

  # Construct the issue API URL from the domain URL and issue key
  issue_api_url <- paste0(jira_domain_url, "/rest/api/latest/issue/", id)

  base_info_cell <- list(
    id = id,
    self = issue_api_url,
    key = issue_key,
    JirAgileR_id = JirAgileR_id
  )

  return(base_info_cell)
}

#' Create ext_info_cel
#'
#' Creates and formats ext_info_cell list \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_type description of issue_type
#' @param status status of issue for development
#' @param resolution name of resolution for issue
#' @param title summary of the issue
#' @param description description of issue
#' @param components components of issue, a list with component names separated by ; (ex. "x-core;x-spring" is two components)
#' @param creator_name name of creator of issue
#' @param reporter_name name of reporter reporting the issue
#' @param assignee_name name of person the issue is being assigned to
#' @return A list named 'ext_info_cell' which contains all the parameters and its generated fake data formats
create_ext_info <- function(jira_domain_url, issue_type, status, resolution, title, description, components, creator_name, reporter_name, assignee_name) {

  ext_info_cell <- list(
    title = list(title),
    issuetype = create_issue_type(jira_domain_url, issue_type),
    components = create_components(jira_domain_url, components),
    creator = create_creator(jira_domain_url, creator_name),
    created = list("2007-07-08T06:07:06.000+0000"),
    description = description,
    reporter = create_reporter(jira_domain_url, reporter_name),
    resolution = create_resolution(name = resolution),
    resolutiondate = "2007-08-13T19:12:33.000+0000",
    assignee = create_assignee(jira_domain_url, assignee_name),
    updated = list("2008-05-12T08:01:39.000+0000"),
    status = create_status(jira_domain_url, status)
  )

  return(ext_info_cell)
}

#' Create Issue Comments
#'
#' Create issue comments cell for \code{\link{make_jira_issue}}.
#' Other parameters associated to the comments, such as the author
#' and update author are currently hardcoded.
#'
#' @param comments A character vector containing the comment body.
create_issue_comments <- function(comments) {
  comments_vector <- list()

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
    comments_vector[[length(comments_vector) + 1]] <- comment
  }

  return(comments_vector)
}

#' Create Issue Type
#'
#' Create issue type cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_type name of the issue type (e.g. New Feature)
#' @return A list named 'issue_type' that represents the issue type of the JIRA issue
create_issue_type <- function(jira_domain_url, issue_type) {

  issue_id <- sample(1:10, 1)
  self_url <- paste0(jira_domain_url, "/rest/api/", issue_id, "/issuetype/", issue_id)

  issue_type <- list(
    self = list(list(self_url)),
    id = list(list(issue_id)),
    description = list(list("A new feature of the product, which has yet to be developed.")),
    iconUrl = list("https://domain.org/jira/secure/viewavatar?size=xsmall&avatarId=21141&avatarType=issuetype"),
    name = list(issue_type),
    subtask = list(FALSE),
    avatarId = list(21141)
  )

  return(issue_type)
}

#' Create Components
#'
#' Creates the component cells for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param components string of names of components (ex. "x-core;x-spring" is two components)
#' @return A list named 'components' which contains each component and its details
create_components <- function(jira_domain_url, components) {

  # separate components names with ; (ex. "x-core;x-spring" is two components)
  components_names <- unlist(stringi::stri_split_regex(components, pattern = ";"))
  components_list <- list()

  # for loop to create a component for each component name
  for (name in components_names) {
    id <- sample(10000000: 99999999, 1)
    self_url <- paste0(jira_domain_url, "/rest/api/2/component/", id)

    component <- list(
      self = list(self_url),
      id = list(as.character(id)),
      name = list(name)
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
create_reporter <- function(jira_domain_url, reporter_name) {

  self_url <- paste0(jira_domain_url, "/rest/api/2/user?username=", reporter_name)

  avatarUrls = list(
    "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user1",
    "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user1",
    "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user1",
    "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user1"
  )

  reporter <- list(
    self = list(self_url),
    name = "user_id",
    key = "user_id",
    avatarUrls = avatarUrls,
    displayName = list(reporter_name),
    active = list(TRUE),
    timeZone = list("Etc/UTC")
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
create_resolution <- function(self_url = "https://domain.org/jira/rest/api/2/resolution/1",
                              id = "1",
                              description = "A fix for this issue is checked into the tree and tested.",
                              name = "Fixed") {
  resolution <- list(
    self = list(self_url),
    id = list(id),
    description = list(description),
    name = list(name)
  )

  return(resolution)
}

#' Create Assignee
#'
#' Creates a assignee cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param assignee_name name of assignee
#' @return A list named 'assignee' which contains the assignee's information
create_assignee <- function(jira_domain_url, assignee_name) {

  self_url <- paste0(jira_domain_url, "/rest/api/2/user?username=", assignee_name)

  avatarUrls = list(
    "48x48" = "https://example.com/jira/secure/useravatar?size=large&ownerId=user1",
    "24x24" = "https://example.com/jira/secure/useravatar?size=small&ownerId=user1",
    "16x16" = "https://example.com/jira/secure/useravatar?size=xsmall&ownerId=user1",
    "32x32" = "https://example.com/jira/secure/useravatar?size=medium&ownerId=user1"
  )

  assignee <- list(
    self = list(self_url),
    name = "user_id",
    key = "user_id",
    avatarUrls = avatarUrls,
    displayName = list(assignee_name),
    active = list(TRUE),
    timeZone = list("Etc/UTC")
  )

  return(assignee)
}

#' Create Status
#'
#' Creates a status cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param status description of status
#' @return A list named 'status' containing status's information
create_status <- function(jira_domain_url, status) {

  status_id <- sample(1:10, 1)
  status_category_id <- sample(1:10, 1)

  self_url <- paste0(jira_domain_url, "/rest/api/2/status/", status_id)
  statusCategory_self_url <- paste0(jira_domain_url, "/rest/api/2/statuscategory/", status_category_id)

  status <- list(
    self = list(self_url),
    description = list("The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened."),
    iconUrl = list("https://domain.org/jira/images/icons/statuses/closed.png"),
    name = status,
    id = as.character(status_id),
    statusCategory = list(
      self = list(statusCategory_self_url),
      id = list(status_category_id),
      key = list("done"),
      colorName = list("green"),
      name = list("Done")
    )
  )

  return(status)
}


#' Define function to fetch issues from JIRA REST API and save as JSON
#'
#' Download issue data from "rest/api/lastest/search" endpoint
#'
#' @param username Atlassian username as a string
#' @param password Atlassian token
#' @param domain Custom JIRA domain URL set in config file
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_path_issue_tracker_issues Path that files will be save along
#' @param maxResults (optional) the maximum number of results to download per page.
#' Default is 50
#' @param verbose boolean flag to specify printing operational
#' messages or not
#' @param maxDownloads Maximum downloads per function call
#' @param created_latest the maximum value of the 'created' field in existing files
#' This parameter is set to enable refresh capability
#' @export
download_and_save_jira_issues <- function(domain,
                                          username = NULL,
                                          password = NULL,
                                          jql_query,
                                          fields,
                                          save_path_issue_tracker_issues,
                                          maxResults = 50,
                                          verbose = FALSE,
                                          maxDownloads = 5000,
                                          created_latest = "'1970-01-01'") {

  # Ensure the domain starts with https:// for secure communication.
  if (!grepl("^https?://", domain)) {
    domain <- paste0("https://", domain)
  }

  # if (!is.null(created_latest)){
  #   jql_query <- paste(jql_query, "AND created >= ", created_latest)
  #   message(jql_query)
  # }

  #Initialize variables for pagination

  startAt <- 0
  total <- maxResults
  all_issues <- list()
  # This variable counts your download count. This is important to not exceed the max downloads per hour
  downloadCount <- 0
  time <- Sys.time()
  message("Starting Downloads at ", time)
  # Loop that downloads each issue into a file
  repeat{

    # Check update our download count and see if it is approaching the limit
    if (downloadCount + maxResults > maxDownloads) {
      # erorr message
      time <- Sys.time()
      message("Cannot download as maxDownloads will be exceeeded. Reccommend running again at a later time. Downloads ended at ", time)
      break
    } # Resume donwloading

    # Construct the API endpoint URL
    url <- paste0(domain, "/rest/api/2/search")

    # Authenticate if username and password are provided
    if(length(credentials) >= 2) {
      username <- credentials[1]
      password <- credentials[2]
      # Use the credentials for authentication
      auth <- httr::authenticate(as.character(username), as.character(password), "basic")
    } else {
      if(verbose){
        message("No credentials present.")
      }
      auth <- NULL
    }

    # Prepare query parameters for the API call
    query_params <- list(jql = jql_query, fields = paste(fields, collapse = ","), maxResults = maxResults, startAt = startAt)

    # Make the API call
    response <- httr::GET(url, query = query_params)

    # Stop if there's an HTTP error
    if (httr::http_error(response)) {
      stop("API request failed: ", httr::http_status(response)$message)
    }

    # Extract issues. Append the new issues to all_issues
    content <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    all_issues <- append(all_issues, content$issues)

    #saves each issue to separate file with the issue key and the time it was downloaded.
    #This can of course be changed to use different identifiers. Current naming
    #convention is [save_path_issue_tracker_issues]_[1st-issue-key]-[last-issue-key]_[timestamp]
    #The intention is to state the range of the issues.
    file_name <- save_path_issue_tracker_issues

    if (grepl("\\.json$", file_name)) {
      # Remove .json if present in file_name
      file_name <- sub("\\.json$", "", file_name)
    }
    # naming convention for each page
    for (i in seq_along(content$issues)) {
      if (i == 1){
        issue <- content$issue[[i]]
        issue_key <- issue$key
        file_name <- paste0(file_name, "_", issue_key)
      }
      if (i == maxResults){
        issue <- content$issue[[i]]
        issue_key <- issue$key
        timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
        file_name <- paste0(file_name, "-", issue_key, "_", timestamp, ".json")
      }
      #jsonlite::write_json(content, file_name)
    }

    #write the files
    jsonlite::write_json(content, file_name, auto_unbox=TRUE)

    if (verbose){
      message("saved file to ", file_name)
    }

    downloadCount <- downloadCount + maxResults
    if (verbose){
      message("Saved ", downloadCount, " total issues")
    }

    maxResults <- length(content$issues)

    #updates startat for next loop
    if (length(content$issues) < maxResults) {
      break
    } else {
      startAt <- startAt + maxResults
    }
  }

  # Final verbose output
  if (verbose) {
    message("Success! Fetched and saved issues.")
  }

  # Returns the content so that it can be saved to a variable via function call
  return(all_issues)
}
