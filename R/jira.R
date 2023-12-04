# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Creates a sample jira issue without comment and two components
#'
#' This example replicates Kaiaulu issue #244
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
#' @param comments a character vector where each element is a comment string (e.g. c("This is first comment", "This is second comment"))
#' @return The path to the sample .json file.
#' @export
#' @family {unittest}
make_jira_issue <- function(jira_domain_url, issue_key, issue_type, status, resolution, title, description, components, creator_name, reporter_name, assignee_name, comments = NULL) {

  issues <- list()

  base_info_cell <- create_base_info(jira_domain_url, issue_key)
  issues[["base_info"]][[1]] <- base_info_cell

  ext_info_cell <- create_ext_info(jira_domain_url, issue_type, status, resolution, title, description, components, creator_name, reporter_name, assignee_name)

  if (!is.null(comments) && length(comments) > 0) {

    make_jira_issue_comments <- function(comments) {
      comments_vector <- list()

      # go through and make comment for each body in comment_bodies
      # only comment bodies changes for comments, the rest of comments information is hard coded below
      for (body in comment_bodies) {
        comment <- list(
          self = "https://example.com/jira/rest/api/2/issue/10001/comment/1000",
          id = "1000",
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

      return(list(comments = comments_vector))
    }

    ext_info_cell[["comment"]] <- make_jira_issue_comments(comments_vector)
  }

  issues[["ext_info"]][[1]] <- ext_info_cell

  folder_path <- "/tmp"
  jira_json_path <- file.path(folder_path,"fake_issues.json")
  jsonlite::write_json(issues,file.path(folder_path,"fake_issues.json"))

  return(issues)

}

#' Create base_info_cell
#'
#' Creates and formats base_info_cell for fake Jira issue
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_key key for Jira issue
#' @return A list named 'base_info_cell' containing all information for base cell in json file
#' @export
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

#' Create ext_info_cell for issues
#'
#' Creates and formats ext_info_cell for fake Jira issue
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_type_description description of issue_type
#' @param title issue title
#' @param components components of issue, a list with component names seperated by ; (ex. "x-core;x-spring" is two components)
#' @param description description of issue
#' @param reporter_name name of reporter reporting the issue
#' @param assignee_name name of assignee the issue is being assigned to
#' @param status_description description of the status of the issue
#' @return A list named 'ext_info_cell' which contains all the parameters and its generated fake data formats
#' @export
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
    assignee = list(assignee_name),
    updated = list("2008-05-12T08:01:39.000+0000"),
    status = create_status(jira_domain_url, status)
  )

  return(ext_info_cell)
}

#' Create Issue Type
#'
#' Create issue type cell for fake Jira issue
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_type name of the issue type (e.g. New Feature)
#' @return A list named 'issue_type' that represents the issue type of the JIRA issue
#' @export
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
#' Creates a list populated with components and their information from components_list
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
#' Creates a creator field with information
#'
#' @param URL of JIRA domain
#' @param creator_name name of creator
#' @return A list named 'creator' that has creator's information
#' @export
create_creator <- function(jira_domain_url, creator_name) {
  self_url <- paste0(jira_domain_url, "/rest/api/2/user?username=", creator_name)

  creator <- list(
    self = self_url,
    name = creator_name,
    key = sample(1:10, 1),
    displayName = "Fake User1",
    active = TRUE,
    timeZone = "Etc/UTC"
  )

  return(creator)
}

#' Create Reporter
#'
#' Creates a reporter field with information
#'
#' @param jira_domain_url URL of JIRA domain
#' @param reporter_name name of reporter
#' @return A list named 'reporter' which contains the reporter's information
#' @export
create_reporter <- function(jira_domain_url, reporter_name) {

  self_url <- paste0(jira_domain_url, "/rest/api/2/user?username=", reporter_name)

  reporter <- list(
    self = list(self_url),
    name = list(reporter_name),
    key = list(reporter_name),  # assuming key is the same as the reporter name
    displayName = list("Fake User"),
    active = list(TRUE),
    timeZone = list("Etc/UTC")
  )

  return(reporter)
}

#' Create Resolution
#'
#' Creates a resolution with hard coded information
#'
#' @param self_url URL of API endpoint
#' @param id ID associated with resolution
#' @param description Description of resolution
#' @param name Name of Resolution
#' @return A list named 'resolution' which contains the resolution's information
#' @export
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

#' Create Status
#'
#' Creates a status with description for fake jira generator
#'
#' @param jira_domain_url URL of JIRA domain
#' @param status_description description of status
#' @return A list named 'status' containing status's information
#' @export
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


#' Make Jira Issue Tracker
#'
#' Create a full JIRA Issue Tracker with multiple issues
#'
#' @param issues list of issues that will make up the issue tracker
#' @return A list named 'status' containing status's information
#' @export
make_jira_issue_tracker <- function(issues) {

  # validate input
  if (!is.list(issues)) {
    stop("The issues parameter should be a list of issues.")
  }

  # Loop to convert input to a list of issues
  # issues_list <- list()
  # for (i in seq_along(issues)) {
  #   issues_list[[i]] <- issues[i]
  # }

  issue_tracker <- list()

  # FOR LOOP to make flattened list
  issue_tracker_base_list <- list()
  issue_tracker_ext_list <- list()

  for(issue in issues) {
    issue_tracker_base_list <- c(issue_tracker_base_list, list(issue[["base_info"]][[1]]))
    issue_tracker_ext_list <- c(issue_tracker_ext_list, list(issue[["ext_info"]][[1]]))
  }

  issue_tracker[["base_info"]] <- issue_tracker_base_list
  issue_tracker[["ext_info"]] <- issue_tracker_ext_list

  # ORIGINAL lapply, doesn't work all the way
  # issue_tracker[["base_info"]] <- lapply(issues, "[[", "base_info")
  # issue_tracker[["ext_info"]] <- lapply(issues, "[[", "ext_info")

  folder_path <- "/tmp"
  jira_json_path <- file.path(folder_path,"issue_tracker.json")
  jsonlite::write_json(issue_tracker,file.path(folder_path,"issue_tracker.json"))

  return(jira_json_path)
}

# TESTING PURPOSES - WILL DELETE
#
# issues_vector <- c(list(issue1), list(issue2))
#
# issue_tracker_path <- make_jira_issue_tracker(issues_vector)
#
# comment_bodies <- c(
#   "This is the first body comment",
#   "This is the second body comment"
# )
#
# test call to make_jira_issue_tracker with two issues, no comments
# issue1 <- make_jira_issue(jira_domain_url = "https://project.org/jira",
#                           issue_key = "GERONIMO",
#                           issue_type = "A new feature of the product, which has yet to be developed.",
#                           status = "The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened.",
#                           resolution = "finished",
#                           title = "This is a summary.",
#                           description = "This is a description of the issue.",
#                           components = "x-core;x-spring",
#                           creator_name = "Bob",
#                           reporter_name = "Joe",
#                           assignee_name = "Moe",
# )
#
# issue2 <- make_jira_issue(jira_domain_url = "https://project.org/jira",
#                           issue_key = "2",
#                           issue_type = "A new feature of the product, which has yet to be developed.",
#                           status = "The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened.",
#                           resolution = "finished",
#                           title = "This is a summary.",
#                           description = "This is a description of the issue.",
#                           components = "x-core;x-spring",
#                           creator_name = "Bob",
#                           reporter_name = "Joe",
#                           assignee_name = "Moe",
#                           comment_bodies
# )



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
jira_delete_sample_log <- function(folder_path="/tmp"){
  folder_path <- path.expand(folder_path)
  jira_json_path <- file.path(folder_path,"fake_issues.json")
  error <- system2('rm',
                   args = c(jira_json_path),
                   stdout = TRUE,
                   stderr = FALSE)
}
