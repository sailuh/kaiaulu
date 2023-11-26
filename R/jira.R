# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Creates a sample jira issue without comment and two components
#'
#' This example replicates Kaiaulu issue #244
#'
#' @return The path to the sample .json file.
#' @export
#' @family {unittest}
make_jira_issue <- function(jira_domain_url, issue_key, issue_type_description, summary, components, description, reporter_name, assignee_name, status_description) {

  issues <- list()

  base_info_cell <- create_base_info(jira_domain_url, issue_key)
  issues[["base_info"]][[1]] <- base_info_cell

  ext_info_cell <- create_ext_info(jira_domain_url, issue_type_description, summary, components, description, reporter_name, assignee_name, status_description)
  issues[["ext_info"]][[1]] <- ext_info_cell

  folder_path <- "/tmp"
  jira_json_path <- file.path(folder_path,"fake_issues.json")
  jsonlite::write_json(issues,file.path(folder_path,"fake_issues.json"))

  return(issues)

}

# same thing as make_jira_issue, but with optional comments field
make_jira_issue_comment <- function(jira_domain_url, issue_key, issue_type_description, summary, components, description, reporter_name, assignee_name, status_description, comments_body) {

  issues <- list()

  base_info_cell <- create_base_info(jira_domain_url, issue_key)
  issues[["base_info"]][[1]] <- base_info_cell

  ext_info_cell <- create_ext_info(jira_domain_url, issue_type_description, summary, components, description, reporter_name, assignee_name, status_description)
  ext_info_cell[["comment"]] <- create_comments(comments_body)

  issues[["ext_info"]][[1]] <- ext_info_cell

  folder_path <- "/tmp"
  jira_json_path <- file.path(folder_path,"fake_issues_comments.json")
  jsonlite::write_json(issues,file.path(folder_path,"fake_issues_comments.json"))

  return(issues)

}

#' Create base_info_cell
#'
#' Creates and formats base_info_cell for fake Jira issue
#'
#' @param jira_domain_url URL of JIRA
#' @param issue_key key for Jira issue
#' @return A list named 'base_info_cell' containing all the parameters
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

#' Create ext_info_cell for issues (without comments)
#'
#' Creates and formats ext_info_cell for fake Jira issue
#'
#' @return ext info cell
#' @export
create_ext_info <- function(jira_domain_url, issue_type_description, summary, components, description, reporter_name, assignee_name, status_description) {

  ext_info_cell <- list(
    summary = list(summary),
    issuetype = create_issue_type(jira_domain_url, issue_type_description),
    components = create_components(jira_domain_url, components),
    creator = create_creator(),
    created = list("2007-07-08T06:07:06.000+0000"),
    description = list(description),
    reporter = create_reporter(jira_domain_url, reporter_name),
    resolution = create_resolution(),
    resolutiondate = list("2007-08-13T19:12:33.000+0000"),
    assignee = list(assignee_name),
    updated = list("2008-05-12T08:01:39.000+0000"),
    status = create_status(jira_domain_url, status_description)
  )

  return(ext_info_cell)
}

#' Create Issue Type
#'
#' Create issue type cell for fake Jira issue
#'
#' @param jira_domain_url URL of JIRA
#' @param issue_type_description description of the issue type
#' @return A list named 'issue_type' that represents the issue type of the Jira issue
#' @export
create_issue_type <- function(jira_domain_url, issue_type_description) {

  issue_id <- sample(1:10, 1)
  self_url <- paste0(jira_domain_url, "/rest/api/", issue_id, "/issuetype/", issue_id)

  issue_type <- list(
    self = list(list(self_url)),
    id = list(list(issue_id)),
    description = list(list(issue_type_description)),
    iconUrl = list("https://domain.org/jira/secure/viewavatar?size=xsmall&avatarId=21141&avatarType=issuetype"),
    name = list("New Feature"),
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
#' @return A list named 'components' which contains each component
create_components <- function(jira_domain_url, components) {

  # separate components names with ; (ex. "x-core;x-spring" is two components)
  components_names <- unlist(strsplit(components, ";"))
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

    # add individual component to components_list
    components_list[[length(components_list) + 1]] <- component
  }

  return(components_list)
}

#' Create Creator
#'
#' Creates a creator field with information
#'
#' @param self_url URL of API endpoint
#' @param name Name of creator
#' @param key Key associated with creator
#' @param displayName Display name of creator
#' @param active TRUE, indicates if creator's account is active or not
#' @param timeZone Timezone associated with creator
#' @return A list named 'creator' that has creator's information
#' @export
create_creator <- function(self_url = "https://domain.org/jira/rest/api/2/user?username=user1",
                           name = "user1",
                           key = "user1",
                           displayName = "Fake User1",
                           active = TRUE,
                           timeZone = "Etc/UTC") {
  creator <- list(
    self = list(self_url),
    name = list(name),
    key = list(key),
    displayName = list(displayName),
    active = list(active),
    timeZone = list(timeZone)
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
    key = list(reporter_name),  # Assuming key is the same as the reporter name
    displayName = list("Fake User"),
    active = list(TRUE),
    timeZone = list("Etc/UTC")
  )

  return(reporter)
}

#' Create Resolution
#'
#' Creates a resolution with information
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
#' Creates a status with information
#'
#' @param jira_domain_url URL of JIRA domain
#' @param status_description description of status
#' @return A list named 'status' containing status's information
#' @export
create_status <- function(jira_domain_url, status_description) {

  status_id <- sample(1:10, 1)
  status_category_id <- sample(1:10, 1)

  self_url <- paste0(jira_domain_url, "/rest/api/2/status/", status_id)
  statusCategory_self_url <- paste0(jira_domain_url, "/rest/api/2/statuscategory/", status_category_id)

  status <- list(
    self = list(self_url),
    description = list(status_description),
    iconUrl = list("https://domain.org/jira/images/icons/statuses/closed.png"),
    name = "Closed",
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

#' Create Comment
#'
#' Creates comment field with information
#'
#' @param comments_body A list of comment_details, similar to component_details
#' @return comments_list
#' @export
create_comments <- function(comments_body) {
  comment_texts <- unlist(strsplit(comments_body, ";"))
  comments <- list()

  for (body in comment_texts) {
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
        timeZone = "Etc/UTC"
      ),
      created = "2021-01-01T10:00:00.000+0000",
      updated = "2021-01-01T12:00:00.000+0000"
    )
    comments[[length(comments) + 1]] <- comment
  }

  return(list(comments = comments))
}

# # sample make_jira_issue call with all parameters, no comments
# make_jira_issue(jira_domain_url = "https://project.org/jira",
#                 issue_key = "2",
#                 issue_type_description = "This is a description.",
#                 summary = "This is a summary.",
#                 components = "x-core;x-spring",
#                 description = "This is a description of the issue.",
#                 reporter_name = "Joe",
#                 assignee_name = "Moe",
#                 status_description = "The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened."
# )
#
# # sample make_jira_issue_comments call with all parameters, with 1 comment
# make_jira_issue_comment(jira_domain_url = "https://project.org/jira",
#                 issue_key = "2",
#                 issue_type_description = "This is a description.",
#                 summary = "This is a summary.",
#                 components = "x-core;x-spring",
#                 description = "This is a description of the issue.",
#                 reporter_name = "Joe",
#                 assignee_name = "Moe",
#                 status_description = "The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened.",
#                 comments_body = "This is a comment.;This is another comment."
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
