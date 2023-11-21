# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Creates a sample jira issue without comment and two components
#'
#' This example replicates Kaiaulu issue #244
#'
#' @param folder_path An optional path to where the sample .json should be created.
#' @return The path to the sample .json file.
#' @export
#' @family {unittest}
jira_create_sample_log_issues <- function(folder_path="/tmp") {
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  folder_path <- path.expand(folder_path)

  issues <- list()

  base_info_cell <- create_base_info()
  issues[["base_info"]][[1]] <- base_info_cell

  ext_info_cell <- create_ext_info_issues()
  issues[["ext_info"]][[1]] <- ext_info_cell

  jira_json_path <- file.path(folder_path,"fake_issues.json")
  jsonlite::write_json(issues,file.path(folder_path,"fake_issues.json"))

  return(jira_json_path)
}

#' Create base_info_cell
#'
#' Creates and formats base_info_cell for fake Jira issue
#'
#' @param id ID of Jira issue
#' @param self URL of API endpoint
#' @param key key for Jira issue
#' @param JirAgileR_id JirAgileR id
#' @return A list named 'base_info_cell' containing all the parameters
#' @export
create_base_info <- function(id = 11, self = "https://project.org/jira/rest/api/latest/issue/11", key = "PROJECT-68", JirAgileR_id = 1) {
  base_info_cell <- list(
    id = id,
    self = self,
    key = key,
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
create_ext_info_issues <- function() {
  ext_info_cell <- list(
    summary = list("Issue Description"),
    issuetype = create_issuetype(),
    components = create_components(component_details),
    creator = create_creator(),
    created = list("2007-07-08T06:07:06.000+0000"),
    description = list(),
    reporter = create_reporter(),
    resolution = create_resolution(self_url = "https://domain.org/jira/rest/api/2/resolution/1"),
    resolutiondate = list("2007-08-13T19:12:33.000+0000"),
    assignee = list(),
    updated = list("2008-05-12T08:01:39.000+0000"),
    status = create_status()
  )

  return(ext_info_cell)
}

#' Create Issue Type
#'
#' Create issue type cell for fake Jira issue
#'
#' @param self_url URL of issue type's endpoint
#' @param id ID of Jira issue
#' @param description Description of issue type
#' @param iconUrl URL of icon representing issue type
#' @param name Name of issue type
#' @param subtask FALSE, determines if the issue is a subtask
#' @param avatarId ID for issue type's avatar image
#' @return A list named 'issuetype' that represents the issue type of the Jira issue
#' @export
create_issuetype <- function(self_url = "https://project.org/jira/rest/api/2/issuetype/2",
                             id = "2",
                             description = "A new feature of the product, which has yet to be developed.",
                             iconUrl = "https://domain.org/jira/secure/viewavatar?size=xsmall&avatarId=21141&avatarType=issuetype",
                             name = "New Feature",
                             subtask = FALSE,
                             avatarId = 21141) {
  issuetype <- list(
    self = list(list(self_url)),
    id = list(list(id)),
    description = list(list(description)),
    iconUrl = list(iconUrl),
    name = list(name),
    subtask = list(subtask),
    avatarId = list(avatarId)
  )

  return(issuetype)
}

#' Create Components
#'
#' Creates a list populated with components and their information from components_list
#'
#' @param components_list A list with the lists of components
#' @return A list named 'components' which contains each component
create_components <- function(components_list) {
  components <- list()
  for (i in 1:length(components_list)) {
    component <- components_list[[i]]
    components[[i]] <- list (
      self = list(component$self),
      id = list(component$id),
      name = list(component$name)
    )
  }
  return(components)
}

# each list is a component, added through create_components above
component_details <- list(
  list(self = "https://domain.org/jira/rest/api/2/component/12313938", id = "12313938", name = "x-core"),
  list(self = "https://domain.org/jira/rest/api/2/component/12313939", id = "12313939", name = "x-spring")
)

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
#' @param self_url URL of API endpoint
#' @param name Name of reporter
#' @param key Key associated with reporter
#' @param displayName Display name of reporter
#' @param active TRUE, indicates if reporter's account is active or not
#' @param timeZone Timezone associated with reporter
#' @return A list named 'reporter' which contains the reporter's information
#' @export
create_reporter <- function(self_url = "https://domain.org/jira/rest/api/2/user?username=user1",
                            name = "user1",
                            key = "user1",
                            displayName = "Fake User1",
                            active = TRUE,
                            timeZone = "Etc/UTC") {
  reporter <- list(
    self = list(self_url),
    name = list(name),
    key = list(key),
    displayName = list(displayName),
    active = list(active),
    timeZone = list(timeZone)
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
#' @param self_url URL of API endpoint
#' @param description Description of status
#' @param iconUrl URL to icon associated with status
#' @param name Name of status
#' @param id ID associated with status
#' @param statusCategory_self URL to status category API endpoint
#' @param statusCategory_id ID associated with status category
#' @param statusCategory_key key associated with status category
#' @param statusCategory_colorName name of color associated with status category
#' @param statusCategory_name name of status category
#' @return A list named 'status' containing status's information
#' @export
create_status <- function(self_url = "https://domain.org/jira/rest/api/2/status/6",
                          description = "The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened.",
                          iconUrl = "https://domain.org/jira/images/icons/statuses/closed.png",
                          name = "Closed",
                          id = "6",
                          statusCategory_self = "https://domain.org/jira/rest/api/2/statuscategory/3",
                          statusCategory_id = 3,
                          statusCategory_key = "done",
                          statusCategory_colorName = "green",
                          statusCategory_name = "Done") {
  status <- list(
    self = list(self_url),
    description = list(description),
    iconUrl = list(iconUrl),
    name = name,
    id = id,
    statusCategory = list(
      self = list(statusCategory_self),
      id = list(statusCategory_id),
      key = list(statusCategory_key),
      colorName = list(statusCategory_colorName),
      name = list(statusCategory_name)
    )
  )

  return(status)
}

#' Create ext_info_cell for issues (with comments)
#'
#' Creates the ext_info_cell with issues with comments
#'
#' @return ext_info_cell with comment fields
#' @export
# values can be changed either here if hard coded or in respective create function below
create_ext_info_issues_comments <- function() {
  ext_info_cell <- list(
    summary = list("Issue Description"),
    issuetype = create_issuetype(),
    components = create_components(component_details),
    creator = create_creator(),
    created = list("2007-07-08T06:07:06.000+0000"),
    description = list(),
    reporter = create_reporter(),
    resolution = create_resolution(self_url = "https://domain.org/jira/rest/api/2/resolution/1"),
    resolutiondate = list("2007-08-13T19:12:33.000+0000"),
    assignee = list(),
    updated = list("2008-05-12T08:01:39.000+0000"),
    status = create_status(),
    comment = create_comment(comment_details)
  )

  return(ext_info_cell)
}

#' Create Comment
#'
#' Creates comment field with information
#'
#' @param comment_details A list of comment_details, similar to component_details
#' @return comments
#' @export
create_comment <- function(comment_details) {
  comments <- list()
  for (comment_detail in comment_details) {
    comment <- list(
      self = comment_detail$self,
      id = comment_detail$id,
      author = list(
        self = comment_detail$author_self,
        name = comment_detail$author_name,
        key = comment_detail$author_key,
        avatarUrls = list(
          "48x48" = comment_detail$author_avatarUrls_48x48,
          "24x24" = comment_detail$author_avatarUrls_24x24,
          "16x16" = comment_detail$author_avatarUrls_16x16,
          "32x32" = comment_detail$author_avatarUrls_32x32
        ),
        displayName = comment_detail$author_displayName,
        active = comment_detail$author_active,
        timeZone = comment_detail$author_timeZone
      ),
      body = comment_detail$body,
      updateAuthor = list(
        self = comment_detail$updateAuthor_self,
        name = comment_detail$updateAuthor_name,
        key = comment_detail$updateAuthor_key,
        avatarUrls = list(
          "48x48" = comment_detail$updateAuthor_avatarUrls_48x48,
          "24x24" = comment_detail$updateAuthor_avatarUrls_24x24,
          "16x16" = comment_detail$updateAuthor_avatarUrls_16x16,
          "32x32" = comment_detail$updateAuthor_avatarUrls_32x32
        ),
        displayName = comment_detail$updateAuthor_displayName,
        active = comment_detail$updateAuthor_active,
        timeZone = comment_detail$updateAuthor_timeZone
      ),
      created = comment_detail$created,
      updated = comment_detail$updated
    )
    comments[[length(comments) + 1]] <- comment
  }
  return(list(comments = comments))
}

# details of comments to be added
comment_details <- list(
  list(
    self = "https://example.com/jira/comment/1",
    id = "1",
    author_self = "https://example.com/jira/user?username=user1",
    author_name = "user1",
    author_key = "user1",
    author_avatarUrls_48x48 = "https://example.com/avatar/user1_48x48.jpg",
    author_avatarUrls_24x24 = "https://example.com/avatar/user1_24x24.jpg",
    author_avatarUrls_16x16 = "https://example.com/avatar/user1_16x16.jpg",
    author_avatarUrls_32x32 = "https://example.com/avatar/user1_32x32.jpg",
    author_displayName = "User One",
    author_active = TRUE,
    author_timeZone = "Etc/UTC",
    body = "First comment body.",
    updateAuthor_self = "https://example.com/jira/user?username=user2",
    updateAuthor_name = "user2",
    updateAuthor_key = "user2",
    updateAuthor_avatarUrls_48x48 = "https://example.com/avatar/user2_48x48.jpg",
    updateAuthor_avatarUrls_24x24 = "https://example.com/avatar/user2_24x24.jpg",
    updateAuthor_avatarUrls_16x16 = "https://example.com/avatar/user2_16x16.jpg",
    updateAuthor_avatarUrls_32x32 = "https://example.com/avatar/user2_32x32.jpg",
    updateAuthor_displayName = "User Two",
    updateAuthor_active = TRUE,
    updateAuthor_timeZone = "America/New_York",
    created = "2021-01-01T10:00:00.000+0000",
    updated = "2021-01-01T12:00:00.000+0000"
  )

)

#' Create Jira Sample Log Issues with comments
#'
#' Creates a sample jira issue with two components WITH comments
#'
#' @param folder_path An optional path to where the sample .json should be created.
#' @return The path to the sample .json file.
#' @export
#' @family {unittest}
jira_create_sample_log_issues_comments <- function(folder_path="/tmp") {
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  folder_path <- path.expand(folder_path)

  issues <- list()

  base_info_cell <- create_base_info()
  issues[["base_info"]][[1]] <- base_info_cell

  ext_info_cell <- create_ext_info_issues_comments()
  issues[["ext_info"]][[1]] <- ext_info_cell

  # write to json
  jira_json_path <- file.path(folder_path,"fake_issues_comments.json")
  jsonlite::write_json(issues,file.path(folder_path,"fake_issues_comments.json"))

  return(jira_json_path)
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
jira_delete_sample_log <- function(folder_path="/tmp"){
  folder_path <- path.expand(folder_path)
  jira_json_path <- file.path(folder_path,"fake_issues.json")
  error <- system2('rm',
                   args = c(jira_json_path),
                   stdout = TRUE,
                   stderr = FALSE)
}
