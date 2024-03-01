# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Create JIRA Issue
#'
#' Creates a single JIRA Issue as a list, which can be saved as a JSON with or without comments.
#'
#' @param jira_domain_url URL of JIRA domain (e.g. "https://project.org/jira")
#' @param issue_key issue key of JIRA issue (e.g. "PROJECT-68" or "GERONIMO-6723)
#' @param version_names list of version names (e.g. c("3.1.5", "4.0.0"))
#' @param issue_type type of JIRA issue (e.g. "New Feature", "Task", "Bug")
#' @param status status of issue for development (e.g. "In Progress")
#' @param resolution name of resolution for issue (e.g. "Fixed")
#' @param summary summary of the issue (e.g. "Site Keeps Crashing")
#' @param description more detailed description of issue (e.g. "The program keeps crashing because this reason")
#' @param components list of components of issue (e.g. c("PS", "Tests"))
#' @param creator_name name of creator of issue (e.g. "John Doe")
#' @param reporter_name name of reporter of issue (e.g. "Jane Doe")
#' @param assignee_name name of person the issue is being assigned to (e.g. "Joe Schmo")
#' @param comments character list where each element is a comment string (e.g. c("This is first comment", "This is second comment"))
#' @return A list which represents the JIRA JSON in memory
#' @export
#' @family {unittest}
make_jira_issue <- function(jira_domain_url, issue_key, version_names, resolution, priority, labels,
                            assignee_name, status, components, creator_name, reporter_name, issue_type,
                            project_type, description, summary, comments = NULL) {

  # Create an issue with the given parameters as a list. If comments are specified, then add comments to the list
  fields <- list(
    fixVersions = create_fix_versions(jira_domain_url, version_names),
    resolution = create_resolution(name = resolution),
    priority = create_priority(jira_domain_url, priority),
    labels = labels,
    assignee = create_assignee(jira_domain_url, assignee_name),
    status = create_status(jira_domain_url, status),
    components = create_components(jira_domain_url, components),
    creator = create_creator(jira_domain_url, creator_name),
    reporter = create_reporter(jira_domain_url, reporter_name),
    issuetype = create_issue_type(jira_domain_url, issue_type),
    project = create_project(jira_domain_url, issue_key, project_type),
    resolutiondate = "2007-08-13T19:12:33.000+0000",
    created = "2007-07-08T06:07:06.000+0000",
    updated = "2008-05-12T08:01:39.000+0000",
    description = description,
    summary = summary
  )

  if (!is.null(comments) && length(comments) > 0) {

    fields[["comment"]][["comments"]] <- create_issue_comments(comments)
    fields[["comment"]][["maxResults"]] <- length(issue[["comment"]][[1]])
    fields[["comment"]][["total"]] <- length(issue[["comment"]][[1]])
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
#' Create issue type cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_type name of the issue type (e.g. New Feature)
#' @return A list named 'issue_type' that represents the issue type of the JIRA issue
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
#' @return A list named 'status' containing status's information
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
#' Create a fixVersions cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param version_names list of version names for the issue
#' @return A list named 'fixVersions' with a list of versions.
create_fix_versions <- function(jira_domain_url, version_names) {

  fixVersions_list <- list()

  for(version_name in version_names){
    id <- sample(10000000: 99999999, 1)
    self_url <- paste0(jira_domain_url, "/rest/api/2/version/", id)

    version <- list(
      self = self_url,
      id = as.character(id),
      description = "This is a description of the fixVersion",
      name = version_name,
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
#' @return A list named 'priority' containing priority information.
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
#' Create a parent cell for \code{\link{make_jira_issue}}.
#'
#' @param jira_domain_url URL of JIRA domain
#' @param issue_key issue key of JIRA issue (e.g. "PROJECT-68" or "GERONIMO-6723)
#' @param status status of issue for development (e.g. "In Progress")
#' @param priority the name of the priority of the issue (Major, Minor, Trivial)
#' @param issue_type type of JIRA issue (e.g. "New Feature", "Task", "Bug")
#' @return A list named 'parent' that contains information on a parent issue
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
#' @param issue_key issue key of JIRA issue (e.g. "PROJECT-68" or "GERONIMO-6723)
#' @param project_type the type of the project
#' @return A list named 'project' that contains project type and other project information
create_project <- function(jira_domain_url, issue_key, project_type) {

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
    key = issue_key,
    name = issue_key,
    projectTypeKey = project_type,
    avartarUrls = avatarUrls
  )
}
