# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Parsers ##############

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
      issue_components = stringi::stri_c(unlist(sapply(issue_comment[["components"]],"[[","name")),collapse = ";"),
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @param domain Custom JIRA domain URL set in config file
#' @param credentials a path to text file containing your username/api token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_path_issue_tracker_issues Path that files will be save along
#' @param maxResults (optional) the maximum number of results to download per page.
#' Default is 50
#' @param verbose boolean flag to specify printing operational
#' messages or not
#' @param maxDownloads Maximum downloads per function call
#' @param search_query an optional API parameter that alters the GET request
#' @export

download_and_save_jira_issues <- function(domain,
                                          credentials,
                                          jql_query,
                                          fields,
                                          save_path_issue_tracker_issues,
                                          maxResults = 50,
                                          verbose = FALSE,
                                          maxDownloads = 5000,
                                          search_query = NULL) {

  # Ensure the domain starts with https:// for secure communication.
  if (!grepl("^https?://", domain)) {
    domain <- paste0("https://", domain)
  }

  # append search_query to jql_query if present
  if (!is.null(search_query)){
    jql_query <- paste(jql_query, search_query)
    message(jql_query)
  }

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

    #Authenticate if username and password are provided
    if(length(credentials) >= 2) {
      username <- credentials[1]
      password <- credentials[2]
      # Use the credentials for authentication
      auth <- httr::authenticate(as.character(username), as.character(password), "basic")
      #message("successfully authenticated")
    } else {
      if(verbose){
        message("No credentials present or are formatted incorrectly.")
      }
      auth <- NULL
    }

    # Prepare query parameters for the API call
    query_params <- list(jql = jql_query, fields = paste(fields, collapse = ","), maxResults = maxResults, startAt = startAt)

    if (verbose && (downloadCount == 0)){
      message("Query paramters: ", query_params)
    }
    # Make the API call
    response <- httr::GET(url, query = query_params)

    # Stop if there's an HTTP error
    if (httr::http_error(response)) {
      stop("API request failed: ", httr::http_status(response)$message)
    }

    # Extract issues. for iteration of naming convention and checks
    R_object_content <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"),
                                   simplifyVector = FALSE)
    # The number of issues downloaded
    issue_count <- length(R_object_content$issues)
    # save the raw content for a writeLines later
    raw_content <- httr::content(response, "text", encoding = "UTF-8")

    #message(class(response_content))
    #io_make_file(save_path_issue_tracker_issues, response_content)
    #jsonlite::write_json(response_content, save_path_issue_tracker_issue_comments)

    # Check to make sure that the api is downloading the correct amount of issues specified by maxResults
    # This checks for only the first page (if downloadCount ==0)
    # If the total number of issues retrieved is less than maxResults, then of course issue_count
    # will be < maxResults so we check to make sure this is not true (total >= maxResults)
    if ((downloadCount == 0) && (maxResults != issue_count) && (total > maxResults)) {
      message("Total number of issues queried: ", total)
      message(". maxResults specified: ", maxResults)
      message(". Number of issues retrieved: ", issue_count)
      message(". Something went wrong with the API request. Changing maxResults to ", issue_count)
      maxResults <- issue_count
    }
    # R_object_content <- jsonlite::read_json(response_content, simplifyVector = FALSE)
    # all_issues <- append(all_issues, content$issues)

    # Set the filename from the config file. It will be modified in the following code
    file_name <- save_path_issue_tracker_issues

    if (grepl("\\.json$", file_name)) {
      # Remove .json if present in file_name. It will be added again in the naming convention
      file_name <- sub("\\.json$", "", file_name)
    }

    # naming convention for each page
    for (i in rev(seq_along(R_object_content$issues))) {
      if (i == 1){
        issue <- R_object_content$issue[[i]]
        # Get the 'created' field
        issue_created <- issue$fields$created
        # Convert the time string to a POSIXct object, specifying the format
        posix_time <- as.POSIXct(issue_created, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        # Convert the POSIXct object to UNIX time
        unix_time <- as.numeric(posix_time)
        # append to the filename
        file_name <- paste0(file_name, "_", unix_time, ".json")
        # less lines? But less readable
        # file_name <- paste0(file_name, "_", as.numeric(as.POSIXct(R_object_content$issue[[i]]$fields$created, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")), ".json")
      }
      if (i == issue_count){
        issue <- R_object_content$issue[[i]]
        # Get the 'created' field
        issue_created <- issue$fields$created
        # Convert the time string to a POSIXct object, specifying the format
        posix_time <- as.POSIXct(issue_created, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        # Convert the POSIXct object to UNIX time
        unix_time <- as.numeric(posix_time)
        # append to the filename
        file_name <- paste0(file_name, "_", unix_time)
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

    # update downloadCount and optional print statements
    downloadCount <- downloadCount + issue_count
    if (verbose && (issue_count > 0)){
      message("saved file to ", file_name)
      message("Saved ", downloadCount, " total issues")
    }


    #updates startat for next loop
    if (issue_count < maxResults) {
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
  return(NULL)
}


#' Define function to fetch issues from 'created' date ranges from JIRA REST API and save as JSON
#'
#' Download issue data from "rest/api/lastest/search" endpoint
#'
#' @param domain Custom JIRA domain URL set in config file
#' @param credentials a path to text file containing your username/api token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_path_issue_tracker_issues Path that files will be save along
#' @param maxResults (optional) the maximum number of results to download per page.
#' Default is 50
#' @param verbose boolean flag to specify printing operational
#' messages or not
#' @param maxDownloads Maximum downloads per function call
#' @param date_lower_bound an optional API parameter that alters the GET request
#' @param date_upper_bound an optional API parameter that alters the GET request
#' @export
download_and_save_jira_issues_by_created <- function(domain,
                                                     credentials,
                                                     jql_query,
                                                     fields,
                                                     save_path_issue_tracker_issues,
                                                     maxResults,
                                                     verbose,
                                                     maxDownloads,
                                                     date_lower_bound = NULL,
                                                     date_upper_bound = NULL){
  created_query <- ""
  if (!is.null(date_lower_bound)){
    created_query <- paste0(created_query, "AND created >= '", date_lower_bound, "' ")
  }
  if (!is.null(date_upper_bound)){
    created_query <- paste0(created_query, "AND created <= '", date_upper_bound, "' ")
  }

  message("Appending ", created_query, " to api request.")

  download_and_save_jira_issues(domain,
                                credentials,
                                jql_query,
                                fields,
                                save_path_issue_tracker_issues,
                                maxResults,
                                verbose,
                                maxDownloads,
                                search_query = created_query)
}

#' Define function to fetch issues from 'created' date ranges from JIRA REST API and save as JSON
#'
#' Download issue data from "rest/api/lastest/search" endpoint
#'
#' @param domain Custom JIRA domain URL set in config file
#' @param credentials a path to text file containing your username/api token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_path_issue_tracker_issues Path that files will be save along
#' @param maxResults (optional) the maximum number of results to download per page.
#' Default is 50
#' @param verbose boolean flag to specify printing operational
#' messages or not
#' @param maxDownloads Maximum downloads per function call
#' @param issueKey_lower_bound an optional API parameter that alters the GET request
#' @param issueKey_upper_bound an optional API parameter that alters the GET request
#' @export
download_and_save_jira_issues_by_issueKey <- function(domain,
                                                      credentials,
                                                      jql_query,
                                                      fields,
                                                      save_path_issue_tracker_issues,
                                                      maxResults,
                                                      verbose,
                                                      maxDownloads,
                                                      issueKey_lower_bound = NULL,
                                                      issueKey_upper_bound = NULL){
  created_query <- ""
  if (!is.null(issueKey_lower_bound)){
    created_query <- paste0(created_query, "AND issueKey >= ", issueKey_lower_bound)
  }
  if (!is.null(issueKey_upper_bound)){
    created_query <- paste0(created_query, " AND issueKey <= ", issueKey_upper_bound)
  }

  message("Appending ", created_query, " to api request.")

  download_and_save_jira_issues(domain,
                                credentials,
                                jql_query,
                                fields,
                                save_path_issue_tracker_issues,
                                maxResults,
                                verbose,
                                maxDownloads,
                                search_query = created_query)
}

#' Define function to extract greatest issueKey from a file and pass it to download_and_save_jira_issues_by_issueKey
#'
#' Download issue data from "rest/api/2/search" endpoint
#'
#' @param domain Custom JIRA domain URL set in config file
#' @param credentials a path to text file containing your username/api token
#' @param jql_query Specific query string to specify criteria for fetching
#' @param fields List of fields that are downloaded in each issue
#' @param save_path_issue_tracker_issues Path that files will be save along
#' @param maxResults (optional) the maximum number of results to download per page.
#' Default is 50
#' @param verbose boolean flag to specify printing operational
#' messages or not
#' @param maxDownloads Maximum downloads per function call
#' @param file_name The filename that contains the greatest issueKey. Returned by a parser
#' @param unaltered_file_path the unaltered filepath set in the config file
#' @export
download_and_save_jira_issues_refresh <- function(domain,
                                                  credentials,
                                                  jql_query,
                                                  fields,
                                                  save_path_issue_tracker_issues,
                                                  maxResults,
                                                  verbose,
                                                  maxDownloads,
                                                  file_name,
                                                  unaltered_file_path){

  issue_refresh <- paste0(unaltered_file_path, file_name)

  # Read the JSON file
  json_data <- jsonlite::fromJSON(txt = issue_refresh, simplifyVector = FALSE)

  # Extract the value with the greatest issueKey
  issue_first <- json_data$issue[[1]]['key']

  # Construct the search query to append to the JIRA API request
  search_query <- paste0("AND issueKey > ", issue_first)

  message("Appending ", search_query, " to JQL query")

  download_and_save_jira_issues(domain,
                                credentials,
                                jql_query,
                                fields,
                                save_path_issue_tracker_issues,
                                maxResults,
                                verbose,
                                maxDownloads,
                                search_query)
}
