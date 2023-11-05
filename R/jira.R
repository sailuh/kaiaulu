# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Creates a sample jira issue without comment and two components
#'
#' This example replicates Kaiaulu issue #244
#'
#'
#' @param folder_path An optional path to where the sample .json should be created.
#' @return The path to the sample .json file.
#' @export
#' @family {unittest}
jira_create_sample_log <- function(folder_path="/tmp"){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  folder_path <- path.expand(folder_path)

  issues <- list()

  issues[["base_info"]] <- list()
  base_info_cell <- list()
  base_info_cell[["id"]] <- 11
  base_info_cell[["self"]] <- "https://project.org/jira/rest/api/latest/issue/11"
  base_info_cell[["key"]] <- "PROJECT-68"
  base_info_cell[["JirAgileR_id"]] <- 1
  issues[["base_info"]][[1]] <- base_info_cell



  issues[["ext_info"]] <- list()
  ext_info_cell <- list()
  ext_info_cell[["summary"]][[1]] <- "Issue Description"

  ext_info_cell[["issuetype"]] <- list()
  ext_info_cell[["issuetype"]][["self"]][[1]] <- list("https://project.org/jira/rest/api/2/issuetype/2")
  ext_info_cell[["issuetype"]][["id"]][[1]] <- list("2")
  ext_info_cell[["issuetype"]][["description"]][[1]] <- list("A new feature of the product, which has yet to be developed.")
  ext_info_cell[["issuetype"]][["iconUrl"]] <- list("https://domain.org/jira/secure/viewavatar?size=xsmall&avatarId=21141&avatarType=issuetype")
  ext_info_cell[["issuetype"]][["name"]] <- list("New Feature")
  ext_info_cell[["issuetype"]][["subtask"]]<- list(FALSE)
  ext_info_cell[["issuetype"]][["avatarId"]] <- list(21141)

  ext_info_cell[["components"]] <- list()
  ext_info_cell[["components"]][[1]] <- list()
  ext_info_cell[["components"]][[1]][["self"]] <- list("https://domain.org/jira/rest/api/2/component/12313938")
  ext_info_cell[["components"]][[1]][["id"]] <- list("12313938")
  ext_info_cell[["components"]][[1]][["name"]] <- list("x-core")
  ext_info_cell[["components"]][[2]] <- list()
  ext_info_cell[["components"]][[2]][["self"]] <- list("https://domain.org/jira/rest/api/2/component/12313939")
  ext_info_cell[["components"]][[2]][["id"]] <- list("12313939")
  ext_info_cell[["components"]][[2]][["name"]] <- list("x-spring")

  ext_info_cell[["creator"]] <- list()
  ext_info_cell[["creator"]][["self"]] <- list("https://domain.org/jira/rest/api/2/user?username=user1")
  ext_info_cell[["creator"]][["name"]] <- list("user1")
  ext_info_cell[["creator"]][["key"]] <- list("user1")
  ext_info_cell[["creator"]][["displayName"]] <- list("Fake User1")
  ext_info_cell[["creator"]][["active"]] <- list(TRUE)
  ext_info_cell[["creator"]][["timeZone"]] <- list("Etc/UTC")

  ext_info_cell[["created"]] <- list("2007-07-08T06:07:06.000+0000")

  ext_info_cell[["description"]] <- list()

  ext_info_cell[["reporter"]] <- list()

  # same cells as creator, but user information may be different
  ext_info_cell[["reporter"]] <- list()
  ext_info_cell[["reporter"]][["self"]] <- list("https://domain.org/jira/rest/api/2/user?username=user1")
  ext_info_cell[["reporter"]][["name"]] <- list("user1")
  ext_info_cell[["reporter"]][["key"]] <- list("user1")
  ext_info_cell[["reporter"]][["displayName"]] <- list("Fake User1")
  ext_info_cell[["reporter"]][["active"]] <- list(TRUE)
  ext_info_cell[["reporter"]][["timeZone"]] <- list("Etc/UTC")

  ext_info_cell[["resolution"]] <- list()
  ext_info_cell[["resolution"]][["self"]] <- list("https://domain.org/jira/rest/api/2/resolution/1")
  ext_info_cell[["resolution"]][["id"]] <- list("1")
  ext_info_cell[["resolution"]][["description"]] <- list("A fix for this issue is checked into the tree and tested.")
  ext_info_cell[["resolution"]][["name"]] <- list("Fixed")

  ext_info_cell[["resolutiondate"]] <- list("2007-08-13T19:12:33.000+0000")

  ext_info_cell[["assignee"]] <- list()

  ext_info_cell[["updated"]] <- list("2008-05-12T08:01:39.000+0000")

  ext_info_cell[["status"]] <- list()
  ext_info_cell[["status"]][["self"]] <- list("https://domain.org/jira/rest/api/2/status/6")
  ext_info_cell[["status"]][["description"]] <- list("The issue is considered finished, the resolution is correct. Issues which are not closed can be reopened.")
  ext_info_cell[["status"]][["iconUrl"]] <- list("https://domain.org/jira/images/icons/statuses/closed.png")
  ext_info_cell[["status"]][["name"]] <- "Closed"
  ext_info_cell[["status"]][["id"]] <- "6"
  ext_info_cell[["status"]][["statusCategory"]] <- list()
  ext_info_cell[["status"]][["statusCategory"]][["self"]] <- list("https://domain.org/jira/rest/api/2/statuscategory/3")
  ext_info_cell[["status"]][["statusCategory"]][["id"]] <- list(3)
  ext_info_cell[["status"]][["statusCategory"]][["key"]] <- list("done")
  ext_info_cell[["status"]][["statusCategory"]][["colorName"]] <- list("green")
  ext_info_cell[["status"]][["statusCategory"]][["name"]] <- list("Done")

  issues[["ext_info"]][[1]] <- ext_info_cell

  jira_json_path <- file.path(folder_path,"fake_issues.json")
  jsonlite::write_json(issues,file.path(folder_path,"fake_issues.json"))


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
