# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Parsers ##############

#' Parse GitHub Issue and Pull Request Comments
#'
#' Parses Issue, Pull Request, and Comments Endpoints into a reply table.
#' See example usage in the download_github_comments.Rmd vignette.
#'
#' @param issues_json_folder_path The path to the downloaded issues JSON. See \code{\link{github_api_project_issue}}.
#' @param pull_requests_json_folder_path The path to the downloaded pull requests JSON. See \code{\link{github_api_project_pull_request}}.
#' @param comments_json_folder_path The path to the downloaded comments JSON. See \code{\link{github_api_project_issue_or_pr_comments}}.
#' @param commit_json_folder_path The path to the downloaded commits JSON (used to map github username to the git log). See \code{\link{github_api_project_commits}}.
#' @param pr_comments_json_folder_path The path to the download pr comments JSON. see \code{\link{github_api_project_pr_comments}}.
#' @return A single reply table which combines the communication from the three jsons.
#' @export
parse_github_replies <- function(issues_json_folder_path,
                                 pull_requests_json_folder_path,
                                 comments_json_folder_path,
                                 commit_json_folder_path,
                                 pr_comments_json_folder_path){
# PASSING THE WRONG PARAMETER HERE

#  issues_json_folder_path <- paste0(github_replies_folder_path,"/issue/")
#  pull_requests_json_folder_path <- paste0(github_replies_folder_path,"/pull_request/")
#  comments_json_folder_path <- paste0(github_replies_folder_path,"/issue_or_pr_comment/")
#  commit_json_folder_path <- paste0(github_replies_folder_path,"/commit/")
#  pr_comments_json_folder_path <- paste0(github_replies_folder_path,"/pr_comment/")

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

  # Tabulate PR Comments
  all_pr_comments <- lapply(list.files(pr_comments_json_folder_path,
                                       full.names = TRUE), jsonlite::read_json)
  all_pr_comments <- lapply(all_pr_comments,
                            github_parse_project_pr_comments)
  all_pr_comments <- rbindlist(all_pr_comments, fill=TRUE)

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

#######

#' Download Project Contributors
#'
#' Download project contributors from GET /repos/{owner}/{repo}/contributors" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For more details see \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-repository-contributors}.
#' @export
github_api_project_contributors <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/contributors",
         owner=owner,
         repo=repo,
         page=1,
         per_page=100,
         .token=token)
}

###### Github Pull Request Reviews ######

#' Download Pull Request Reviews
#'
#' Download Pull Request Reviews from "GET /repos/{owner}/{repo}/pulls/{pull_number}/reviews" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For details, see \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-reviews-for-a-pull-request}.
#' @export
github_api_pr_reviews <- function(owner,repo,pull_number,token){
  gh::gh("GET /repos/{owner}/{repo}/pulls/{pull_number}/reviews",
         owner=owner,
         repo=repo,
         pull_number=pull_number,
         page=1,
         per_page=100,
         .token=token)
}

#' Parse Pull Requests' Reviews JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#' This function only parses for the reviews made on the pull request.
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_pr_reviews <- function(api_responses) {
  parse_response <- function(api_response) {
    parsed_response <- list()
    parsed_response[["review_id"]] <- api_response[["id"]]
    parsed_response[["user_id"]] <- api_response[["user"]][["id"]]
    parsed_response[["reviewer"]] <- api_response[["user"]][["login"]]
    parsed_response[["submitted_at"]] <- api_response[["submitted_at"]]
    parsed_response[["state"]] <- api_response[["state"]]
    parsed_response[["body"]] <- api_response[["body"]]
    parsed_response[["pull_request_url"]] <- api_response[["_links"]][["pull_request"]][["href"]]
    parsed_response[["commit_id"]] <- api_response[["commit_id"]]



    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

###### Github Pull Request Commits ######

#' Download Pull Request Commits
#'
#' Download Pull Request Commits from "GET /repos/{owner}/{repo}/pulls/{pull_number}/commits" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For details, see \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-commits-on-a-pull-request}.
#' @export
github_api_pr_commits <- function(owner,repo,pull_number,token){
  gh::gh("GET /repos/{owner}/{repo}/pulls/{pull_number}/commits",
         owner=owner,
         repo=repo,
         pull_number=pull_number,
         page=1,
         per_page=100,
         .token=token)
}

#' Parse Pull Requests' Commits JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#' This function only parses for the commits made on the pull request.
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_pr_commits <- function(api_responses) {
  parse_response <- function(api_response) {
    parsed_response <- list()
    parsed_response[["user"]] <- api_response[["commit"]][["author"]][["name"]]
    parsed_response[["date"]] <- api_response[["commit"]][["author"]][["date"]]
    parsed_response[["message"]] <- api_response[["commit"]][["message"]]

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

#' Download Pull Request Files
#'
#' Download Pull Request Files from "GET /repos/{owner}/{repo}/pulls/{pull_number}/files" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For details, see \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-files-on-a-pull-request}.
#' @export
github_api_pr_files <- function(owner,repo,pull_number,token){
  gh::gh("GET /repos/{owner}/{repo}/pulls/{pull_number}/files",
         owner=owner,
         repo=repo,
         pull_number=pull_number,
         page=1,
         per_page=100,
         .token=token)
}

#' Parse Pull Requests' Files JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#' This function only parses for the files made on the pull request.
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_pr_files <- function(api_responses) {
  parse_response <- function(api_response) {
    parsed_response <- list()
    parsed_response[["filename"]] <- api_response[["filename"]]
    parsed_response[["additions"]] <- api_response[["addtions"]]
    parsed_response[["deletions"]] <- api_response[["deletions"]]
    parsed_response[["changes"]] <- api_response[["changes"]]

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

#' Download Pull Request Requested Reviewers
#'
#' Download Pull Request Requested Reviewers from "GET /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For details, see \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#get-all-requested-reviewers-for-a-pull-request}.
#' @export
github_api_pr_reviewers <- function(owner,repo,pull_number,token){
  gh::gh("GET /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers",
         owner=owner,
         repo=repo,
         pull_number=pull_number,
         page=1,
         per_page=100,
         .token=token)
}

#' Parse Pull Requests' Requested Reviewers JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#' This function only parses for the files made on the pull request.
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_pr_reviewers <- function(api_responses) {
  parse_response <- function(api_response) {
    parsed_response <- list()
    parsed_response[["users"]] <- api_response[["users"]][["login"]]

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

#' Download Pull Request Merge Status
#'
#' Download Pull Request Requested Reviewers from "GET /repos/{owner}/{repo}/pulls/{pull_number}/merge" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For details, see \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#check-if-a-pull-request-has-been-merged}.
#' @export
github_api_pr_merge <- function(owner,repo,pull_number,token){
  gh::gh("GET /repos/{owner}/{repo}/pulls/{pull_number}/merge",
         owner=owner,
         repo=repo,
         pull_number=pull_number,
         page=1,
         per_page=100,
         .token=token)
}

#' Parse Pull Requests' Merge Status JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#' This function only checks for the merge status for a pull request.
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_pr_merge <- function(api_responses) {
  parse_response <- function(api_response) {
    parsed_response <- list()
    parsed_response[["status"]] <- api_response[["status"]]

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

###### Github Issue Events ######

#' Download Project Issue Events
#'
#' Download Issues from "GET /repos/{owner}/{repo}/issues/events" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For details, see \url{https://docs.github.com/en/free-pro-team@latest/rest/reference/issues#events}
#' and \url{https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/issue-event-types}.
#' @export
github_api_project_issue_events <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/issues/events",
         owner=owner,
         repo=repo,
         type="IssuesEvent",
         page=1,
         per_page=100,
         .token=token)
}


#' Parse Issue Events JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#'
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_issue_events <- function(api_responses){
  parse_response <- function(api_response){
    parsed_response <- list()
    parsed_response[["id"]] <- api_response[["id"]]
    parsed_response[["created_at"]] <- api_response[["created_at"]]
    parsed_response[["commit_id"]] <- ifelse(length(api_response[["commit_id"]]) == 0,
                                             NA,
                                             api_response[["commit_id"]])
    parsed_response[["event"]] <- api_response[["event"]]
    parsed_response[["actor_login"]] <- api_response[["actor"]][["login"]]
    parsed_response[["actor_id"]] <- api_response[["actor"]][["id"]]
    parsed_response[["actor_type"]] <- api_response[["actor"]][["type"]]
    parsed_response[["issue_number"]] <- api_response[["issue"]][["number"]]
    parsed_response[["issue_title"]] <- api_response[["issue"]][["title"]]
    parsed_response[["issue_user_login"]] <- api_response[["issue"]][["user"]][["login"]]
    parsed_response[["issue_user_id"]] <- api_response[["issue"]][["user"]][["id"]]
    parsed_response[["issue_user_site_admin"]] <- api_response[["issue"]][["user"]][["site_admin"]]
    parsed_response[["issue_state"]] <- api_response[["issue"]][["state"]]
    parsed_response[["issue_author_association"]] <- api_response[["issue"]][["author_association"]]
    parsed_response[["issue_body"]] <- api_response[["issue"]][["body"]]
    parsed_response[["issue_assignee_login"]] <- api_response[["issue"]][["assignee"]][["login"]]
    parsed_response[["issue_assignee_id"]] <- api_response[["issue"]][["assignee"]][["id"]]
    parsed_response[["issue_body"]] <- api_response[["issue"]][["body"]]

    assignees_list <- api_response[["issue"]][["assignees"]]
    assignees_list <- lapply(assignees_list,function(x){
      data.table(data.frame(issue_assignees_login=x[["login"]],issue_assignees_id=x[["id"]]))
    })
    assignees_list <- rbindlist(assignees_list,fill=TRUE)
    parsed_response[["issue_assignees_login"]] <- stringi::stri_c(assignees_list$issue_assignees_login,collapse = ";")
    parsed_response[["issue_assignees_id"]] <- stringi::stri_c(assignees_list$issue_assignees_id,collapse = ";")

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

###### Github Issue Search ######

#' Download Project Issues via Search
#'
#' Download Commits from "GET /repos/{owner}/{repo}/search/issues" endpoint.
#' This search endpoint allows for optional query parameter. Potential queries are found
#' [here](https://docs.github.com/en/rest/search/search?apiVersion=2022-11-28). The query parameter
#' assumes that owner/repo is already prepended to the query. If no query is passed to the function,
#' it will prepend only owner/repo to the query.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param query Optional query to append to search api
#' @param issue_or_pr This specifies whether issues or pull requests are being searched for.
#' Acceptable inputs are "is:issue" or "is:pull-request".
#' @param verbose Prints operational messages when set to true such as stating the search query.
#' @references For details, see \url{https://docs.github.com/en/rest/search/search?apiVersion=2022-11-28}.
#' @references For details on timestampes, se \url{https://docs.github.com/en/search-github/searching-on-github/searching-issues-and-pull-requests#search-by-when-an-issue-or-pull-request-was-created-or-last-updated}
#' @export
github_api_project_issue_search <- function(owner, repo, token, query = NULL, issue_or_pr, verbose=TRUE) {
  # Construct the search query
  #Check if there is a query
  if (!is.null(query)){
    search_query <- query
  } else {
    search_query <- "repo:"
    search_query <- paste0(search_query,owner,"/",repo," ", issue_or_pr)
  }

  if(verbose){
    message("Search query: ", search_query)
  }

  # Perform the GitHub API call
  gh_response <- gh::gh("/search/issues",
                        q = search_query,
                        state = 'all',
                        page = 1,
                        per_page = 100,
                        .token = token)
  return(gh_response)
}

#' Parse Issues JSON from refresh to Table
#'
#' Note not all columns available in the downloaded json are parsed. This parser
#' is adapted from \code{link{github_parse_project_issue}} to parse data
#' from the refresh_issue folder. This data is downloaded from the Github API
#' search endpoint and has a different level of nesting than the original data
#'
#' @param api_responses API response obtained from github_api_* function.
#' @export
#' @seealso  \code{link{github_api_project_issue_refresh}} to refresh issue data
github_parse_search_issues_refresh <- function(api_responses) {
  # Helper function to parse each issue
  parse_response <- function(api_response) {
    parsed_response <- list()
    parsed_response[["issue_id"]] <- api_response[["id"]]
    parsed_response[["issue_number"]] <- api_response[["number"]]
    parsed_response[["html_url"]] <- api_response[["html_url"]]
    parsed_response[["url"]] <- api_response[["url"]]
    parsed_response[["created_at"]] <- api_response[["created_at"]]
    parsed_response[["updated_at"]] <- api_response[["updated_at"]]
    parsed_response[["state"]] <- api_response[["state"]]
    parsed_response[["issue_user_login"]] <- api_response[["user"]][["login"]]
    parsed_response[["author_association"]] <- api_response[["author_association"]]
    parsed_response[["title"]] <- api_response[["title"]]
    parsed_response[["body"]] <- api_response[["body"]]

    # Parsing labels
    parsed_response[["labels"]] <- api_response[["labels"]]
    if(length(parsed_response[["labels"]]) > 0) {
      parsed_response[["labels"]] <- stringi::stri_c(sapply(parsed_response[["labels"]], "[[", "name"), collapse = ",")
    } else {
      parsed_response[["labels"]] <- NA_character_
    }

    parsed_response <- as.data.table(parsed_response)
    return(parsed_response)
  }

  # Assuming 'items' contains the issues
  all_issues <- lapply(api_responses[["items"]], parse_response)
  return(rbindlist(all_issues, fill = TRUE))
}

###### Github Issues ######

#' Download Project Issues
#'
#' Download  Issues from "GET /repos/{owner}/{repo}/issues" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/issues#list-repository-issues}.
github_api_project_issue <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/issues",
         owner=owner,
         repo=repo,
         state="all",
         page=1,
         per_page=100,
         .token=token)
}

#' Parse Issues JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#'
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_issue <- function(api_responses){
  parse_response <- function(api_response){
    parsed_response <- list()
    parsed_response[["issue_id"]] <- api_response[["id"]]
    parsed_response[["issue_number"]] <- api_response[["number"]]
    parsed_response[["html_url"]] <- api_response[["html_url"]]
    parsed_response[["url"]] <- api_response[["url"]]
    parsed_response[["created_at"]] <- api_response[["created_at"]]
    parsed_response[["updated_at"]] <- api_response[["updated_at"]]
    parsed_response[["state"]] <- api_response[["state"]]
    parsed_response[["issue_user_login"]] <- api_response[["user"]][["login"]]
    parsed_response[["author_association"]] <- api_response[["author_association"]]
    parsed_response[["title"]] <- api_response[["title"]]
    parsed_response[["body"]] <- api_response[["body"]]

    parsed_response[["labels"]] <- api_response[["labels"]]
    if(length(parsed_response[["labels"]]) > 0){
      parsed_response[["labels"]] <- stringi::stri_c(sapply(parsed_response[["labels"]],"[[","name"),collapse = ",")
    }else{
      parsed_response[["labels"]] <- NA_character_
    }

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

#' Download Project Issues Refresh
#'
#' Uses the adopted file name convention by \code{\link{github_api_iterate_pages}} to identify
#' the latest downloaded Github created_at date among directory "issue_search".
#' It returns the first page of the github query for issues created after this date by calling
#' \code{\link{github_api_project_issue_search}}
#'
#' If the issue directory is empty, then the created query will not be appended to the api call
#' and the first page of a query retrieving all issues will be returned. This function can therefore
#' be used in the specified folder to continuously refresh available issues
#' data.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param save_path_issue_refresh The folder path that the refresh downloader downloads to
#' @param issue_or_pr This specifies whether issues or pull requests are being searched for.
#' Acceptable inputs are "is:issue" or "is:pull-request".
#' @param verbose A boolean value that prints operational messages when set to TRUE.
#' These may include announcing successful execution of code, API queries, files saved, etc.
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/issues#list-repository-issues}.
#' @references For details on timestampes, se \url{https://docs.github.com/en/search-github/searching-on-github/searching-issues-and-pull-requests#search-by-when-an-issue-or-pull-request-was-created-or-last-updated}
#' @seealso  \code{link{github_api_project_issue}} to download all issue data
#' @seealso  \code{link{format_created_at_from_file}} for function that iterates through
#' a .json file and returns the greatest 'created_at' value
#' @seealso  \code{link{github_api_iterate_pages}} to write data returned by this function to file as .json
github_api_project_issue_refresh <- function(owner,
                                             repo,
                                             token,
                                             save_path_issue_refresh,
                                             issue_or_pr,
                                             verbose){


  # Check if refresh folder is empty
  contents_refresh <- list.files(path = save_path_issue_refresh)

  # If the file is empty, download all issues
  if(length(contents_refresh) == 0) {
    if(verbose){
      message("No files exist in directory. Downloading all files")
    }
    query <- NULL
    gh_response <- github_api_project_issue_search(owner, repo, token, query, issue_or_pr, verbose=TRUE)
    return(gh_response)
  } else {
    # Get the name of the file with the most recent date from the refresh_issue file if not empty
    latest_created_issue_refresh <- paste0(save_path_issue_refresh, parse_jira_latest_date(save_path_issue_refresh))
    latest_created_issue_refresh <- head(latest_created_issue_refresh,1)
    # get the greatest created_at value among issues in the refresh_issues file
    created_refresh <- format_created_at_from_file(latest_created_issue_refresh, item="items")
    # }
    if(verbose){
      message("Greatest created value from issue_search folder: ", created_refresh)
    }

    # construct the query
    query <- paste0("repo:",owner,"/",repo," ", issue_or_pr," created:>",created_refresh)

    if (verbose){
      message("Github API query: ",query)
    }
    # Call the API function
    gh_response <- github_api_project_issue_search(owner, repo, token, query, verbose=TRUE)
    return(gh_response)
  }
}

#' Download Github Issue Data by Date
#'
#' Appends a 'created' field to a search github JQL query and returns the first page of the response.
#'
#' Acceptable formats for `date_lower_bound` and `date_upper_bound` are:
#'
#' * "YYYY-MM-DD"
#' * "YYYY-MM-DDTHH:MM"
#' * "YYYY-MM-DDTHH:MM:SS"
#' * "YYYY-MM-DDTHH:MM:SSZ"
#' * "YYYY-MM-DDTHH:MM:SS+00:00"
#' * NULL
#'
#' For example: `date_lower_bound="2020-07-04"` (an issue ocurring at the exact specified time will also be downloaded).
#'
#' For further details on the `created` Query see [the associated Github API documentation](https://docs.github.com/en/search-github/searching-on-github/searching-issues-and-pull-requests#search-by-when-an-issue-or-pull-request-was-created-or-last-updated).
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param date_lower_bound Optional. Specify the lower bound date time (e.g. 2023/11/16 21:00)
#' @param date_upper_bound Optional. Specify the upper bound date time (e.g. 2023/11/16 21:00)
#' @param issue_or_pr This specifies whether issues or pull requests are being searched for.
#' Acceptable inputs are "is:issue" or "is:pull-request".
#' greatest dates and the file name that contains the greatest date.
#' @param verbose boolean value. When set to true, it prints operational messages including
#' greatest dates and the file name that contains the greatest date.
#' @export
#' @references For details on is:issue or is:pull-request see \url{https://docs.github.com/en/rest/search/search?apiVersion=2022-11-28}
#' @references For details on timestampes, se \url{https://docs.github.com/en/search-github/searching-on-github/searching-issues-and-pull-requests#search-by-when-an-issue-or-pull-request-was-created-or-last-updated}
#' @seealso  \code{link{github_api_project_issue_or_pr_comment_refresh}} to refresh comment data
#' @seealso  \code{link{github_api_project_issue_or_pr_comments}} to refresh issue data
github_api_project_issue_by_date <- function(owner,
                                             repo,
                                             token,
                                             date_lower_bound = NULL,
                                             date_upper_bound = NULL,
                                             issue_or_pr,
                                             verbose = FALSE) {
  # Base query to include repository and issue filter
  query <- paste0("repo:",owner, "/", repo, " ", issue_or_pr)
  message(query)

  # Add date filters to the query if provided
  if (!is.null(date_lower_bound) && !is.null(date_upper_bound)) {
    query <- sprintf("%s created:%s..%s", query, date_lower_bound, date_upper_bound)
    if(verbose){
      message("Downloading issue data created between ", date_lower_bound, " and ", date_upper_bound, ".")
    }
  } else if (!is.null(date_lower_bound)) {
    query <- sprintf("%s created:>=%s", query, date_lower_bound)
  } else if (!is.null(date_upper_bound)) {
    query <- sprintf("%s created:<=%s", query, date_upper_bound)
  }

  # Only proceed if at least one date bound is provided
  if (is.null(date_lower_bound) && is.null(date_upper_bound)) {
    stop("At least one of 'date_lower_bound' or 'date_upper_bound' must be provided.
         If you have provided at least one, it may be improperly formatted.")
  }

  # Perform the API call using the constructed query
  gh_response <- github_api_project_issue_search(owner, repo, token, query, issue_or_pr,verbose=TRUE)

  return(gh_response)
}

###### Github Issue or Pull Request #####

#' Download Project Issue's or Pull Request's Comments
#'
#' Download Issues' and Pull Request's Comments from "GET /repos/{owner}/{repo}/issues/comments" endpoint.
#' All Pull Requests are counted as Issues, but not all Issues are Pull Requests
#' Optional parameter since is used to download comments updated after the specified date.
#' If the value of since is NULL, it is not passed to the API call and all comments are downloaded.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param since Optional parameter to specify pulling only comments updated after this date
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/issues#list-issue-comments-for-a-repository} and
#' \url{https://docs.github.com/en/rest/guides/working-with-comments#pull-request-comments}.
#' @export
github_api_project_issue_or_pr_comments <- function(owner,repo,token,since=NULL){
  if (!is.null(since)){
    gh::gh("GET /repos/{owner}/{repo}/issues/comments",
           owner=owner,
           repo=repo,
           page=1,
           per_page=100,
           .token=token,
           since=since)
  } else {
    gh::gh("GET /repos/{owner}/{repo}/issues/comments",
           owner=owner,
           repo=repo,
           page=1,
           per_page=100,
           .token=token)
  }
}

#' Parse Issues' or Pull Requests' Comments JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#'
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_issue_or_pr_comments <- function(api_responses){
  parse_response <- function(api_response){
    parsed_response <- list()
    parsed_response[["comment_id"]] <- api_response[["id"]]
    parsed_response[["html_url"]] <- api_response[["html_url"]]
    parsed_response[["issue_url"]] <- api_response[["issue_url"]]
    parsed_response[["created_at"]] <- api_response[["created_at"]]
    parsed_response[["updated_at"]] <- api_response[["updated_at"]]
    parsed_response[["comment_user_login"]] <- api_response[["user"]][["login"]]
    parsed_response[["author_association"]] <- api_response[["author_association"]]
    parsed_response[["body"]] <- api_response[["body"]]

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

#' Download Project issues or pr comments after certain date
#'
#' Uses the adopted file name convention by \code{\link{github_api_iterate_pages}} to identify
#' the latest downloaded Github created_at date among the directory(intended to be the  folder).
#' It uses this date to construct a query and calls \code{\link{github_api_project_issue_or_pr_comments}}
#'
#' If no files exist in the file_save_path,\code{link{github_api_project_issue_or_pr_comments}}
#' is called with no additional query and all comments are downloaded.
#'
#' Because the endpoint this function relies on is based on the updated timestamp, running the refresher
#' will download the most recent version of the comment changes. Only the most recent version of the comment will
#' be downloaded, not all copies. However, if the same comment was modified before the next refresh call,
#' then if the refresher function was executed again, then this would result in two comments with the same
#' comment id being present in the table. This can be addressed by performing a group by over the comment\_id
#' in the generated parsed table, and selecting to return the max(updated_at) comment, resulting in a table
#' that only the most recent comment verson as of the latest time the refresher was executed.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param file_save_path the save path for the issue comments folder
#' @param verbose boolean value. When set to true, it prints operational messages including
#' greatest dates and the file name that contains the greatest date.
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/issues#list-repository-issues}.
#' @seealso  \code{link{github_api_project_issue_or_pr_comments}} to download all comment data
#' @seealso  \code{link{format_created_at_from_file}} for function that iterates through
#' a .json file and returns the greatest 'created_at' value
#' @seealso  \code{link{github_api_iterate_pages}} to write data returned by this function to file as .json
#' @seealso  \code{link{github_api_project_issue_or_pr_comments}} to call issue/comments endpoint
github_api_project_issue_or_pr_comment_refresh <- function(owner,repo,token,file_save_path,verbose=TRUE){
  # Check if the file is empty by checking its size
  # List all files and subdirectories in the directory
  contents <- list.files(path = save_path_issue_or_pr_comments)

  # If the file is empty, download all issues
  if(length(contents) == 0) {
    # Run regular downloader
    issues <- github_api_project_issue_or_pr_comments(owner,repo,token)
    return (issues)
  } else {
    # Get the name of the file with the most recent date
    latest_updated_issue_or_pr_comment <- paste0(file_save_path, parse_jira_latest_date(save_path_issue_or_pr_comments))
    latest_updated_issue_or_pr_comment <- (head(latest_updated_issue_or_pr_comment,1))
    # get the created_at value
    message("got file", latest_updated_issue_or_pr_comment)
    created <- format_created_at_from_file(latest_updated_issue_or_pr_comment, item="")

    # Convert the string to a POSIXct object
    time_value <- as.POSIXct(created, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

    # Add one second
    new_time_value <- time_value + 1

    # Format the new time value back into the original string format
    formatted_new_time_value <- format(new_time_value, "%Y-%m-%dT%H:%M:%SZ")

    if(verbose){
      message("file name with greatest date: ",latest_updated_issue_or_pr_comment)
      message("Latest date: ",formatted_new_time_value)
    }
    # Make the API call
    gh_response <- github_api_project_issue_or_pr_comments(owner,repo,token,formatted_new_time_value)
  } #end if/else
}

#' Download Github comment Data by Date
#'
#' Appends a 'since' query to the issue/comments api request and returns the first page of the result.
#'
#' #' Acceptable formats for `since` are:
#'
#' * "YYYY-MM-DD"
#' * "YYYY-MM-DDTHH:MM"
#' * "YYYY-MM-DDTHH:MM:SS"
#' * "YYYY-MM-DDTHH:MM:SSZ"
#' * "YYYY-MM-DDTHH:MM:SS+00:00"
#' * NULL
#'
#'#' For example: `since="2020-07-04"` (a comment ocurring at the exact specified time will also be downloaded).
#'
#' For further details on the `since` Query see [the associated Github API documentation](https://docs.github.com/en/rest/issues/comments?apiVersion=2022-11-28#:~:text=asc%2C%20desc-,since,-string).
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param since The lower bound. Comments created and/or updated after this date will be retrieved.
#' @param verbose boolean value. When set to true, it prints operational messages including
#' greatest dates and the file name that contains the greatest date.
#' @export
#' @seealso  \code{link{github_api_project_issue_or_pr_comment_refresh}} to refresh comment data
#' @seealso  \code{link{github_api_project_issue_refresh}} to refresh issue data
#' @seealso  \code{link{github_api_project_issue_or_pr_comments}} to call issue/comments endpoint
github_api_project_issue_or_pr_comments_by_date <- function(owner,
                                                            repo,
                                                            token,
                                                            since,
                                                            verbose = FALSE) {
  if (is.null(since)) {
    stop("The lower bound parameter is empty or improperly formatted")
  }
  if(verbose){
    message("Downloading comments updated/created after: ", since)
  }
  # Make the API call
  gh_response <- github_api_project_issue_or_pr_comments(owner,repo,token,since)
  return(gh_response)
}

###### Github Pull Requests ######

#' Download Project Pull Requests
#'
#' Download  Pull Requests from "GET /repos/{owner}/{repo}/pulls" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/pulls#list-pull-requests}.
github_api_project_pull_request <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/pulls",
         owner=owner,
         repo=repo,
         state="all",
         page=1,
         per_page=100,
         .token=token)
}

#' Parse Pull Requests JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#'
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_pull_request <- function(api_responses){
  parse_response <- function(api_response){
    parsed_response <- list()
    parsed_response[["pr_id"]] <- api_response[["id"]]
    parsed_response[["pr_number"]] <- api_response[["number"]]
    parsed_response[["html_url"]] <- api_response[["html_url"]]
    parsed_response[["url"]] <- api_response[["url"]]
    parsed_response[["created_at"]] <- api_response[["created_at"]]
    parsed_response[["updated_at"]] <- api_response[["updated_at"]]
    parsed_response[["state"]] <- api_response[["state"]]
    parsed_response[["pr_user_login"]] <- api_response[["user"]][["login"]]
    parsed_response[["author_association"]] <- api_response[["author_association"]]
    parsed_response[["title"]] <- api_response[["title"]]
    parsed_response[["body"]] <- api_response[["body"]]

    parsed_response[["labels"]] <- api_response[["labels"]]
    if(length(parsed_response[["labels"]]) > 0){
      parsed_response[["labels"]] <- stringi::stri_c(sapply(parsed_response[["labels"]],"[[","name"),collapse = ",")
    }else{
      parsed_response[["labels"]] <- NA_character_
    }

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

#' Download Project's Pull Request Comments
#'
#' @description Downloads the Pull Request Comments from `GET /repos/{owner}/{repo}/pulls/comments` endpoint.
#' Optional parameter since is used to download comments updated after the specified date.
#' If the value of since is NULL, it is not passed to the API call and all comments are downloaded.
#' NOTE: This function is different from the `github_api_project_issue_or_pr_comments`.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param since Optional parameter to specify pulling only comments updated after this date
#' @references For details, see \url{https://docs.github.com/en/rest/pulls/comments?apiVersion=2022-11-28#about-pull-request-review-comments}
#' @export
github_api_project_pr_comments <- function(owner, repo, token, since=NULL) {
  if (!is.null(since)) {
    # Get all pull request comments
    api_response <- gh::gh("GET /repos/{owner}/{repo}/pulls/comments",
          owner=owner,
          repo=repo,
          page=1,
          per_page=100,
          .token=token,
          since=since)
  } else {
    api_response <- gh::gh("GET /repos/{owner}/{repo}/pulls/comments",
           owner=owner,
           repo=repo,
           page=1,
           per_page=100,
           .token=token)
  }
}

#' Parse Pull Requests' Comments JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#' Note this is different from the `github_parse_project_issue_or_pr_comments` function.
#' This function only parses for the in-line code and comments made on the pull request.
#' `review_id` A integer value that refers to the review comment made when creating the review.
#' `file_path` A string containing the filepath of the file the review comment is made on.
#' `start_line` An integer value of the first line number if multiple lines are selected when
#' making the comment, or null if only 1 line is selected. Will also return null if line is deleted
#' in later commits.
#' `line` An integer value of the last line number if multiple lines are selected when
#' making the comment, or the the line number when only 1 line is selected. Will return
#' 1 if no lines are selected when making the comment or null if the line is deleted
#' in later commits.
#' `original_start_line` An integer value of the first line number if multiple lines are selected when
#' making the comment, or null if only 1 line is selected. The line number integer will match the line
#' number at the time the review comment is made, regardless if a later commit changes the line number.
#' `original_line` An integer value of the last line number if multiple lines are selected when
#' making the comment, or the the line number when only 1 line is selected. Will return
#' 1 if no lines are selected when making the comment. The line number integer will match the line
#' number at the time the review comment is made, regardless if a later commit changes the line number.
#' `diff_hunk` A string containing the code hunk the review comment is referencing.
#' It will contain the lines from the start of the (+/-) hunk until the line
#' associated by the review comment. Will return null if the review comment is not
#' tied to a specific line.
#' `body` A string containing main text of the review comment.
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_pr_comments <- function(api_responses) {
  parse_response <- function(api_response) {
    parsed_response <- list()
    parsed_response[["review_id"]] <- api_response[["pull_request_review_id"]]
    parsed_response[["comment_id"]] <- api_response[["id"]]
    parsed_response[["html_url"]] <- api_response[["html_url"]]
    parsed_response[["created_at"]] <- api_response[["created_at"]]
    parsed_response[["updated_at"]] <- api_response[["updated_at"]]
    parsed_response[["comment_user_login"]] <- api_response[["user"]][["login"]]
    parsed_response[["author_association"]] <- api_response[["author_association"]]
    parsed_response[["file_path"]] <- api_response[["path"]]
    parsed_response[["start_line"]] <- api_response[["start_line"]]
    parsed_response[["line"]] <- api_response[["line"]]
    parsed_response[["original_start_line"]] <- api_response[["original_start_line"]]
    parsed_response[["original_line"]] <- api_response[["original_line"]]
    parsed_response[["position"]] <- api_response[["position"]]
    parsed_response[["diff_hunk"]] <- api_response[["diff_hunk"]]
    parsed_response[["body"]] <- api_response[["body"]]
    parsed_response[["commit_id"]] <- api_response[["commit_id"]]


    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

#' Download Project Pull Request Comments Refresh
#'
#' Uses the adopted file name convention by \code{\link{github_api_iterate_pages}} to identify
#' the latest downloaded Github created_at date among the directory(intended to be the  folder).
#' It uses this date to construct a query and calls \code{\link{github_api_project_pr_comments}}
#'
#' If no files exist in the file_save_path,\code{link{github_api_project_pr_comments}}
#' is called with no additional query and all comments are downloaded.
#'
#' Because the endpoint this function relies on is based on the updated timestamp, running the refresher
#' will download the most recent version of the comment changes. Only the most recent version of the comment will
#' be downloaded, not all copies. However, if the same comment was modified before the next refresh call,
#' then if the refresher function was executed again, then this would result in two comments with the same
#' comment id being present in the table. This can be addressed by performing a group by over the comment\_id
#' in the generated parsed table, and selecting to return the max(updated_at) comment, resulting in a table
#' that only the most recent comment verson as of the latest time the refresher was executed.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param file_save_path the save path for the pr comments folder
#' @param verbose boolean value. When set to true, it prints operational messages including
#' greatest dates and the file name that contains the greatest date.
#' @export
#' @references For details, see For details, see \url{https://docs.github.com/en/rest/pulls/comments?apiVersion=2022-11-28#about-pull-request-review-comments}.
#' @seealso  \code{link{github_api_project_pr_comments}} to download all pull request comment data
#' @seealso  \code{link{format_created_at_from_file}} for function that iterates through
#' a .json file and returns the greatest 'created_at' value
#' @seealso  \code{link{github_api_iterate_pages}} to write data returned by this function to file as .json
#' @seealso  \code{link{github_api_project_pr_comments}} to call pr comments endpoint
github_api_project_pr_comments_refresh <- function(owner,repo,token,file_save_path=save_path_pr_comments,verbose=TRUE){
  # Check if the file is empty by checking its size
  # List all files and subdirectories in the directory
  contents <- list.files(path = file_save_path)
  # If the file is empty, download all pr comments
  if(length(contents) == 0) {
    if (verbose) {
      message(file_save_path, " filepath is empty, running regular downloader.")
    }
    # Run regular downloader
    pr_comments <- github_api_project_pr_comments(owner,repo,token)
    return (pr_comments)
  } else {
    # Get the name of the file with the most recent date
    latest_updated_pr_comments <- paste0(file_save_path, parse_jira_latest_date(file_save_path))
    latest_updated_pr_comments <- (head(latest_updated_pr_comments,1))

    if (verbose) {
      message("File with most recent date: ", latest_updated_pr_comments)
    }
    # get the created_at value
    created <- format_created_at_from_file(latest_updated_pr_comments, item="")

    # Convert the string to a POSIXct object
    time_value <- as.POSIXct(created, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

    # Add one second
    new_time_value <- time_value + 1

    # Format the new time value back into the original string format
    formatted_new_time_value <- format(new_time_value, "%Y-%m-%dT%H:%M:%SZ")

    if(verbose){
      message("file name with greatest date: ",latest_updated_pr_comments)
      message("Latest date: ",formatted_new_time_value)
    }
    # Make the API call
    gh_response <- github_api_project_pr_comments(owner,repo,token,formatted_new_time_value)
  } #end if/else
}

###### Github Commits ######

#' Download Project Commits
#'
#' Download Commits from "GET /repos/{owner}/{repo}/commits" endpoint.
#' Differently from parsing commits by git cloning the repository, this JSON provides
#' the GitHub user id, which allows for linking file changes and issue events by the
#' same author without relying on identity matching heuristics.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @references For details, see \url{https://docs.github.com/en/rest/reference/repos#commits}.
#' @export
github_api_project_commits <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/commits",
         owner=owner,
         repo=repo,
         page=1,
         per_page=100,
         .token=token)
}
#' Parse Commits JSON to Table
#'
#' Note not all columns available in the downloaded json are parsed.
#'
#' @param api_responses API response obtained from github_api_* function.
#' @export
github_parse_project_commits <- function(api_responses){
  parse_response <- function(api_response){
    parsed_response <- list()
    parsed_response[["author_login"]] <- api_response[["author"]][["login"]]
    parsed_response[["commit_author_name"]] <- api_response[["commit"]][["author"]][["name"]]
    parsed_response[["commit_author_email"]] <- api_response[["commit"]][["author"]][["email"]]
    parsed_response[["committer_login"]] <- api_response[["committer"]][["login"]]
    parsed_response[["commit_committer_name"]] <- api_response[["commit"]][["committer"]][["name"]]
    parsed_response[["commit_committer_email"]] <- api_response[["commit"]][["committer"]][["email"]]
    parsed_response[["commit_message"]] <- api_response[["commit"]][["message"]]

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}

###### Github API Helper Functions ######

#' Returns token remaining available requests.
#'
#' @param token Your GitHub API token
#' @references For more details see \url{https://docs.github.com/en/free-pro-team@latest/rest/overview/resources-in-the-rest-api#rate-limiting}.
#' @export
github_api_rate_limit <- function(token){
  gh::gh_rate_limit(
    response = NULL,
    .token = token,
    .api_url = NULL,
    .send_headers = NULL
  )
}

#' Obtain the next GitHub response page.
#'
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
#' @keywords internal
github_api_page_next <- function(gh_response){
  gh::gh_next(gh_response)
}

#' Obtain the previous GitHub response page.
#'
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
#' @keywords internal
github_api_page_prev <- function(gh_response){
  gh::gh_prev(gh_response)
}

#' Obtain the first GitHub response page.
#'
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
#' @keywords internal
github_api_page_first <- function(gh_response){
  gh::gh_first(gh_response)
}


#' Obtain the last GitHub response page.
#'
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
#' @keywords internal
github_api_page_last <- function(gh_response){
  gh::gh_last(gh_response)
}

#' GitHub Page Iterator
#'
#' GitHub API endpoints return data in pages, each containing by default 100 entries.
#' This iterator can be used to iterate over the next page in order to download all the
#' project's data available from the endpoint (up to a user-defined maximum or the remaining
#' available requests in the used user's token). This function can differentiate between
#' data downloaded from search endpoint or not, in which the issues are differently nested.
#' It also differentiates these endpoints from commit data, which also uses a different level
#' of nesting. This is important in order to extract the minimum and maximum value of time created
#' for each page for the naming convention, which is owner_repo_(min time)_(max time).json.
#'
#' @param token Your GitHub API token
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @param save_folder_path A folder path to save the downloaded json pages "as-is".
#' @param prefix Prefix to be added to every json file name
#' @param max_pages The maximum number of pages to download. MAX = Available token requests left
#' @param verbose Boolean value that prints operating messages when set to TRUE, does not print when false.
#' Operating messages may be details about certain parts of the code correctly executing or printing names
#' of files created, etc.
#' @references For details see \url{https://docs.github.com/en/free-pro-team@latest/rest/guides/traversing-with-pagination}.
#' @export
#' @keywords internal
github_api_iterate_pages <- function(token,gh_response,save_folder_path,prefix=NA,max_pages=NA,verbose=TRUE){
  page_number <- 1

  data_exists = TRUE
  # Set the max_pages to your api limit unless specified
  if(is.na(max_pages)){
    max_pages <- github_api_rate_limit(token)$remaining
  }

  # determine if the passed data is from the refresh folder or not
  json_string <- toJSON(gh_response, pretty = TRUE, auto_unbox = TRUE)
  json_data <- fromJSON(json_string, simplifyVector = TRUE)

  # Check if 'total_count' is present at the top level of the JSON structure
  # This allows us to determine if the data is formatted from the search endpoint or not
  if ("total_count" %in% names(json_data)) {
    is_issue_refresh <- TRUE
  } else {
    is_issue_refresh <- FALSE
  }
  # message(is_issue_refresh)

  # Check if it is

  #Get the most and least recent 'created_at' date in unixtime in this page
  while(!is.null(gh_response) & page_number < max_pages){

    #  Set the file name from the config file. It will be modified in the following code
    file_name <- save_folder_path
    if(length(gh_response) > 0) {
      #  Extract 'created_at' dates. Different nesting levels for refresh data or not
      # Run this code if it's not issue_refresh. Important for different levels of nesting
      if (is_issue_refresh==FALSE){
        # Make list of all created_dates
        created_dates <- sapply(gh_response, function(issue) issue$created_at)
        # Remove NULL entries from the list. The list will be NULL if it is commit data currently
        created_dates <- Filter(Negate(is.null), created_dates)

        # Check if the list is NULL, signifying this is commit data
        if (length(created_dates)==0){
          created_dates <- sapply(gh_response, function(issue) {
            if (!is.null(issue$commit) && !is.null(issue$commit$author) && !is.null(issue$commit$author$date)) {
              return(issue$commit$author$date)
            } else {
              return(NA) # Return NA if the path does not exist
            }
          })
        }

        # Run this code if it is for issue refresh
      } else {
        # Make list of all created dates
        created_dates <- sapply(gh_response$items, function(issue) issue$created_at)
        # End the loop if there is no usable data
        # message(gh_response)
        # message(created_dates)
        if (length(created_dates)==0){
          if(verbose){
            message("Nothing left to download")
          }
          break
        }
      }
      #
      # Convert to POSIXct date objects
      # date_objects <- as.POSIXct(created_dates, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
      date_objects <- as.POSIXct(created_dates, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

      # Find the greatest and smallest date
      latest_date <- max(date_objects)
      latest_date_unix <- as.numeric(latest_date)
      oldest_date <- min(date_objects)
      oldest_date_unix <- as.numeric(oldest_date)

      # Append oldest and latest dates to the file name
      file_name <- paste0(file_name, "_", oldest_date_unix)
      file_name <- paste0(file_name, "_", latest_date_unix, ".json")

      # Print the latest and oldest dates and file name
      if (verbose){
        message("Latest date:", latest_date_unix)
        message("Oldest date:", oldest_date_unix)
        message("File name: ", file_name)
        message("extracted dates for page ", page_number)
      }
    } else {
      data_exists = FALSE
      if(verbose){
        message("Nothing to download")
      }
    }

    # Save the pages to file
    if (data_exists == TRUE){
      # construct the file name
      file_name <- paste0(save_folder_path,
                          owner,"_",repo,"_",
                          oldest_date_unix, "_",
                          latest_date_unix,
                          ".json")
      # Write to file
      write_json(gh_response,file_name,
                 pretty=TRUE,auto_unbox=TRUE)
      if (verbose){
        message("Written to file: ", file_name)
      }
    }
    # increment the page number
    page_number <- page_number + 1
    res <- try(
      {
        gh_response <- github_api_page_next(gh_response)
      },silent=TRUE)
    if(inherits(res,"try-error")) {
      gh_response <- NULL
    }
  }
}


#' Retrieve greatest 'created_at' value from file
#'
#' Function to read a JSON file along a path and return the 'created_at'
#' date of the greatest value for the issue key. Note that the 'created_at'
#' value differs in how it is nested. This format is returned by the
#' issue endpoint currently, but is in level 2 of data returned by search endpoint.
#' So we allow the input of an item_path parameter to specify the level of nesting
#'
#' @param file_name the path and the file name. For example:
#' ../../rawdata/github/kaiaulu/issue_or_pr_comment/sailuh_kaiaulu_issue_or_pr_comment_1701216000_1701261374.json
#' @param item_path specifies the level of nesting to look for the created_at value. This was
#' implemented given that the results of the search endpoint are differently nested than others.

#' @seealso  \code{link{github_api_project_issue_or_pr_comment_refresh}} to refresh comment data
#' @seealso  \code{link{github_api_project_issue_refresh}} to refresh issue data
#' @keywords internal
format_created_at_from_file <- function(file_name,item_path) {
  # Read the JSON file
  json_data <- fromJSON(txt= file_name, simplifyVector = FALSE)

  # Navigate to the correct level in the JSON structure based on the item_path
  data_to_process <- if (item_path != "") {
    eval(parse(text=paste0("json_data$", item_path)))
  } else {
    json_data
  }

  # Initialize a variable to keep track of the greatest date
  greatest_date <- as.POSIXct("1970-01-01T00:00:00Z", tz = "UTC")

  # Iterate through each element in the data_to_process
  for (item in data_to_process) {
    # Extract 'created_at' date and convert to POSIXct
    current_date <- as.POSIXct(item$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    # Update greatest_date if the current item's date is later
    if (current_date > greatest_date) {
      greatest_date <- current_date
    }
  }

  # Format the greatest date found
  formatted_greatest_date <- format(greatest_date, "%Y-%m-%dT%H:%M:%SZ")

  # Return the latest 'created_at' value
  return(formatted_greatest_date)
}


