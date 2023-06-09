# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


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

#' Download Project Issue's or Pull Request's Comments
#'
#' Download Issues' or Pull Request's Comments from "GET /repos/{owner}/{repo}/issues/comments" endpoint.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/issues#list-issue-comments-for-a-repository} and
#' \url{https://docs.github.com/en/rest/guides/working-with-comments#pull-request-comments}.
#' @export
github_api_project_issue_or_pr_comments <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/issues/comments",
         owner=owner,
         repo=repo,
         page=1,
         per_page=100,
         .token=token)
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

#' Returns token remaining available requests.
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
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
github_api_page_next <- function(gh_response){
  gh::gh_next(gh_response)
}

#' Obtain the previous GitHub response page.
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
github_api_page_prev <- function(gh_response){
  gh::gh_prev(gh_response)
}

#' Obtain the first GitHub response page.
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
github_api_page_first <- function(gh_response){
  gh::gh_first(gh_response)
}


#' Obtain the last GitHub response page.
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
github_api_page_last <- function(gh_response){
  gh::gh_last(gh_response)
}

#' GitHub Page Iterator
#'
#' GitHub API endpoints return data in pages, each containing by default 100 entries.
#' This iterator can be used to iterate over the next page in order to download all the
#' project's data available from the endpoint (up to a user-defined maximum or the remaining
#' available requests in the used user's token).
#'
#' @param token Your GitHub API token
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @param save_folder_path A folder path to save the downloaded json pages "as-is".
#' @param prefix Prefix to be added to every json file name
#' @param max_pages The maximum number of pages to download. MAX = Available token requests left
#' @references For details see \url{https://docs.github.com/en/free-pro-team@latest/rest/guides/traversing-with-pagination}.
#' @export
github_api_iterate_pages <- function(token,gh_response,save_folder_path,prefix=NA,max_pages=NA){
  page_number <- 1

  if(is.na(max_pages)){
    max_pages <- github_api_rate_limit(token)$remaining
  }

  while(!is.null(gh_response) & page_number < max_pages){
    write_json(gh_response,paste0(save_folder_path,
                                  owner,"_",repo,"_",prefix,"_","p_",page_number,
                                  ".json"),
               pretty=TRUE,auto_unbox=TRUE)
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
