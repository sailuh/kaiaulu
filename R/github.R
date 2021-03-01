# https://docs.github.com/en/free-pro-team@latest/rest/guides/traversing-with-pagination


# https://docs.github.com/en/free-pro-team@latest/rest/reference/issues#events
# https://docs.github.com/en/free-pro-team@latest/developers/webhooks-and-events/issue-event-types
#' @export
github_api_project_issue <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/issues/events",
         owner=owner,
         repo=repo,
         type="IssuesEvent",
         page=1,
         per_page=100,
         .token=token)
}


# Parser
#' @export
github_parse_project_issue <- function(api_responses){
  parse_response <- function(api_response){
    parsed_response <- list()
    parsed_response[["id"]] <- api_response[["id"]]
    parsed_response[["created_at"]] <- api_response[["created_at"]]
    parsed_response[["commit_id"]] <- api_response[["commit_id"]]
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
    return(data.table(data.frame(parsed_response)))
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}
# https://docs.github.com/en/rest/reference/repos#commits
#' @export
github_api_project_commits <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/commits",
         owner=owner,
         repo=repo,
         page=1,
         per_page=100,
         .token=token)
}
# Parser
# under commit(author:name and email), committer(name and email), message. Under author (login), under committer (login)
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
    return(data.table(data.frame(parsed_response)))
  }
  rbindlist(lapply(api_responses,parse_response),fill=TRUE)
}
# https://docs.github.com/en/free-pro-team@latest/rest/reference/repos#list-repository-contributors
#' @export
github_api_project_contributor <- function(owner,repo,token){
  gh::gh("GET /repos/{owner}/{repo}/contributors",
         owner=owner,
         repo=repo,
         page=1,
         per_page=100,
         .token=token)
}

# https://docs.github.com/en/free-pro-team@latest/rest/overview/resources-in-the-rest-api#rate-limiting
#' @export
github_api_rate_limit <- function(token){
  gh::gh_rate_limit(
    response = NULL,
    .token = token,
    .api_url = NULL,
    .send_headers = NULL
  )
}

#' @export
github_api_page_next <- function(gh_response){
  gh::gh_next(gh_response)
}

#' @export
github_api_page_prev <- function(gh_response){
  gh::gh_prev(gh_response)
}

#' @export
github_api_page_first <- function(gh_response){
  gh::gh_first(gh_response)
}

#' @export
github_api_page_last <- function(gh_response){
  gh::gh_last(gh_response)
}
