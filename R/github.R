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
  gh_rate_limit(
    response = NULL,
    .token = token,
    .api_url = NULL,
    .send_headers = NULL
  )
}

#' @export
github_api_page_next <- function(gh_response){
  gh_next(gh_response)
}

#' @export
github_api_page_prev <- function(gh_response){
  gh_prev(gh_response)
}

#' @export
github_api_page_first <- function(gh_response){
  gh_first(gh_response)
}

#' @export
github_api_page_last <- function(gh_response){
  gh_last(gh_response)
}
