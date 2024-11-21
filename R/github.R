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
#' @param github_replies_folder_path The folder path to where the github api json files have been downloaded.
#' @return A single reply table which combines the communication from the three jsons.
#' @export
parse_github_replies <- function(github_replies_folder_path){

  issues_json_folder_path <- paste0(github_replies_folder_path,"/issue/")
  pull_requests_json_folder_path <- paste0(github_replies_folder_path,"/pull_request/")
  comments_json_folder_path <- paste0(github_replies_folder_path,"/issue_or_pr_comment/")
  commit_json_folder_path <- paste0(github_replies_folder_path,"/commit/")

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

############## Downloader ##############

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

#' Download Discussions
#'
#' Download Discussions from GraphQL API endpoint.
#' Uses a query to only obtain data defined by the user.
#' GitHub API endpoints return data in pages, each containing by default 100 entries.
#' This function by default iterates over the next page in order to download all the
#' project's data available from the endpoint (up to the remaining
#' available requests in the used user's token).
#' The user can also define the maximum number of pages to download.
#'
#' @param token Your Github API token
#' @param owner Github's repository owner (e.g. sailuh)
#' @param repo Github's repository name (e.g. kaiaulu)
#' @param save_folder_path A folder path to save the downloaded json pages "as-is".
#' @param prefix Prefix to be added to every json file name
#' @param max_pages The maximum number of pages to download. MAX = Available token requests left
#' @export
github_api_discussions <- function(token, owner, repo, save_folder_path, max_pages = NA, verbose = TRUE){
  page_number <- 1
  cursor <- NULL

  # Set the max number of pages to your api limit, unless specified
  if(is.na(max_pages)){
    max_pages <- github_api_rate_limit(token)$remaining
  }

  while(page_number < max_pages){
    # Make the GraphQL query
    query <- paste0('query {
      repository (owner:"', owner, '", name:"', repo, '") {
        discussions (first: 100', if(!is.null(cursor)) paste0(', after: "', cursor,'"'),') {
          pageInfo {
            hasNextPage
            endCursor
          }
          edges {
            node {
              title
              bodyText
              author { login }
              createdAt
              category { name }
              id
              answer { id }
              comments(first: 100) {
                edges {
                  node {
                    discussion { id }
                    bodyText
                    author { login }
                    id
                    createdAt
                  }
                }
              }
            }
          }
        }
      }
    }')

    # Make the request to the GraphQL API
    gh_response <- gh::gh("POST /graphql", query=query, .token=token)

    # Make the list of all created_dates
    created_dates <- sapply(gh_response[["data"]][["repository"]][["discussions"]][["edges"]],
                            function(edge) edge[["node"]][["createdAt"]])
    # Remove NULL entries from created_dates
    created_dates <- Filter(Negate(is.null), created_dates)

    # Convert to POSIXct date objects
    date_objects <- as.POSIXct(created_dates, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

    # Find the greatest and smallest date
    latest_date <- max(date_objects)
    latest_date_unix <- as.numeric(latest_date)
    oldest_date <- min(date_objects)
    oldest_date_unix <- as.numeric(oldest_date)

    # Construct the file_name
    file_name <- paste0(save_folder_path,
                        owner, "_", repo, "_", oldest_date_unix, "_", latest_date_unix,
                        ".json")
    # Save the json to the folder path
    write_json(gh_response, file_name, pretty=TRUE, auto_unbox=TRUE)

    if (verbose) {
      # Print the latest and oldest dates to the file name
      message("Latest date: ", latest_date_unix)
      message("Oldest date: ", oldest_date_unix)
      message("File name: ", file_name)
      message("Extracted dates for page ", page_number)
    }

    has_next_page <- gh_response[["data"]][["repository"]][["discussions"]][["pageInfo"]][["hasNextPage"]]

    if (has_next_page){
      cursor <- gh_response[["data"]][["repository"]][["discussions"]][["pageInfo"]][["endCursor"]]
      page_number <- page_number + 1
    }
    else {
      break
    }
  }
}

github_api_discussions_refresh <- function(token, owner, repo, save_folder_path, verbose = TRUE) {
  # List all files and subdirectories in the directory
  contents <- list.files(save_folder_path, all.files= TRUE)

  # If the file is empty, download all discussions
  if (length(contents) == 0) {
    # Run the regular downloader
    discussions <- github_api_discussions(token, owner, repo, save_folder_path)
    return (discussions)
  }

  # Get the name of the file with the most recent date
  latest_discussion <- contents[which.max(sapply(contents, function (filename) {
    # Use regex to get timestamp
    as.numeric(sub(".*_(\\d+)\\.json$", "\\1", basename(filename)))
  }))]

  # Read the JSON file
  json_data <- fromJSON(file.path(save_folder_path, latest_discussion), simplifyVector = FALSE)
  # Get the created_at values
  created_at <- sapply(json_data[["data"]][["repository"]][["discussions"]][["edges"]],
                       function (edge) edge[["node"]][["createdAt"]])
  # Find the latest created_at
  created_at <- max(created_at)
  # Convert to a POSIXct object
  created_at <- as.POSIXct(created_at, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  # Add one second
  created_at <- created_at + 1
  # Convert back to original
  created_at <- format(created_at, "%Y-%m-%dT%H:%M:%SZ")

  if (verbose) {
    message("File name with latest date: ", latest_discussion)
    message("Latest date: ", created_at)
  }

  page_number <- 1
  cursor <- NULL
  while (TRUE) {
    # Form a new query
    query <- paste0('query {
      repository (owner:"', owner, '", name:"', repo, '") {
        discussions (first: 100, orderBy: {field: CREATED_AT, direction: ASC}, after: "', created_at, '") {
          pageInfo {
            hasNextPage
            endCursor
          }
          edges {
            node {
              title
              bodyText
              author { login }
              createdAt
              category { name }
              id
              answer { id }
              comments(first: 100) {
                edges {
                  node {
                    discussion { id }
                    bodyText
                    author { login }
                    id
                    createdAt
                  }
                }
              }
            }
          }
        }
      }
    }')
    # Make a new API call with the query
    gh_response <- gh::gh("POST /graphql", query=query, .token=token)

    # Check if response has new discussions
    if (length(gh_response[["data"]][["repository"]][["discussions"]][["edges"]]) == 0 && verbose) {
      message("No new discussions")
      break
    }

    # Make the list of all created_dates
    created_dates <- sapply(gh_response[["data"]][["repository"]][["discussions"]][["edges"]],
                            function(edge) edge[["node"]][["createdAt"]])
    # Remove NULL entries from created_dates
    created_dates <- Filter(Negate(is.null), created_dates)

    # Convert to POSIXct date objects
    date_objects <- as.POSIXct(created_dates, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

    # Find the greatest and smallest date
    latest_date <- max(date_objects)
    latest_date_unix <- as.numeric(latest_date)
    oldest_date <- min(date_objects)
    oldest_date_unix <- as.numeric(oldest_date)

    # Construct the file_name
    file_name <- paste0(save_folder_path,
                        owner, "_", repo, "_", oldest_date_unix, "_", latest_date_unix,
                        ".json")
    # Save the json to the folder path
    write_json(gh_response, file_name, pretty=TRUE, auto_unbox=TRUE)

    if (verbose) {
      # Print the latest and oldest dates to the file name
      message("Latest date: ", latest_date_unix)
      message("Oldest date: ", oldest_date_unix)
      message("File name: ", file_name)
      message("Extracted dates for page ", page_number)
    }

    has_next_page <- gh_response[["data"]][["repository"]][["discussions"]][["pageInfo"]][["hasNextPage"]]

    if (has_next_page){
      cursor <- gh_response[["data"]][["repository"]][["discussions"]][["pageInfo"]][["endCursor"]]
      page_number <- page_number + 1
    }
    else {
      break
    }
  }
}

#' Parse Discussions JSON to Table
#'
#' @description This function parses through the JSON of Github Discussions
#' downloaded from the `github_api_discussions` function, and turns it into
#' a table. This function does not parse the data for the discussion comments.
#'
#' @param api_responses API response obtained from `github_api_discussions` function.
#' @return The parsed table on the discussions.
#' @export
github_parse_discussions <- function(api_response) {
  parsed_response <- list()
  parsed_response[["author_login"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                              function(edge) { edge[["node"]][["author"]][["login"]] }))
  parsed_response[["title"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                       function(edge) { edge[["node"]][["title"]] }))
  parsed_response[["discussion_id"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                               function(edge) { edge[["node"]][["id"]] }))
  parsed_response[["created_at"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                            function(edge) { edge[["node"]][["createdAt"]] }))
  parsed_response[["category_name"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                               function(edge) { edge[["node"]][["category"]][["name"]] }))
  parsed_response[["answer_id"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                           function(edge) { edge[["node"]][["answer"]][["id"]] %||% NA}))
  parsed_response[["body"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                      function(edge) { edge[["node"]][["bodyText"]] }))

  parsed_response <- as.data.table(parsed_response)

  return(parsed_response)
}

#' Parse Discussions JSON to Table
#'
#' @description This function parses through the JSON of Github Discussions
#' downloaded from the `github_api_discussions` function, and turns it into
#' a table. This function only parses through the Discussion comments within
#' the JSON, and does not parse the discussion themselves.
#'
#' @param api_response API response obtained from `github_api_discussions` function.
#' @return The parsed table of Discussion Comments.
#' @export
github_parse_discussion_comments <- function(api_response) {
  parsed_response <- list()
  parsed_response[["parent_discussion_id"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                                       function(edge) { lapply(edge[["node"]][["comments"]][["edges"]],
                                                                               function(edge) {edge[["node"]][["discussion"]][["id"]]}) }))
  parsed_response[["comment_id"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                                       function(edge) { lapply(edge[["node"]][["comments"]][["edges"]],
                                                                               function(edge) {edge[["node"]][["id"]]}) }))
  parsed_response[["comments_author_login"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                                       function(edge) { lapply(edge[["node"]][["comments"]][["edges"]],
                                                                               function(edge) {edge[["node"]][["author"]][["login"]]}) }))
  parsed_response[["comments_created_at"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                                     function(edge) { lapply(edge[["node"]][["comments"]][["edges"]],
                                                                             function(edge) {edge[["node"]][["createdAt"]]}) }))
  parsed_response[["comments_body"]] <- unlist(lapply(api_response[["data"]][["repository"]][["discussions"]][["edges"]],
                                               function(edge) { lapply(edge[["node"]][["comments"]][["edges"]],
                                                                       function(edge) {edge[["node"]][["bodyText"]]}) }))
  parsed_response <- as.data.table(parsed_response)

  return(parsed_response)
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
#' @keywords internal
github_api_page_next <- function(gh_response){
  gh::gh_next(gh_response)
}

#' Obtain the previous GitHub response page.
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
#' @keywords internal
github_api_page_prev <- function(gh_response){
  gh::gh_prev(gh_response)
}

#' Obtain the first GitHub response page.
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @export
#' @keywords internal
github_api_page_first <- function(gh_response){
  gh::gh_first(gh_response)
}


#' Obtain the last GitHub response page.
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
#' available requests in the used user's token).
#'
#' @param token Your GitHub API token
#' @param gh_response A response returned by any GitHub endpoint which is paginated (e.g. \code{\link{github_api_project_commits}}).
#' @param save_folder_path A folder path to save the downloaded json pages "as-is".
#' @param prefix Prefix to be added to every json file name
#' @param max_pages The maximum number of pages to download. MAX = Available token requests left
#' @references For details see \url{https://docs.github.com/en/free-pro-team@latest/rest/guides/traversing-with-pagination}.
#' @export
#' @keywords internal
github_api_iterate_pages <- function(token,gh_response,save_folder_path,prefix=NA,max_pages=NA){
  page_number <- 1

  if(is.na(max_pages)){
    max_pages <- github_api_rate_limit(token)$remaining
  }

  while(!is.null(gh_response) & page_number < max_pages){

    write_json(gh_response,paste0(save_folder_path,
                                  owner, "_", repo, "_", prefix, "_", "p_", page_number,
                                  ".json"),
               pretty=TRUE, auto_unbox=TRUE)

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
