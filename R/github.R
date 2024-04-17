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
#' available requests in the used user's token). This function can differentiate between
#' data downloaded from search endpoint or not, in which the issues are differently nested.
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
github_api_iterate_pages <- function(token,gh_response,save_folder_path,prefix=NA,max_pages=NA,verbose){
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


#' Download Project Issues after a date
#'
#' Returns issue data that has not already been downloaded
#' Gets the name of the file with the most recent data along the designated save paths for both
#' issue and refresh_issue folder. Extracts the greatest 'created_at' value for both of them.
#' It compares these values and calls the search endpoint to retrieve all issues created after this date.
#' #' If no files exist in the issue file, \code{link{github_api_project_issue}} is called instead
#' and all issues are downloaded.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @param save_path_issue The folder path that the original issue downloader downloads to
#' @param save_path_issue_refresh The folder path that the refresh downloader downloads to
#' @param verbose A boolean value that prints operational messages when set to TRUE.
#' These may include announcing successful execution of code, API queries, files saved, etc.
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/issues#list-repository-issues}.
#' @seealso  \code{link{github_api_project_issue}} to download all issue data
#' @seealso  \code{link{format_created_at_from_file}} for function that iterates through
#' a .json file and returns the greatest 'created_at' value
#' @seealso  \code{link{github_api_iterate_pages}} to write data returned by this function to file as .json
github_api_project_issue_refresh <- function(owner,
                                             repo,
                                             token,
                                             save_path_issue,
                                             save_path_issue_refresh,
                                             verbose){

  # Check if issue folder is empty
  contents <- list.files(path = save_path_issue)
  # Check if refresh folder is empty
  contents_refresh <- list.files(path = save_path_issue_refresh)

  # If the file is empty, download all issues
  if(length(contents) == 0) {
    # Run regular downloader
    gh_response <- github_api_project_issue(owner,repo,token)
    github_api_iterate_pages(token,gh_response,
                             save_path_issue,
                             prefix="issue",
                             verbose=TRUE)
  } else {

  # Get the name of the file with the most recent date from the issue file
  latest_date_issue <- paste0(save_path_issue, parse_jira_latest_date(save_path_issue))
  # Get the name of the file with the most recent date from the refresh_issue file if not empty
    if (length(contents_refresh) != 0){
      latest_date_issue_refresh <- paste0(save_path_issue_refresh, parse_jira_latest_date(save_path_issue_refresh))
    }

    # get the greatest created_at value among issues in the issues file
  created <- format_created_at_from_file(latest_date_issue, item="")
  message("Greatest created value from issue folder: ", created)

  if (length(contents_refresh) != 0){
     # get the greatest created_at value among issues in the refresh_issues file
    created_refresh <- format_created_at_from_file(latest_date_issue_refresh, item="items")
  }
  if(verbose){
    message("Greatest created value from issue folder: ", created)
    if (length(contents_refresh) != 0){
        message("Greatest created value from refresh_issue folder: ", created_refresh)
    }
  }

  # Get the greatest value of the created_at from either folder
  if (length(contents_refresh) != 0){
    if(created>created_refresh){
      greatest_created <- created
    } else {
      greatest_created <- created_refresh
    }
  } else {
    greatest_created <- created
  }

  # API Call
  query <- sprintf("repo:%s/%s is:issue created:>%s", owner, repo, greatest_created)
  # query <- sprintf("repo:%s/%s is:issue", owner, repo)
  if (verbose){
    message(query)
  }
  # Use the Search API endpoint to search for issues
  gh_response <- gh::gh("/search/issues",
                   q = query,
                   state = 'all',
                   page = 1,
                   per_page = 100,
                   .token = token)
                    # Adjust .limit as needed, though GitHub API has its own paging mechanisms
  return(gh_response)
  }
}

#' Download Project issues or pr comments after certain date
#'
#' Returns issue and pull request comements that has not already been downloaded
#' Gets the name of the file with the most recent date along the designated save path.
#' Extracts the greatest 'created_at' date from that file
#' Calls issues/comments endpoint to download comments created after that date
#' If no files exist in the file, \code{link{github_api_project_issue_or_pr_comments}} is called instead.
#'
#' @param owner GitHub's repository owner (e.g. sailuh)
#' @param repo GitHub's repository name (e.g. kaiaulu)
#' @param token Your GitHub API token
#' @export
#' @references For details, see \url{https://docs.github.com/en/rest/reference/issues#list-repository-issues}.
#' @seealso  \code{link{github_api_project_issue_or_pr_comments}} to download all comment data
#' @seealso  \code{link{format_created_at_from_file}} for function that iterates through
#' a .json file and returns the greatest 'created_at' value
#' @seealso  \code{link{github_api_iterate_pages}} to write data returned by this function to file as .json
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
  latest_date_issue_or_pr_comment <- paste0(file_save_path, parse_jira_latest_date(save_path_issue_or_pr_comments))
  # get the created_at value
  message("got file", latest_date_issue_or_pr_comment)
  created <- format_created_at_from_file(latest_date_issue_or_pr_comment, item="")
  if(verbose){
    message("file name with greatest date: ",latest_date_issue_or_pr_comment)
    message("Latest date: ",created)
  }
  # Github API Call
  gh::gh("GET /repos/{owner}/{repo}/issues/comments",
         owner=owner,
         repo=repo,
         since=created,  # Pass the `since` parameter in the API request
         page=1,
         per_page=100,
         .token=token)
    } #end if/else
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
#' @export
#' @seealso  \code{link{github_api_project_issue_or_pr_comment_refresh}} to refresh comment data
#' @seealso  \code{link{github_api_project_issue_refresh}} to refresh issue data
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
