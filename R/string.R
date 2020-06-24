# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Edit Distance of two words
#' @param a a string
#' @param b a string
#' @export
#' @importFrom utils adist
normalized_levenshtein <- function(a,b){
  return(drop(1 - adist(a,b)/max(nchar(a),nchar(b))))
}
#' Format Name and Email
#' @param name_email A string containing name and email
#' @export
format_name_email <- function(name_email){
  # Remove < or > if any
  name_email <- stri_replace_all_regex(name_email,pattern="<|>",replacement="")
  # Fix at, AT => @
  at_regex <- "at|AT| at | AT "
  name_email <- stri_replace_last_regex(name_email,pattern=at_regex,replacement="@")
  return(name_email)
}
#' Split Name and Email
#' @param name_email A formatted name and email.
#' @export
split_name_email <- function(name_email){ # rename parameter to format_name_email
  name_email_split <- list()
  name_email_split[["name"]] <- NA
  name_email_split[["email"]] <- NA
  name_email_split[["id"]] <- NA

  name_email <- stri_split_regex(name_email,pattern=" ")[[1]]
  # If there is a name AND email (assumes name occurs before email)
  if(length(name_email)  > 1){
    name_email_split[["name"]] <- stri_c(name_email[1:(length(name_email)-1)],collapse=" ")
    name_email_split[["email"]] <- name_email[length(name_email)]
    # Only name or email exist -- which?
  }else if(length(name_email) == 1){
    is_email <- drop(!is.na(stri_match_first(name_email,regex = "@")))
    if(is_email){
      name_email_split[["email"]] <- name_email
    }else{
      name_email_split[["name"]] <- name_email
    }
  }
  return(name_email_split)
}
#' Compares identities in a list by index
#' @param i Index of the first identity
#' @param j Index of the second identity
#' @param parsed_name_email The list of name and email identities
#' @export
is_same_identity <- function(i,j,parsed_name_email){
  # R does not stop on first false condition on AND like C.
  if(i == 0){
    return(FALSE)
  }
  is_name_match <- stri_cmp_eq(parsed_name_email[[i]]$name,
                               parsed_name_email[[j]]$name)
  # name is missing to compare
  is_name_match <- ifelse(is.na(is_name_match),FALSE,is_name_match)

  is_email_match <- stri_cmp_eq(parsed_name_email[[i]]$email,
                                parsed_name_email[[j]]$email)
  # name is missing to comppare
  is_email_match <- ifelse(is.na(is_email_match),FALSE,is_email_match)
  return(is_name_match | is_email_match)
}

#' Identify authors with different names and emails
#'
#' @param unique_name_email A single string containing name and email as used in git and mailinglists.
#' @export
assign_exact_identity <- function(unique_name_email){
  formatted_name_email <- format_name_email(unique_name_email)
  A <- lapply(unique_name_email,split_name_email)
  id <- 1
  i <- 1
  while( i <= length(A)){
    j <- i
    while(j > 1 & !is_same_identity(j-1,i,A)){
      j <- j - 1
    }
    # No existing id found, assign new id
    if(j == 1){
      A[[i]]$id <- id
      id <- id + 1
    # j > 1 and a match was found
    }else{
      A[[i]]$id <- A[[j-1]]$id
    }
    i <- i + 1
  }
  ids <- sapply(A,"[[",3)
  return(ids)
}
#' Assign Common Id to Nodes and Edgelists to Git and Mbox
#' @param project_git the parsed mbox file from \code{\link{parse_gitlog}}
#' @param project_mbox the parsed mbox file from \code{\link{parse_mbox}}
#' @param assign_identity_function The heuristic function which decides common IDs
#' (currently only available: \code{\link{assign_network_identity}})
#' @export
assign_network_identity <- function(project_git,project_mbox,assign_identity_function){
  # git_author_network type == TRUE if node is author, else is file
  git_authors <- project_git[["nodes"]]$name[project_git[["nodes"]]$type]
  git_files <- project_git[["nodes"]]$name[!project_git[["nodes"]]$type]

  # mbox_network type == TRUE if node is author, else is thread
  mbox_authors <- project_mbox[["nodes"]]$name[project_mbox[["nodes"]]$type]
  mbox_threads <- project_mbox[["nodes"]]$name[!project_mbox[["nodes"]]$type]

  all_name_emails <- unique(c(git_authors,mbox_authors))
  name_mapping <- data.table(raw_name=all_name_emails,id=assign_identity_function(all_name_emails))

  # Map the ids to git_authors vertices from the graph
  author_v_ids <- merge(data.table(raw_name=git_authors),
                        name_mapping,
                        by="raw_name",all.x=TRUE,sort=FALSE)$id
  #Suplement the remaining node ids, i.e. files, with NA
  project_git[["nodes"]]$id <- c(author_v_ids,git_files)

  # Similarly, map the ids to the ids of mbox_authors
  mbox_v_ids <- merge(data.table(raw_name=mbox_authors),
                      name_mapping,
                      by="raw_name",all.x=TRUE,sort=FALSE)$id
  #Suplement the remaining node ids, i.e. files, with NA
  project_mbox[["nodes"]]$id <- c(mbox_v_ids,mbox_threads)

  # Swap name attribute and id attribute to simplify usage by igraph
  # Node
  project_git[["nodes"]]$raw_name <- project_git[["nodes"]]$name
  project_mbox[["nodes"]]$raw_name <- project_mbox[["nodes"]]$name
  project_git[["nodes"]]$name <- project_git[["nodes"]]$id
  project_mbox[["nodes"]]$name <- project_mbox[["nodes"]]$id
  # Edgelist
  name_id_mapping <- project_git[["nodes"]]$id
  names(name_id_mapping) <- project_git[["nodes"]]$raw_name
  project_git[["edgelist"]]$author <- name_id_mapping[project_git[["edgelist"]]$author]

  name_id_mapping <- project_mbox[["nodes"]]$id
  names(name_id_mapping) <- project_mbox[["nodes"]]$raw_name
  project_mbox[["edgelist"]]$author <- name_id_mapping[project_mbox[["edgelist"]]$author]

  # Node list must not occur twice, collapse raw_names
  project_git[["nodes"]] <- project_git[["nodes"]][,.(color=color[1],
                                                      type=type[1],
                                                      id=id[1],
                                                      raw_name=stri_c(raw_name,
                                                                      collapse=" | "))
                                                   ,by="name"]
  project_mbox[["nodes"]] <- project_mbox[["nodes"]][,.(color=color[1],
                                                        type=type[1],
                                                        id=id[1],
                                                        raw_name=stri_c(raw_name,
                                                                        collapse=" | "))
                                                     ,by="name"]
  # Edgelists of previous 2 different authors may now be the same, sum weights up.
  project_git[["edgelist"]] <- project_git[["edgelist"]][,.(weight=sum(weight)),
                                                         by=c("author","file")]
  project_mbox[["edgelist"]] <- project_mbox[["edgelist"]][,.(weight=sum(weight)),
                                                           by=c("author","thread")]

  projects <- list()
  projects[["project_git"]] <- project_git
  projects[["project_mbox"]] <- project_mbox

  return(projects)
}


# Various imports
#' @importFrom stringi stri_replace_last
#' @importFrom stringi stri_replace_first
#' @importFrom stringi stri_c
#' @importFrom stringi stri_cmp_eq
#' @importFrom stringi stri_replace_last_regex
#' @importFrom stringi stri_replace_all_regex
#' @importFrom stringi stri_replace_all_regex
#' @importFrom stringi stri_split_regex
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
NULL
