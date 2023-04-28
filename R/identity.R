# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Edit Distance of two words
#' @param a a string
#' @param b a string
#' @export
#' @importFrom utils adist
#' @keywords internal
normalized_levenshtein <- function(a,b){
  return(drop(1 - adist(a,b)/max(nchar(a),nchar(b))))
}
#' Format Name and Email
#' @param name_email A string containing name and email
#' @export
format_name_email <- function(name_email){
  # Remove < or > if any
  name_email <- stri_replace_all_regex(name_email,
                                       pattern='<|>|\\(|\\)|\\"|,| via RT'
                                       ,replacement="")

  # Fix at only if @ doesnt exist. Matt != M@t (See unit test)
  email_symbol_exist <- any(!is.na(stri_match_all(name_email,regex = "@")))
  # The @ is hiding somewhere. Find it!
  if(!email_symbol_exist){
    # Find the at!
    at_regex <- "-at-| at | AT | At | aT |at|AT|At|aT"
    name_email <- stri_replace_last_regex(name_email,pattern=at_regex,replacement="@")
  }else{
    # Fix spaced at, AT => @
    at_regex <- "-at-|AT| at | AT "
    name_email <- stri_replace_last_regex(name_email,pattern=at_regex,replacement="@")
  }
  return(name_email)
}
#' Split Name and Email
#' @param name_email A formatted name and email.
#' @export
split_name_email <- function(name_email){ # rename parameter to format_name_email
  name_email_split <- list(name = NA, email = NA, id = NA)
  words <- stri_split_regex(name_email,pattern=" ")[[1]]
  n_words <- length(words)
  # Which string is name, and which string is email?
  email_index <- which(!is.na(stri_match_first(words,regex = "@")))
  contains_email <- length(email_index) == 1
  # If there is a name AND email
  if(n_words > 1 & contains_email){
    name_email_split[["email"]] <- words[email_index]
    name_email_split[["name"]] <- stri_c(words[!(1:n_words %in% email_index)],
                                         collapse=" ")
    # Only name or email exist -- which?
  }else if(n_words <= 1 & contains_email){
    name_email_split[["email"]] <- words[email_index]
  }else{
    name_email_split[["name"]] <- stri_c(words,collapse=" ")
  }
  return(name_email_split)
}
#' Compares identities in a list by index
#' @param i Index of the first identity
#' @param j Index of the second identity
#' @param parsed_name_email The list of name and email identities
#' @param use_name_only Uses only the name field (i.e. disregard the e-mail field)
#' @export
is_same_identity <- function(i,j,parsed_name_email,use_name_only=FALSE){
  # R does not stop on first false condition on AND like C.
  if(j == 0){
    return(FALSE)
  }
  # name comparison
  is_name_match <- stri_cmp_eq(parsed_name_email[[i]]$name,
                               parsed_name_email[[j]]$name)
  # name is missing to compare
  is_name_match <- ifelse(is.na(is_name_match),FALSE,is_name_match)

  # reverse name comparison
  name <- stri_split_regex(parsed_name_email[[i]]$name," ")[[1]]
  reverse_name <- stri_c(name[length(name):1],collapse=" ")
  is_reverse_name_match <- stri_cmp_eq(reverse_name,
                                       parsed_name_email[[j]]$name)
  # reverse name is missing
  is_reverse_name_match <- ifelse(is.na(is_reverse_name_match),
                                  FALSE,
                                  is_reverse_name_match)

  # email comparison
  is_email_match <- stri_cmp_eq(parsed_name_email[[i]]$email,
                                parsed_name_email[[j]]$email)
  # email is missing to compare
  is_email_match <- ifelse(is.na(is_email_match),FALSE,is_email_match)

  if(use_name_only){
    return(is_name_match)
  }else{
    return(is_name_match | is_reverse_name_match | is_email_match)
  }
}

#' Identify authors with different names and emails
#'
#' @param unique_name_email A single string containing name and email as used in git and mailinglists.
#' @param use_name_only Uses only the name field (i.e. disregard the e-mail field)
#' @export
assign_exact_identity <- function(unique_name_email,use_name_only=FALSE){
  formatted_name_email <- format_name_email(unique_name_email)
  A <- lapply(formatted_name_email,split_name_email)
  id <- 1
  i <- 1
  while( i <= length(A)){
    j <- i
    while(j >= 2 & !is_same_identity(i,j-1,A,use_name_only)){
      j <- j - 1
    }
    # When j == 2, we tested A[i] and A[j-1]
    # Hence, no id is found or j would be 2
    # No existing id found, assign new id
    if(j == 1){
      A[[i]]$id <- id
      id <- id + 1
      # a match was found
    }else{
      A[[i]]$id <- A[[j-1]]$id
    }
    i <- i + 1
  }
  ids <- sapply(A,"[[",3)
  return(ids)
}
#' Assign Common Id to Nodes and Edgelists to Git and Mbox
#' @param project_log A list which can be any number of tables from \code{\link{parse_gitlog}}
#' or \code{\link{parse_mbox}}
#' @param name_column A string or vector of strings containing the column names
#' which identity match should apply. One column name should provided for each
#' table in the `project_log` list.
#' @param assign_identity_function The heuristic function which decides common IDs
#' (currently only available: \code{\link{assign_exact_identity}})
#' @param use_name_only Uses only the name field (i.e. disregard the e-mail field)
#' @param label Whether to replace the original non matched name with the
#' collection of all names (label == raw_name), or an id (label == identity_id)
#' @return Returns `project_log`, with two added columns, `raw_name` and
#' `identity_id`. `raw_name` contains all names matched to the user, while
#' `identity_id` provides a unique identifier, starting at 1, for all names
#' passed in `project_log` across all tables of the list.
#' @export
identity_match <- function(project_log,name_column,assign_identity_function,
                           use_name_only=FALSE,
                           label){
  all_name_emails <- c()
  for(i in 1:length(project_log)){
    all_name_emails <- c(all_name_emails,project_log[[i]][[name_column[i]]])
  }
  all_name_emails <- unique(all_name_emails)

  name_mapping <- data.table(raw_name=all_name_emails,
                             identity_id=assign_identity_function(all_name_emails,
                                                                  use_name_only))
  name_mapping_collapsed <- name_mapping[,.(raw_name = stri_c(raw_name,collapse = " | ")),
                                         by=identity_id]

  for(i in 1:length(project_log)){

    project_log[[i]] <- merge(project_log[[i]],
                              name_mapping,by.x=name_column[i],by.y="raw_name")


    project_log[[i]] <- merge(project_log[[i]],
                              name_mapping_collapsed,
                              by.x="identity_id",
                              by.y="identity_id",all.x = TRUE)
    if(label == "raw_name"){
      project_log[[i]][[name_column[i]]] <- project_log[[i]][["raw_name"]]
    }else if (label == "identity_id"){
      project_log[[i]][[name_column[i]]] <- project_log[[i]][["identity_id"]]
    }else{
      stop(stri_c("Unknown label: ",label))
    }
  }
  return(project_log)
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
