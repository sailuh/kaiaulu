# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @export
normalized_levenshtein <- function(a,b){
  return(drop(1 - adist(a,b)/max(nchar(a),nchar(b))))
}
#' @export
format_name_email <- function(name_email){
  # Remove < or > if any
  name_email <- stri_replace_all_regex(name_email,pattern="<|>",replace="")
  # Fix at, AT => @
  at_regex <- "at|AT| at | AT "
  name_email <- stri_replace_last_regex(name_email,pattern=at_regex,replace="@")
  return(name_email)
}
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
    is_email <- drop(!is.na(stri_match(name_email,regex = "@")))
    if(is_email){
      name_email_split[["email"]] <- name_email
    }else{
      name_email_split[["name"]] <- name_email
    }
  }
  return(name_email_split)
}
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


# Various imports
#' @importFrom stringi stri_replace_last
#' @importFrom stringi stri_replace_first
#' @importFrom stringi stri_c
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
