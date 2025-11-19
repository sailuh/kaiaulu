# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Remove punctuation and symbols from text
#'
#' Cleans a character vector by removing all Unicode punctuation and symbol characters.
#'
#' @param text_vector A character vector to be cleaned.
#' @return A character vector with all punctuation and symbols removed.
#' @export
filter_punctuation_from_text <- function(text_vector) {
  if (!is.character(text_vector)) stop("Input must be a character vector")
  stringi::stri_replace_all_regex(text_vector, "\\p{P}|\\p{S}", "")
}

#' Clean GitHub-style email replies
#'
#' Removes quoted GitHub notifications, lines starting with '>', and fenced code blocks.
#' Trims leading/trailing whitespace from each element of a character vector.
#'
#' @param text_vector A character vector containing text to clean.
#' @return A character vector with quoted text, code blocks, and extra whitespace removed.
#' @export
clean_text <- function(text_vector) {
  if (!is.character(text_vector)) stop("Input must be a character vector")
  
  # Remove GitHub-style email headers + quoted blocks
  text_vector <- stringi::stri_replace_all_regex(
    text_vector,
    "^(On[\\s\\S]*?notifications@github\\.com\\s*?wrote:\\s*?)?(^(>).*\\s)*",
    ""
  )
  
  # Remove ANY lines starting with ">"
  text_vector <- stringi::stri_replace_all_regex(
    text_vector,
    "(?m)^>.*$",
    ""
  )
  
  # Remove fenced code blocks
  text_vector <- stringi::stri_replace_all_regex(
    text_vector,
    "```[a-zA-Z0-9]*\\n?[\\s\\S]*?\\n?```",
    ""
  )
  
  # Trim whitespace
  text_vector <- stringi::stri_trim_both(text_vector)
  
  return(text_vector)
}

#' Remove Markdown formatting from text
#'
#' Converts Markdown text to plain text by first converting to HTML and then extracting all text nodes.
#'
#' @param text_vector A character vector containing Markdown-formatted text.
#' @return A character vector with Markdown formatting removed.
#' @export
filter_markdown_from_text <- function(text_vector) {
  if (!is.character(text_vector)) stop("Input must be a character vector")
  
  sapply(text_vector, function(text) {
    html_content <- markdownToHTML(text = text, fragment.only = TRUE)
    xml_doc <- htmlParse(html_content, asText = TRUE)
    text_nodes <- xpathSApply(xml_doc, "//text()", xmlValue)
    paste(text_nodes, collapse = "")
  }, USE.NAMES = FALSE)
}

#' Add Comment Classification and Uniquify Comment IDs (creates column)
#'
#' Adds a 'if_comment' column to the data frame to indicate whether a row is a reply to
#' a subject or a new subject. If it is a comment, the `reply_id` is modified to ensure uniqueness.
#'
#' @param df A data frame containing columns 'reply_id' and 'reply_subject'.
#' @return a data  frame with a new 'if_comment' column (TRUE if it is a comment, otherwise false).
#' possibly modified 'reply_id' values for rows where 'if_comment' is TRUE.
#' @export
create_if_comment_column <- function (df) {
  if (!("reply_id" %in% colnames (df) && "reply_subject" %in% colnames (df
      ))) {
    stop ("The data frame does not contain reply_id or reply_subject columns ")
}
df$if_comment <- df$reply_id != df$reply_subject

df$reply_id <- ifelse (
  df$if_comment ,
  paste0(df$reply_id, "_unique_", seq_along(df$reply_id)),
  df$reply_id
)
  return (df)
}


#' Process Mbox Data
#'
#' Cleans and standardizes mbox email reply subjects, handling encoding
#' issues and removing common prefixes like "Re:".
#'
#' @param df A data.table containing mbox email data. Must include a 'reply_subject' column.
#' @return A cleaned and processed data.table with normalized reply subjects and generated reply IDs.
#' @export
process_mbox_data <- function(df) {
  if (!"reply_subject" %in% colnames(df)) {
    stop("The data frame does not contain a reply_subject column.")
  }

  df$reply_subject <- iconv(df$reply_subject, from = "UTF-8", to = "UTF-8", sub = "byte")

  df$reply_id <- ifelse(
    grepl("^[Rr][Ee]:", df$reply_subject),
    df$reply_subject,
    df$reply_subject
  )

  df$reply_subject <- ifelse(
    grepl("^[Rr][Ee]:", df$reply_subject),
    sub("^[Rr][Ee]:\\s*", "", df$reply_subject),
    df$reply_subject
  )

  return(df)
}

#' Clean Mbox Data
#'
#' Removes automated or irrelevant entries from mbox email data such as system-generated messages,
#' Jira or GitHub notifications, and optional vote-related messages.
#'
#' @param df A data.table containing mbox email data. Must include 'reply_subject' and 'reply_from' columns.
#' @param if_clean_vote Logical indicating whether to remove vote-related messages (default is TRUE).
#' @return A cleaned data.table with unwanted messages removed.
#' @export
clean_mbox_data <- function(df, if_clean_vote = TRUE) {
  if (!("reply_subject" %in% colnames(df))) {
    stop("The data frame does not contain a 'reply_subject' column.")
  }
  if (!("reply_from" %in% colnames(df))) {
    stop("The data frame does not contain a 'reply_from' column.")
  }

  cleaned_df <- df[!grepl("^\\[GitHub\\]|^\\[jira\\]", df$reply_subject, ignore.case = TRUE), ]
  cleaned_df <- cleaned_df[!grepl("HELIX-", cleaned_df$reply_subject), ]
  cleaned_df <- cleaned_df[!grepl("Review Request", cleaned_df$reply_subject, ignore.case = TRUE), ]
  cleaned_df <- cleaned_df[!grepl("no-reply@apache.org|jenkins@builds.apache.org", cleaned_df$reply_from), ]

  if (if_clean_vote) {
    cleaned_df <- cleaned_df[!grepl("\\[VOTE\\]", cleaned_df$reply_subject, ignore.case = TRUE), ]
  }

  return(cleaned_df)
}

#' Clean Mbox Comment Data
#'
#' Removes duplicate messages and invalid comments that do not reference any known subject message.
#'
#' @param df A data.table containing mbox comment data. Must include 'if_comment', 'reply_subject', and 'reply_id' columns.
#' @return A cleaned data.table with only valid subject and comment rows retained
#' @export
clean_mbox_comment <- function(df) {
  if (!("if_comment" %in% colnames(df))) {
    stop("The data frame does not contain an if_comment column")
  }

  if (!("reply_subject" %in% colnames(df))) {
    stop("The data frame does not contain a reply_subject column")
  }

  if (!("reply_id" %in% colnames(df))) {
    stop("The data frame does not contain a reply_id column")
  }

  initial_row_count <- nrow(df)
  df <- df[!duplicated(df$reply_id), ]
  duplicate_rows_removed <- initial_row_count - nrow(df)

  subject_df <- df[df$if_comment == FALSE, ]
  comment_df <- df[df$if_comment == TRUE, ]

  initial_comment_row_count <- nrow(comment_df)
  valid_reply_ids <- subject_df$reply_id
  comment_df <- comment_df[comment_df$reply_subject %in% valid_reply_ids, ]
  comment_rows_removed <- initial_comment_row_count - nrow(comment_df)

  cleaned_df <- rbind(subject_df, comment_df)

  cat("Removed", duplicate_rows_removed, "duplicate rows\n")
  cat("Removed", comment_rows_removed, "invalid comment rows\n")

  return(cleaned_df)
}


#' Clean JIRA Data
#'
#' Removes bot-generated messages and 'Hudson' from the JIRA dataset.
#'
#' @param df A data.table containing JIRA data. Must include a 'reply_from' column.
#' @return A cleaned data.table with only relevant human-generated messages.
#' @export
clean_jira_data <- function(df) {
  if (!("reply_from" %in% colnames(df))) {
    stop ("The data frame does not contain a reply _ from column ")
  }

  cleaned_df <- df[!grepl("bot", df$reply_from, ignore.case = TRUE), ]
  cleaned_df <- cleaned_df[!grepl("Hudson", cleaned_df$reply_from, ignore.case = TRUE), ]

  return(cleaned_df)
}
