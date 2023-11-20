# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.




#' Create Mbox Reply String
#'
#' Used to create a single e-mail reply string in .mbox format.
#' See \code{link{create_mbox_from_replies}} to save it to disk.
#'
#'
#' @param mailing_list specific mailing list associated with this mbox data
#' @param reply_from_author name of sender (e.g. Firstname Lastname)
#' @param reply_from_email email of sender. (e.g. sender@domain.com --- will be formatted as <sender@domain.com>).
#' @param reply_to_author name of recipient (e.g. First Name Lastname)
#' @param reply_to_email email of recipient. (e.g. recipient@domain.com --- will be formatted as <recipient@domain.com>).
#' @param reply_cc_author name of one additional Cc'ed recipient (e.g. Firstname Lastname)
#' @param reply_cc_email email of one additional Cc'ed recipient (e.g. recipient2@domain.com --- will be formatted as <recipient2@domain.com>).
#' @param reply_datetime Date and time in the following format: 2023-02-11T09:30:00  (Be sure to add the capital T to separate date and time).
#' @param timezone  The abbreviation of desired timezone (e.g: HST). For more information on date and timezone refer to POSIX doc. Enter: '?POSIXct' in console
#' @param reply_subject Subject of the email as a string
#' @param reply_body Body of the email as a string
#' @return the content of the fake mbox you created returned as a string in mbox format
#' @references For details, see \url{https://en.wikipedia.org/wiki/Email#Header_fields}.
#' @export
make_mbox_reply <- function(mailing_list, reply_from_author, reply_from_email, reply_to_author, reply_to_email, reply_cc_author, reply_cc_email, reply_datetime, timezone, reply_subject, reply_body) {

# format the date correctly
  cdate <- format(as.POSIXct(reply_datetime, format = "%Y-%m-%dT%H:%M:%S"), "%a, %e %b %Y %H:%M:%S ")

 reply_from_full_info <- paste0(reply_from_author, " <", reply_from_email, ">")
 reply_to_full_info <- paste0(reply_to_author, " <", reply_to_email, ">")
 reply_cc_full_info <- paste0(reply_cc_author, " <", reply_cc_email, ">")


  mbox_content <- paste0(
    "From MAILER-DAEMON Thu Jul 18 13:48:48 2013",
    "\nPath: example.com!not-for-mail",
    "\nFrom: ", reply_from_full_info,
    "\nNewsgroups: gmane. ", mailing_list,
    "\nSubject: ", reply_subject,
    "\nDate: ", cdate, timezone,
    "\nApproved: auto",
    "\nMessage-ID: <",as.character(sample(1:100,1)),"@domain.org>",
    "\nNNTP-Posting-Host: example.com",
    "\nMime-Version: 1.0",
    "\nContent-Type: text/plain; charset=us-ascii; format=flowed",
    "\nContent-Transfer-Encoding: 7bit",
    "\nX-Complaints-To: complaints@example.com",
    "\nNNTP-Posting-Date: ", cdate, timezone,
    "\nUser-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.8) Gecko/20020205",
    "\nX-Accept-Language: en-us ",
    "\nOriginal-To: ", reply_to_full_info, " ", reply_cc_full_info,
    "\nPrecedence: bulk",
    "\nX-Mailing-List: ", paste0(mailing_list, "@example.com"),
    "\n\n", reply_body
  )

  return(mbox_content)
}


#' Takes in mbox replies and creates a .mbox file
#'
#' Takes a list of mbox replies generated with create_fake_mbox_replies function and compiles them all into a single
#' fake .mbox file
#'
#' @param replies An array of replies that have been created with \code{\link{make_mbox_reply}}
#' @param folder_path  Folder path for the .mbox file being created. Defaulted at /tmp
#' @param file_name Name of the file that will store the .mbox file
#' @return the path of the .mbox file that was created
#' @export
make_mbox_mailing_list <- function(replies, folder_path = "/tmp", file_name) {

  # Create a unique filename for the mbox file
  mbox_filepath <- file.path(folder_path, paste0(file_name, ".mbox"))

 # make the file
  mbox_body <- stringi::stri_c(replies,collapse = "\n\n")
  io_make_file(mbox_filepath,mbox_body)

  # Return the path of the created mbox file
  return(mbox_filepath)
}

