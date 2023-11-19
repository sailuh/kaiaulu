# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.




#' Creates fake mbox reply data
#'
#' Used to create a fake mbox replies with the specified parameters and returns the output to the console. Can be used
#' in conjunction with create_mbox_from_replies function to create a .mbox file to create a full fake .mbox file with
#' multiple of these fake replies
#'
#' @param mlist specific mailing list associated with this mbox data
#' @param reply_from_author name of sender. Formatted as: FIRSTNAME LASTNAME
#' @param reply_from_email email of sender, automatically encapsulated in <> eg: sender@domain.com
#' @param reply_to_author name of receiver. Formatted as: FIRSTNAME LASTNAME
#' @param reply_to_email email of receiver, automatically encapsulated in <>  eg: receiver@domain.com
#' @param reply_cc_author name of carbon copy of email to additional recipients. Formatted as: FIRSTNAME LASTNAME
#' @param reply_cc_email email of carbon copy recipients, automatically encapsulated in <> input eg: recipientCC@domain.org
#' @param reply_datetime Date, time in the following format: 2023-02-11T09:30:00  Be sure to add the capital T to separate date and time
#' @param timezone  The abbreviation of desired timezone eg: HST
#' @param reply_subject subject of the email as a string
#' @param reply_body the body of the email as a string
#' @return the content of the fake mbox you created
#' @export
create_fake_mbox_replies <- function(mlist, reply_from_author, reply_from_email, reply_to_author, reply_to_email, reply_cc_author, reply_cc_email, reply_datetime, timezone, reply_subject, reply_body) {

# format the date correctly
  cdate <- format(as.POSIXct(reply_datetime, format = "%Y-%m-%dT%H:%M:%S"), "%a, %e %b %Y %H:%M:%S ")

 reply_from_full_info <- paste0(reply_from_author, " <", reply_from_email, "> ")
 reply_to_full_info <- paste0(reply_to_author, " <", reply_to_email, "> ")
 reply_cc_full_info <- paste0(reply_cc_author, " <", reply_cc_email, "> ")


  mbox_content <- paste0(
    "From MAILER-DAEMON Thu Jul 18 13:48:48 2013 ",
    "\nPath: example.com!not-for-mail ",
    "\nFrom: ", reply_from_full_info,
    "\nNewsgroups: gmane. ", mlist,
    "\nSubject: ", reply_subject,
    "\nDate: ", cdate, timezone,
    "\nApproved: auto ",
    "\nMessage-ID: <", length(reply_body),
    "\nNNTP-Posting-Host: example.com",
    "\nMime-Version: 1.0 ",
    "\nContent-Type: text/plain; charset=us-ascii; format=flowed ",
    "\nContent-Transfer-Encoding: 7bit ",
    "\nX-Complaints-To: complaints@example.com ",
    "\nNNTP-Posting-Date: ", cdate, timezone,
    "\nUser-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.8) Gecko/20020205 ",
    "\nX-Accept-Language: en-us ",
    "\nOriginal-To: ", reply_to_full_info, " ", reply_cc_full_info,
    "\nPrecedence: bulk",
    "\nX-Mailing-List: ", paste0(mlist, "@example.com "),
    "\n\n", reply_body
  )

  return(mbox_content)
}


#' Takes in mbox replies and creates a .mbox file
#'
#' Takes a list of mbox replies generated with create_fake_mbox_replies function and compiles them all into a single
#' fake .mbox file
#'
#' @param folder_path  Folder path for the .mbox file being created. Defaulted at /tmp
#' @param folder_name Name of the folder that will store the .mbox file
#' @param replies A list of replies that have been created with create_fake_mbox_replies() function that will be added to .mbox file
#' @return the path of the .mbox file that was created, now that each reply has been added
#' @export
create_mbox_from_replies <- function(folder_path = "/tmp", folder_name, replies) {
  # Create an mbox file from a list of replies

  # Create a unique filename for the mbox file
  mbox_filepath <- file.path(folder_path, paste0(folder_name, ".mbox"))

  # Open the mbox file for writing
  mbox_file <- file(mbox_filepath, "w")

  # Iterate through each reply and write it to the mbox file
  for (reply in replies) {
    cat(reply, "\n\n", file = mbox_file)
  }

  # Close the mbox file
  close(mbox_file)

  # Return the path of the created mbox file
  return(mbox_filepath)
}














