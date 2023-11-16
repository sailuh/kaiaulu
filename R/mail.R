# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.




## Creates fake mbox data
#' @param mlist specific mailing list associated with this mbox data
#' @param sender information of person sending the mail
#' @param date First year in the range to be downloaded
#' @param subject subject of the email
#' @param content the body of the email
#' @param randomise_email_case If TRUE, randomizes the email case. (Default = TRUE)
#' @export
create_fake_mbox <- function(mlist, sender, date, subject, content, randomise_email_case = TRUE) {

# format the date correctly
  cdate <- format(as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%S"), "%a, %d %b %Y %H:%M:%S")


  #execute random concatenation of characters to replace the subject and content parameters if anonymity is desired
 # if (randomise_email_case) {
  #  subject <- paste0(sample(c(tolower, toupper), nchar(subject), replace = TRUE),(strsplit(subject, "")), collapse = "")
 #   content <- paste0(sample(c(tolower, toupper), nchar(content), replace = TRUE),(strsplit(content, "")), collapse = "")
 # }

  mbox_content <- paste0(
    "From MAILER-DAEMON Thu Jul 18 13:48:48 2013",
    "\nPath: example.com!not-for-mail",
    "\nFrom: ", sender,
    "\nNewsgroups: gmane.", mlist,
    "\nSubject: ", subject,
    "\nDate: ", cdate,
    "\nApproved: auto",
    "\nMessage-ID: <", length(content), "@example.com>",
    "\nNNTP-Posting-Host: example.com",
    "\nMime-Version: 1.0",
    "\nContent-Type: text/plain; charset=us-ascii; format=flowed",
    "\nContent-Transfer-Encoding: 7bit",
    "\nX-Complaints-To: complaints@example.com",
    "\nNNTP-Posting-Date: ", cdate,
    "\nUser-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.8) Gecko/20020205",
    "\nX-Accept-Language: en-us",
    "\nOriginal-To: ", paste0(mlist, "@example.com"),
    "\nPrecedence: bulk",
    "\nX-Mailing-List: ", paste0(mlist, "@example.com"),
    "\n\n", content
  )

  return(mbox_content)
}


# Call the function with specific values
mlist <- "test-list"
sender <- "john.doe@example.com"
date <- "2023-01-15T08:30:00"
subject <- "Test Email Subject"
content <-  "This is the body of the test email."
