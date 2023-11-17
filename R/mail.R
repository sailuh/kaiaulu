# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.




## Creates fake mbox data
#'
#' @param mlist specific mailing list associated with this mbox data
#' @param reply_from email of sender
#' @param reply_to email of receiver
#' @param reply_cc carbon copy of email to additional recipients
#' @param reply_datetime Date, time in the following format: 2023-02-11T09:30:00
#' @param timezone Abbreviated timezone of the reply eg: HST or EST
#' @param reply_subject subject of the email
#' @param reply_body the body of the email
#' @return the content of the fake mbox you created
#' @export
create_fake_mbox <- function(mlist, reply_from, reply_to, reply_cc, reply_datetime, timezone, reply_subject, reply_body) {

# format the date correctly
 cdate <- format(as.POSIXct(reply_datetime, format = "%Y-%m-%dT%H:%M:%S "), "%a, %d %b %Y %H:%M:%S ")


  mbox_content <- paste0(
    "From MAILER-DAEMON Thu Jul 18 13:48:48 2013 ",
    "\nPath: example.com!not-for-mail ",
    "\nFrom: ", reply_from,
    "\nNewsgroups: gmane. ", mlist,
    "\nSubject: ", reply_subject,
    "\nDate: ", cdate,
    "\nApproved: auto ",
    "\nMessage-ID: <", length(reply_body), "@example.com> ",
    "\nNNTP-Posting-Host: example.com",
    "\nMime-Version: 1.0 ",
    "\nContent-Type: text/plain; charset=us-ascii; format=flowed ",
    "\nContent-Transfer-Encoding: 7bit ",
    "\nX-Complaints-To: complaints@example.com ",
    "\nNNTP-Posting-Date: ", cdate, timezone,
    "\nUser-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:0.9.8) Gecko/20020205 ",
    "\nX-Accept-Language: en-us ",
    "\nOriginal-To: ", paste0(reply_to, "@example.com ", reply_cc, "@example.com "),
    "\nPrecedence: bulk",
    "\nX-Mailing-List: ", paste0(mlist, "@example.com "),
    "\n\n", reply_body
  )

  return(mbox_content)
}


