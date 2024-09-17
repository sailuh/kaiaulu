# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Downloader ##############

#' Download all pipermail files in an archive as mbox files
#' @param mailing_list The name of the mailing list being downloaded
#' @param start_year_month The year and month of the first file to be downloaded
#' @param end_year_month The year and month of the last file to be downloaded
#' @param save_folder_path The folder path in which all the downloaded pipermail files will be stored
#' @return Returns `downloaded_files`, a vector of the downloaded files in the current working directory
#' @export
download_pipermail <- function(mailing_list, start_year_month, end_year_month, save_folder_path) {

  # Create directory if it does not exist
  if (!dir.exists(save_folder_path)) {
    dir.create(save_folder_path, recursive = TRUE)
  }

  # Ensure mailing_list URL ends with a slash
  if (!stringi::stri_endswith_fixed(mailing_list, "/")) {
    mailing_list <- paste0(mailing_list, "/")
  }

  # Get mailing list contents
  response <- httr::GET(mailing_list, httr::timeout(60))
  if (httr::status_code(response) != 200) {
    stop("Failed to access the mailing list page.")
  }

  # Parse the response
  parsed_response <- httr::content(response, "text")
  doc_obj <- XML::htmlParse(parsed_response, asText = TRUE)

  # Table rows
  rows <- XML::getNodeSet(doc_obj, "//tr")

  # Skip header row
  data_rows <- rows[-1]

  # Vector for link storage
  links <- c()

  # Extract the date and link from each row
  for (row in data_rows) {
    # Date in YYYYMM format
    date_extracted <- XML::xpathSApply(row, ".//td[1]", XML::xmlValue)
    date_cleaned <- stringi::stri_replace_last_regex(date_extracted, pattern = ":$", replacement = "")
    date_cleaned <- stringi::stri_trim_both(date_cleaned)
    # Parse the date
    date_parsed <- as.Date(paste0("01 ", date_cleaned), format = "%d %B %Y")
    if (is.na(date_parsed)) {
      warning("Date could not be parsed: ", date_cleaned)
      next
    }
    year_month <- format(date_parsed, "%Y%m")

    # Check if date is within range
    if (year_month >= start_year_month & year_month <= end_year_month) {
      # Get href from column 3
      link_nodes <- XML::xpathSApply(row, ".//td[3]/a", XML::xmlGetAttr, 'href')
      if (length(link_nodes) == 0) {
        warning("No link found in row for date: ", date_cleaned)
        next
      }
      # Store the link in links
      link <- link_nodes[1]
      links <- c(links, link)
    }
  }

  # Vector for downloaded files
  downloaded_files <- c()
  for (i in seq_along(links)) {
    link <- links[i]

    # Extract the name without the .txt.gz extension
    base_name <- gsub("\\.txt\\.gz$", "", link)

    # Parse the date from the base name
    date_parsed <- as.Date(paste0("01-", base_name), format = "%d-%Y-%B")
    if (is.na(date_parsed)) {
      warning("Could not parse date from link: ", link)
      next
    }
    year_month_clean <- format(date_parsed, "%Y%m")

    # Download URL
    txt_url <- paste0(mailing_list, gsub("\\.gz$", "", link))
    gz_url <- paste0(mailing_list, link)

    # Attempt to download the .txt file first
    download_url <- txt_url
    response <- httr::GET(download_url, httr::timeout(60))

    if (httr::status_code(response) != 200) {
      # Fallback to .gz file if .txt is unavailable
      download_url <- gz_url
      response <- httr::GET(download_url, httr::timeout(60))
      if (httr::status_code(response) != 200) {
        cat("Both .txt and .gz downloads failed for link: ", link, "\n")
        next
      }
    }

    # Define the destination file
    dest <- file.path(save_folder_path, paste0('kaiaulu_', year_month_clean, '.mbox'))

    # Print diagnostic info
    cat("Downloading: ", download_url, "\n")
    cat("Saving to: ", dest, "\n")

    # Write file to disk
    if (grepl("\\.gz$", download_url)) {
      # Download the .gz file
      gz_file_path <- file.path(save_folder_path, paste0('kaiaulu_', year_month_clean, '.mbox.gz'))
      httr::GET(download_url, httr::write_disk(gz_file_path, overwrite = TRUE), httr::timeout(60))

      # Unzip the file
      gz_con <- gzfile(gz_file_path, open = "rb")
      out_con <- file(dest, open = "wb")
      while (TRUE) {
        bytes <- readBin(gz_con, what = raw(), n = 1024 * 1024)
        if (length(bytes) == 0) break
        writeBin(bytes, out_con)
      }
      close(gz_con)
      close(out_con)

      # Remove the gz file after unzipping
      file.remove(gz_file_path)
    } else {
      # Download the .txt file directly
      httr::GET(download_url, httr::write_disk(dest, overwrite = TRUE), httr::timeout(60))
    }

    # Add the downloaded file to the list
    downloaded_files <- c(downloaded_files, dest)
  }

  # Return downloaded files
  return(downloaded_files)
}


#' Refresh mbox files downloaded via pipermail
#' Uses the adopted file name convention by \code{\link{download_pipermail}} to identify
#' the latest downloaded mbox. It deletes this file, then redownloads it along with all future months
#' up to the current real-life month.
#' If the directory is empty, then it downloads all pipermail files (as mbox files) via \code{\link{download_pipermail}}
#' @param mailing_list The name of the mailing list being downloaded
#' @param start_year_month The year and month of the first file to be downloaded
#' @param save_folder_path The folder path in which all the downloaded pipermail files will be stored
#' @return Returns `downloaded_files`, a vector of the downloaded files in the current working directory
#' @export
refresh_pipermail <- function(mailing_list, start_year_month, save_folder_path) {

  # Create directory if it does not exist
  if (!dir.exists(save_folder_path)) {
    dir.create(save_folder_path, recursive = TRUE)
  }

  # Check if the folder is empty
  files_in_folder <- list.files(save_folder_path, pattern = "kaiaulu_\\d{6}\\.mbox$")
  if (length(files_in_folder) == 0) {
    # If empty, download from start_year_month to the current month
    end_year_month <- format(Sys.Date(), "%Y%m")
    cat("Folder is empty. Downloading from", start_year_month, "to", end_year_month, "\n")
    download_pipermail(mailing_list, start_year_month, end_year_month, save_folder_path)
    return(NULL)
  }
  # If folder is not empty, find the most recent month
  year_months <- gsub("kaiaulu_(\\d{6})\\.mbox$", "\\1", files_in_folder)
  recent_month <- max(year_months)

  # Delete the most recent file
  recent_file <- file.path(save_folder_path, paste0("kaiaulu_", recent_month, ".mbox"))
  if (file.exists(recent_file)) {
    file.remove(recent_file)
    cat("Deleted the most recent file:", recent_file, "\n")
  }

  # Redownload from the most recent month to the current real-life month
  end_year_month <- format(Sys.Date(), "%Y%m")
  cat("Redownloading from", recent_month, "to", end_year_month, "\n")
  download_pipermail(mailing_list, recent_month, end_year_month, save_folder_path)
}


#' Process .gz files in a folder, unzip and convert them to .mbox
#' Checks a folder for any .gz files, unzips them, and renames them
#' to .mbox format. The original .gz files are deleted after unzipping. If a .mbox
#' file with the same name already exists, it will be overwritten.
#'
#' @param folder_path The path to the folder containing both .gz and .mbox files.
#' @return A list of the .mbox files that were created or updated.
#' @export
process_gz_to_mbox_in_folder <- function(folder_path) {

  # Get the list of files in the folder
  files <- list.files(folder_path, full.names = TRUE)

  # Find .gz files
  gz_files <- files[grepl("\\.gz$", files)]

  # Check if there are no .gz files
  if (length(gz_files) == 0) {
    cat("This folder does not contain any .gz files.\n")
    return(NULL)
  }

  # Vector to store names of converted .mbox files
  converted_mbox_files <- c()

  # Process .gz files
  for (gz_file in gz_files) {
    # Define the corresponding .mbox file path (remove .gz and replace with .mbox)
    mbox_file <- gsub("\\.gz$", ".mbox", gz_file)

    cat("Processing:", gz_file, " -> ", mbox_file, "\n")

    # Open .gz file and unzip its contents to .mbox
    gz_con <- gzfile(gz_file, open = "rb")
    out_con <- file(mbox_file, open = "wb")

    # Read and write the contents
    while (TRUE) {
      bytes <- readBin(gz_con, what = raw(), n = 1024 * 1024)
      if (length(bytes) == 0) break
      writeBin(bytes, out_con)
    }

    # Close connections
    close(gz_con)
    close(out_con)

    # Remove the .gz file
    file.remove(gz_file)

    # Add the converted file to the list
    converted_mbox_files <- c(converted_mbox_files, mbox_file)
  }

  # Return the list of converted .mbox files
  return(converted_mbox_files)
}


#' Compose mod_mbox archives (.mbox) into a single mbox file for use with \code{\link{parse_mbox}}
#' @param base_url An url pointing to the mod_mbox directory (e.g. "http://mail-archives.apache.org/mod_mbox") without trailing slashes
#' @param mailing_list Name of the project mailing list (e.g. apr-dev) in the mod_mbox directory
#' @param from_year First year in the range to be downloaded
#' @param to_year Last year in the range to be downloaded
#' @param save_file_path the full path, including file name and extension to save the file
#' @param is_per_month If TRUE, does not delete monthly files in tmp. (Default = TRUE)
#' @param verbose Prints progress during execution
#' @return Returns the path of the downloaded mbox file.
#' @export
download_mod_mbox <- function(base_url, mailing_list, from_year, to_year, save_file_path,is_per_month=TRUE,verbose=FALSE) {


  #Initialize variables
  counter <- 0
  destination <- list()

  #Open file handle to output file
  output <- path.expand(save_file_path)
  fileConn <- file(output, "w+")

  #Loop through time and compose the mbox file
  for (year in (from_year:to_year)) {

    for (month in 1:12) {
      counter <- counter + 1

      #Generate file destinations for the monthly files in /tmp/
      destination[[counter]] <- sprintf("%d%02d.mbox", year, month)

      if(verbose){
        print(stringi::stri_c("Downloading:",destination[[counter]],sep = " "))
      }

      #Try file download and save result
      full_month_url <- stringi::stri_c(base_url, mailing_list, destination[[counter]], sep = "/")
      full_tmp_save_path <- file.path('/tmp',destination[[counter]])
      x <- httr::GET(full_month_url,
                     httr::write_disk(full_tmp_save_path,overwrite=TRUE))

      #If download was successful, write to mbox file, if not, delete file
      if (httr::http_error(x) == FALSE) {

        #Open read connection
        readCon <- file(full_tmp_save_path, "r")

        data <- readLines(full_tmp_save_path)

        #Write data to output
        writeLines(data, fileConn)

        #Close read connection
        close(readCon)
      }

      #Delete the /tmp/ monthly files
      if(!is_per_month){
        unlink(full_tmp_save_path, force = TRUE)
      }


    }

  }

  #Close connection to target mbox file
  close(fileConn)

  #return output location
  return(output)
}

#' Compose mod_mbox archives (.mbox) into a single mbox file for use with \code{\link{parse_mbox}}
#' @param archive_url A url pointing to the mod_mbox mailing list directory (e.g. "http://mail-archives.apache.org/mod_mbox/apr-dev") without trailing slashes
#' @param mailing_list Name of the project mailing list (e.g. apr-dev) in the mod_mbox directory
#' @param archive_type Name of the archive that the project mailing list is archived in (e.g. apache)
#' @param from_year First year in the range to be downloaded
#' @param to_year Last year in the range to be downloaded
#' @param save_folder_path the full *folder* path where the monthly downloaded mbox will be stored.
#' @param verbose Prints progress during execution
#' @return Returns the path of the downloaded mbox file.
#' @export
download_mod_mbox_per_month <- function(archive_url, mailing_list, archive_type, from_year, to_year, save_folder_path,verbose=FALSE) {


  #Initialize variables
  counter <- 0
  destination <- list()

  #Open file handle to output file
  output <- path.expand(save_folder_path)

  current_date <- Sys.Date()
  current_year <- as.numeric(substr(current_date, 1, 4))
  current_month <- as.numeric(substr(current_date, 6, 7))

  #Loop through time and compose the mbox file
  for (year in (from_year:to_year)) {

    for (month in 1:12) {
      # Check to stop function when month iterates path current real life month
      if (year == current_year && month > current_month) {
        return(output)
      }
      counter <- counter + 1

      #Generate file destinations for the monthly files in /tmp/
      destination[[counter]] <- sprintf("%d%02d.mbox", year, month)
      mbox_file_name <- stringi::stri_c(mailing_list, archive_type, destination[[counter]], sep = "_")

      if(verbose){
        print(stringi::stri_c("Downloading:",mbox_file_name,sep = " "))
      }

      #Try file download and save result
      full_month_url <- stringi::stri_c(archive_url, destination[[counter]], sep = "/")
      full_tmp_save_path <- file.path(output,mbox_file_name)
      x <- httr::GET(full_month_url,
                     httr::write_disk(full_tmp_save_path,overwrite=TRUE))

      # Remove file if error
      # Can only be done post-write, see https://github.com/r-lib/httr/issues/553
      if (httr::http_error(x) && file.exists(full_tmp_save_path)) {
        warning(paste0("Unable to download: ",destination[[counter]]))
        file.remove(full_tmp_save_path)
      }

    }

  }

  #return output location
  return(output)
}


############## Parsers ##############

#' Parse mbox from Perceval
#'
#' Parses an mbox file, which consists of emails in a mailbox, using the Perceval library.
#' Note .mbox files do not have a consistent number of fields (e.g. Reply Cc.). Due to that,
#' the resulting table of parse mbox may have a different number of columns depending on the
#' data used. This function only ensures if columns of interest are available, then they are
#' consistently renamed for clarity.
#'
#' @param perceval_path path to perceval binary
#' @param mbox_path path to mbox archive file (ends in .mbox)
#' @export
#' @family parsers
parse_mbox <- function(perceval_path,mbox_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  perceval_path <- path.expand(perceval_path)
  mbox_path <- path.expand(mbox_path)
  # Remove ".mbox"
  mbox_uri <- stri_replace_last(mbox_path,replacement="",regex=".mbox")
  # Use percerval to parse mbox_path. --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- system2(perceval_path,
                             args = c('mbox',mbox_uri,mbox_path,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)
  # Parsed JSON output as a data.table.
  perceval_parsed <- data.table(jsonlite::stream_in(textConnection(perceval_output),verbose=FALSE))

  columns_of_interest <- c("data.Message.ID","data.In.Reply.To","data.Date","data.From","data.To","data.Cc","data.Subject","data.body.plain","data.body")
  columns_rename <- c("reply_id","in_reply_to_id","reply_datetimetz","reply_from","reply_to","reply_cc","reply_subject","reply_body","reply_body")
  is_available_column <- columns_of_interest %in% colnames(perceval_parsed)

  columns_of_interest <- columns_of_interest[is_available_column]

  perceval_parsed <- perceval_parsed[,..columns_of_interest]

  data.table::setnames(x = perceval_parsed,
                       old = colnames(perceval_parsed),
                       new = columns_rename[is_available_column])

  return(perceval_parsed)
}

#' Parse mbox latest date
#'
#' Returns the name of the latest mod_mbox file downloaded in the specified folder
#'
#' The folder assumes the following convention: "(mailing_list)_(archive_type)_yearmonth.mbox"
#' For example: "geronimo-dev_apache_202401.mbox". This nomenclature is defined by \code{\link{download_mod_mbox_per_month}}
#'
#' @param mbox_path path to mbox archive file (ends in .mbox)
#' @return Returns the name of the latest mod_mbox file
#' @export
#' @family parsers
parse_mbox_latest_date <- function(mbox_path) {
  file_list <- list.files(mbox_path)
  date_list <- list()
  for(i in file_list){
    i <- sub(".mbox", "", i)
    i <- sub("[^_]*_[^_]*_", "", i)
    date_list <- append(date_list, i)
  }
  latest_date <- as.character(max(unlist(date_list)))
  latest_mbox_file <- grep(latest_date, file_list, value = TRUE)
  return(latest_mbox_file)
}

############## Fake Generator ##############

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




