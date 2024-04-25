# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Downloader ##############

#' Download all pipermail files in an archive as mbox files
#' @param archive_url An url pointing to a pipermail archive
#' @param mailing_list The name of the mailing list being downloaded
#' @param archive_type The name of the type of archive that the mailing list is stored in
#' @param save_folder_path The folder path in which all the downloaded pipermail files will be stored
#' @return Returns `destination`, a vector of the downloaded files in the current working directory
#' @export
download_pipermail <- function(archive_url, mailing_list, archive_type, save_folder_path) {

  #Get page
  pagedata <- httr::GET(archive_url)

  #Parse html file into object
  tbls_xml <- XML::htmlParse(pagedata)

  #Extract href tablenodes from html table
  tableNodes <- XML::getNodeSet(tbls_xml, "//td/a[@href]")

  #Extract filenames from tablenode content with xmlGetAtrr
  hrefs <- sapply(tableNodes, XML::xmlGetAttr, 'href')

  #Create Vector
  files <- vector()
  file_names <- vector()

  #Compose download urls for both gunzipped and plain text files
  for (i in hrefs ){
    if (endsWith(i, ".txt.gz")){
      f_month <- match(sub("[^_]*-","", sub(".txt.gz","",i)), month.name)
      f_year <- sub("-[^_]*", "", i)
      file_names <- c(file_names, sprintf("%s%02d.mbox", f_year, f_month))
      i <- stringi::stri_c(archive_url, i, sep = "/")
      files <- c(files, i)
    } else if (endsWith(i, ".txt")) {
      f_month <- match(sub("[^_]*-","", sub(".txt","",i)), month.name)
      f_year <- sub("-[^_]*", "", i)
      file_names <- c(file_names, sprintf("%s%02d.mbox", f_year, f_month))
      i <- stringi::stri_c(archive_url, i, sep = "/")
      files <- c(files, i)
    }
  }
  amount <- length(files)
  # File downloading loop
  for (i in 1:amount){

    #split filename from url and create download destination out of it
    #splits <- stringi::stri_split_fixed(i, "/")
    #destination[[i]] <- paste0(splits[[1]][[length(splits[[1]])]])

    #download file and place it at the destination
    save_file_name <- stringi::stri_c(mailing_list, archive_type, file_names[[i]], sep = "_")
    save_file_path <- stringi::stri_c(save_folder_path, save_file_name, sep = "/")
    httr::GET(files[[i]], httr::write_disk(save_file_path, overwrite=TRUE))
  }

  #Return filenames
  return(save_folder_path)

}


#' Convert pipermail archive files (.txt and .txt.gz) into an mbox format for use with \code{\link{parse_mbox}}
#' @param filelist A vector of pipermail archive files from \code{\link{download_pipermail}}
#' @return Returns `output`, the name of the resulting .mbox file in the current working directory
#' @export
convert_pipermail_to_mbox <- function(filelist) {

  #at to @ replace function
  pipermail_atreplacer <- function(string) {

    rstring <- sub(" at ", "@", string)

    return(rstring)

  }

  output <- "output.mbox"

  #Create mbox file and file connection
  file.create(output)
  fileConn <- file(output, "w+")


  #Read lines from downloaded files and write them to mbox file
  for (filename in filelist[]){

    #Open read connection
    readCon <- file(filename, "r")

    data <- readLines(filename)

    #Find email headers to send to 'at' to @ replacer
    for (i in 1:length(data)) {

      data[i] <- sub("From:? \\S+ at \\S+", pipermail_atreplacer(data[i]), data[i])

    }

    #Write files to output
    writeLines(data, fileConn)

    #Close read connection
    close(readCon)

    #Delete the file
    unlink(filename, force = TRUE)
  }

  #Close connection to mbox file
  close(fileConn)

  #return output location
  return(output)
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

#' Refresh mbox files
#'
#' Uses the adopted file name convention by \code{\link{download_mod_mbox_per_month}} to identify
#' the latest downloaded mbox year i and month j. It deletes the mbox file of year i and month j,
#' then redownloads it along with the remaining months past j up to 12. Then, it calls
#' \code{\link{download_mod_mbox_per_month}} with from_year being year i+1 and to_year being
#' the current real-life year so that all newer mbox files are downloaded.
#'
#' If the directory is empty, then it downloads all mbox files starting from a definable starting year to
#' the current real-life year.
#'
#' @param archive_url A url pointing to the mod_mbox mailing list directory (e.g. "http://mail-archives.apache.org/mod_mbox/apr-dev") without trailing slashes
#' @param mailing_list Name of the project mailing list (e.g. apr-dev) in the mod_mbox directory
#' @param archive_type Name of the archive that the project mailing list is archived in (e.g. apache)
#' @param from_year First year in the range to be downloaded in case there are no mod_mbox files already downloaded (e.g. 201401)
#' @param save_folder_path the full *folder* path where the monthly downloaded mbox will be stored.
#' @param verbose Prints progress during execution
#' @export
refresh_mod_mbox <- function(archive_url, mailing_list, archive_type, from_year, save_folder_path,verbose=FALSE) {
  # Get a list of mbox files currently downloaded in save path folder
  existing_mbox_files <- list.files(save_folder_path)
  output <- save_folder_path

  # Get the current year
  current_date <- Sys.Date()
  current_year <- as.numeric(substr(current_date, 1, 4))
  current_month <- as.numeric(substr(current_date, 6, 7))

  # If there are no mbox files downloaded, then download mbox files as normal using download_mod_mbox_per_month
  if (length(existing_mbox_files) == 0) {
    if (verbose) {
      message("The folder is empty. Downloading mbox files from ", from_year, " to ", current_year, ". \n")
    }
    download_mod_mbox_per_month(archive_url = archive_url,
                                mailing_list = mailing_list,
                                archive_type = archive_type,
                                from_year = from_year,
                                to_year = current_year,
                                save_folder_path = save_folder_path,
                                verbose = verbose)
  } else {
    counter <- 0
    destination <- list()
    latest_file_name <- parse_mbox_latest_date(save_folder_path)
    extracted_year_month <- sub("[^_]*_[^_]*_", "", sub(".mbox", "", latest_file_name))
    output <- path.expand(save_folder_path)

    latest_downloaded_year <- as.numeric(substr(extracted_year_month, 1, 4))
    latest_downloaded_month <- as.numeric(substr(extracted_year_month, 6, 7))
    this_file <- paste(save_folder_path, latest_file_name, sep = "/")
    file.remove(this_file)
    # Download files starting from deleted file month to end of that year
    for (month in (latest_downloaded_month:12)) {
      # Checks to see if iterator goes beyond current month, stops function if it does
      if (latest_downloaded_year == current_year && month > current_month) {
        return(output)
      }
      counter <- counter + 1

      #Generate file destinations for the monthly files in /tmp/
      destination[[counter]] <- sprintf("%d%02d.mbox", latest_downloaded_year, month)
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
        warning(paste0("Unable to download: ",mbox_file_name))
        file.remove(full_tmp_save_path)
      }

    }

    # Call the per-month-downloader to download the new mail missing from the user's machine
    download_mod_mbox_per_month(archive_url = archive_url,
                                mailing_list = mailing_list,
                                archive_type = archive_type,
                                from_year = (latest_downloaded_year+1),
                                to_year = current_year,
                                save_folder_path = save_folder_path,
                                verbose = verbose)
  }
  # End of if-else
}

#' Refresh mbox files downloaded via pipermail
#'
#' Uses the adopted file name convention by \code{\link{download_pipermail}} to identify
#' the latest downloaded mbox year i and month j. It deletes the mbox file of year i and month j,
#' then redownloads it along with the remaining months past j up to 12. Then, it calls
#' \code{\link{download_mod_mbox_per_month}} with from_year being year i+1 and to_year being
#' the current real-life year so that all newer mbox files are downloaded.
#'
#' If the directory is empty, then it downloads all pipermail files (as mbox files) via \code{\link{download_pipermail}}
#'
#' @param archive_url A url pointing to the mod_mbox mailing list directory (e.g. "http://mail-archives.apache.org/mod_mbox/apr-dev") without trailing slashes
#' @param mailing_list Name of the project mailing list (e.g. apr-dev) in the mod_mbox directory
#' @param archive_type Name of the archive that the project mailing list is archived in (e.g. apache)
#' @param save_folder_path the full *folder* path where the monthly downloaded mbox will be stored.
#' @param verbose prints progress during execution
#' @export
refresh_pipermail <- function(archive_url, mailing_list, archive_type, save_folder_path,verbose=FALSE) {
  # Get a list of mbox files currently downloaded in save path folder
  existing_mbox_files <- list.files(save_folder_path)

  # Get the current year
  current_date <- Sys.Date()
  current_year <- as.numeric(substr(current_date, 1, 4))
  current_month <- as.numeric(substr(current_date, 6, 7))

  # If there are no mbox files downloaded, then download mbox files as normal using download_pipermail
  if (length(existing_mbox_files) == 0) {
    if (verbose) {
      message("The folder is empty. Downloading all pipermail files. \n")
    }
    download_pipermail(archive_url = archive_url,
                                mailing_list = mailing_list,
                                archive_type = archive_type,
                                save_folder_path = save_folder_path)
  } else {
    latest_file_name <- parse_mbox_latest_date(save_folder_path)
    extracted_year_month <- sub("[^_]*_[^_]*_", "", sub(".mbox", "", latest_file_name))
    output <- path.expand(save_folder_path)

    latest_downloaded_year <- as.numeric(substr(extracted_year_month, 1, 4))
    latest_downloaded_month <- as.numeric(substr(extracted_year_month, 5, 6))
    this_file <- paste(save_folder_path, latest_file_name, sep = "/")
    file.remove(this_file)

    # Download txt files starting from deleted file month to end of that year, save as mbox
    download_txt_files_latest_downloaded_year <- function(archive_url, mailing_list, archive_type, latest_downloaded_year, latest_downloaded_month,  current_year, current_month, save_folder_path) {
      counter <- 0
      destination <- list()
      mbox_correct_name_format <- list()
      output <- save_folder_path

      for (month in (latest_downloaded_month:12)) {
        if (latest_downloaded_year == current_year && month > current_month) {
          return(output)
        }
        counter <- counter + 1

        #Generate file destinations for the monthly files in /tmp/
        destination[[counter]] <- sprintf("%d-%s.txt", latest_downloaded_year, month.name[month])
        mbox_correct_name_format[[counter]] <- sprintf("%d%02d.mbox", latest_downloaded_year, month)
        mbox_file_name <- stringi::stri_c(mailing_list, archive_type, mbox_correct_name_format[[counter]], sep = "_")

        #Try file download and save result
        full_month_url <- stringi::stri_c(archive_url, destination[[counter]], sep = "/")
        full_tmp_save_path <- file.path(output,mbox_file_name)
        x <- httr::GET(full_month_url,
                       httr::write_disk(full_tmp_save_path,overwrite=TRUE))

        # Remove file if error
        # Can only be done post-write, see https://github.com/r-lib/httr/issues/553
        if (httr::http_error(x) && file.exists(full_tmp_save_path)) {
          file.remove(full_tmp_save_path)
        }

      }
    }

    # Download txt.gz files starting from deleted file month to the end of that year, save as mbox
    download_txt_gz_files_latest_downloaded_year <- function(archive_url, mailing_list, archive_type, latest_downloaded_year, latest_downloaded_month, current_year, current_month, save_folder_path) {

      counter <- 0
      destination <- list()
      mbox_correct_name_format <- list()
      output <- save_folder_path

      for (month in (latest_downloaded_month:12)) {
        if (latest_downloaded_year == current_year && month > current_month) {
          return(output)
        }
        counter <- counter + 1

        #Generate file destinations for the monthly files in /tmp/
        destination[[counter]] <- sprintf("%d-%s.txt.gz", latest_downloaded_year, month.name[month])
        mbox_correct_name_format[[counter]] <- sprintf("%d%02d.mbox", latest_downloaded_year, month)
        mbox_file_name <- stringi::stri_c(mailing_list, archive_type, mbox_correct_name_format[[counter]], sep = "_")

        #Try file download and save result
        full_month_url <- stringi::stri_c(archive_url, destination[[counter]], sep = "/")
        full_tmp_save_path <- file.path(output,mbox_file_name)
        x <- httr::GET(full_month_url,
                       httr::write_disk(full_tmp_save_path,overwrite=TRUE))

        # Remove file if error
        # Can only be done post-write, see https://github.com/r-lib/httr/issues/553
        if (httr::http_error(x) && file.exists(full_tmp_save_path)) {
          file.remove(full_tmp_save_path)
        }

      }
    }

    # Download txt files from the year after the latest downloaded year to the current real life year
    download_txt_files_current_year <- function(archive_url, mailing_list, archive_type, latest_downloaded_year, current_year, current_month, save_folder_path) {

      counter <- 0
      destination <- list()
      mbox_correct_name_format <- list()
      output <- save_folder_path

      for (year in (latest_downloaded_year+1):current_year) {
        for (month in (1:12)) {
          if (year == current_year && month > current_month) {
            return(output)
          }
          counter <- counter + 1

          #Generate file destinations for the monthly files in /tmp/
          destination[[counter]] <- sprintf("%d-%s.txt", year, month.name[month])
          mbox_correct_name_format[[counter]] <- sprintf("%d%02d.mbox", year, month)
          mbox_file_name <- stringi::stri_c(mailing_list, archive_type, mbox_correct_name_format[[counter]], sep = "_")

          #Try file download and save result
          full_month_url <- stringi::stri_c(archive_url, destination[[counter]], sep = "/")
          full_tmp_save_path <- file.path(output,mbox_file_name)
          x <- httr::GET(full_month_url,
                         httr::write_disk(full_tmp_save_path,overwrite=TRUE))

          # Remove file if error
          # Can only be done post-write, see https://github.com/r-lib/httr/issues/553
          if (httr::http_error(x) && file.exists(full_tmp_save_path)) {
            file.remove(full_tmp_save_path)
          }

        }
      }

    }

    # Download txt.gz files from the year after the latest downloaded year to the current real life year
    download_txt_gz_files_current_year <- function(archive_url, mailing_list, archive_type, latest_downloaded_year, current_year, current_month, save_folder_path) {

      counter <- 0
      destination <- list()
      mbox_correct_name_format <- list()
      output <- save_folder_path

      for (year in (latest_downloaded_year+1):current_year) {
        for (month in (1:12)) {
          if (year == current_year && month > current_month) {
            return(output)
          }
          counter <- counter + 1

          #Generate file destinations for the monthly files in /tmp/
          destination[[counter]] <- sprintf("%d-%s.txt.gz", year, month.name[month])
          mbox_correct_name_format[[counter]] <- sprintf("%d%02d.mbox", year, month)
          mbox_file_name <- stringi::stri_c(mailing_list, archive_type, mbox_correct_name_format[[counter]], sep = "_")

          #Try file download and save result
          full_month_url <- stringi::stri_c(archive_url, destination[[counter]], sep = "/")
          full_tmp_save_path <- file.path(output,mbox_file_name)
          x <- httr::GET(full_month_url,
                         httr::write_disk(full_tmp_save_path,overwrite=TRUE))

          # Remove file if error
          # Can only be done post-write, see https://github.com/r-lib/httr/issues/553
          if (httr::http_error(x) && file.exists(full_tmp_save_path)) {
            file.remove(full_tmp_save_path)
          }

        }
      }

    }

    download_txt_files_latest_downloaded_year(archive_url=archive_url,
                                              mailing_list=mailing_list,
                                              archive_type=archive_type,
                                              latest_downloaded_year=latest_downloaded_year,
                                              latest_downloaded_month=latest_downloaded_month,
                                              current_year = current_year,
                                              current_month = current_month,
                                              save_folder_path=save_folder_path)

    download_txt_gz_files_latest_downloaded_year(archive_url=archive_url,
                                                mailing_list=mailing_list,
                                                archive_type=archive_type,
                                                latest_downloaded_year=latest_downloaded_year,
                                                latest_downloaded_month=latest_downloaded_month,
                                                current_year = current_year,
                                                current_month = current_month,
                                                save_folder_path=save_folder_path)

    download_txt_files_current_year(archive_url=archive_url,
                                    mailing_list=mailing_list,
                                    archive_type=archive_type,
                                    latest_downloaded_year=latest_downloaded_year,
                                    current_year=current_year,
                                    current_month = current_month,
                                    save_folder_path=save_folder_path)

    download_txt_gz_files_current_year(archive_url=archive_url,
                                    mailing_list=mailing_list,
                                    archive_type=archive_type,
                                    latest_downloaded_year=latest_downloaded_year,
                                    current_year = current_year,
                                    current_month = current_month,
                                    save_folder_path=save_folder_path)
  }
  # End of if-else
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




