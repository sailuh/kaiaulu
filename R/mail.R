# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Pipermail Downloader ##############

#' Download all pipermail files in an archive as mbox files
#'
#' @description This function downloads pipermail archives from a specified pipermail mailing list as .mbox files.
#' It begins by downloading an .html file that contains the URLs for monthly archives in .txt or .gz formats.
#' The function first attempts to download the .txt file, and if unavailable, it falls back to downloading the .gz file.
#'
#' When a .gz file is downloaded, the function automatically unzips and converts it into an .mbox file,
#' overwriting any existing file with the same name. The original .gz file is deleted after extraction.
#'
#' The downloaded .mbox files are saved in the specified folder following the naming convention kaiaulu_YYYYMM.mbox.
#' The function only downloads files that fall between the specified start_year_month and end_year_month.
#' When both formats fail to download, the function issues a warning indicating the missing month.
#' At the end, the function summarizes the downloads, indicating the range of dates present and any missing months.
#'
#' @param mailing_list The name of the mailing list being downloaded e.g. "https://mta.openssl.org/pipermail/openssl-announce/"
#' @param start_year_month The year and month of the first file to be downloaded format: 'YYYYMM'
#' @param end_year_month The year and month of the last file to be downloaded format: 'YYYYMM', or use Sys.Date
#' @param save_folder_path The folder path in which all the downloaded pipermail files will be stored
#' @param verbose if TRUE, prints diagnostic messages during the download process
#' @return Returns `downloaded_files`, a vector of the downloaded files in the current working directory
#' @export
download_pipermail <- function(mailing_list, start_year_month, end_year_month, save_folder_path, verbose = TRUE) {

  ########## Download and Parse Mailing List HTML for Links ##########

  # Ensure mailing_list url ends with a slash, which is important when constructing links for downloading files,
  # since the extracted links are relative to the base URL.
  # e.g.base url: https://mta.openssl.org/pipermail/openssl-announce/ and extracted link: 2024-June.txt.gz
  if (!stringi::stri_endswith_fixed(mailing_list, "/")) {
    mailing_list <- stringi::stri_c(mailing_list, "/")
  }

  # Archive Index Retrieval
  # Begins by downloading an HTML page that lists the URLs
  # for the monthly archives, which are typically available in .txt or .gz formats.

  # Sends a GET request to the mailing list’s URL to retrieve contents. This is the main page of the mailing list archive,
  # which contains links to individual month files (in .txt or .gz format).
  response <- httr::GET(mailing_list, httr::timeout(60))
  if (httr::status_code(response) != 200) {
    stop("Failed to access the mailing list page.")
  }

  # The content is parsed as text to extract the rows of data from the table that contains the file links.
  parsed_response <- httr::content(response, "text")
  doc_obj <- XML::htmlParse(parsed_response, asText = TRUE)

  # Get all table rows in the archive page. These rows contain the links to the individual month files.
  rows <- XML::getNodeSet(doc_obj, "//tr")
  # Skip the header row, to get to data rows
  data_rows <- rows[-1]
  # Create an empty vector for storing the links that will be extracted.
  links <- c()

  ########## Extract Date and Links ##########
  # Loop through the data rows and extract the date and link from each row.
  # The date is in the first column, and the link is in the third column.
  for (row in data_rows) {
    # Extract and clean the date, which is in the format "Month Year" (e.g., "June 2024").
    date_extracted <- XML::xpathSApply(row, ".//td[1]", XML::xmlValue)
    date_cleaned <- stringi::stri_replace_last_regex(date_extracted, pattern = ":$", replacement = "")
    date_cleaned <- stringi::stri_trim_both(date_cleaned)
    # Parse the cleaned date into a valid date object. This allows us to convert it into the "YYYYMM" format.
    date_parsed <- as.Date(stringi::stri_c("01 ", date_cleaned), format = "%d %B %Y")
    if (is.na(date_parsed)) {
      warning("Date could not be parsed: ", date_cleaned)
      next
    }
    year_month <- format(date_parsed, "%Y%m")

    # Check if the extracted year_month falls within the specified range of start_year_month to end_year_month.
    # If it does, proceed to extract the file link from the third column of the row.
    if (year_month >= start_year_month & year_month <= end_year_month) {
      # Get the link (href) from the third column. This is the link to the .txt or .gz file for that month.
      link_nodes <- XML::xpathSApply(row, ".//td[3]/a", XML::xmlGetAttr, 'href')
      if (length(link_nodes) == 0) {
        warning("No link found in row for date: ", date_cleaned)
        next
      }
      # Store the link in the links vector, for later download.
      link <- link_nodes[1]
      links <- c(links, link)
    }
  }

  ########## Initialize Vector for Failed Months ##########
  failed_months <- character()

  ########## Use Links to Download Individual Files ##########
  # Initialize a vector for storing the paths of the downloaded files.
  downloaded_files <- c()
  for (i in seq_along(links)) {
    link <- links[i]

    # Extract the base name of the file (without the .txt.gz extension), so we can construct the correct download paths.
    base_name <- gsub("\\.txt\\.gz$", "", link)

    # Parse the date from the base name and convert it into "YYYYMM" format for consistency with our file naming.
    date_parsed <- as.Date(stringi::stri_c("01-", base_name), format = "%d-%Y-%B")
    if (is.na(date_parsed)) {
      warning("Could not parse date from link: ", link)
      next
    }
    year_month_clean <- format(date_parsed, "%Y%m")

    # Construct the download URLs for both the .txt and .gz versions of the file.
    # The function will first attempt to download the .txt version.
    txt_url <- stringi::stri_c(mailing_list, gsub("\\.gz$", "", link))
    gz_url <- stringi::stri_c(mailing_list, link)

    # The function attempts to download the .txt file for each month.
    # If the .txt file is unavailable, it falls back to downloading the
    # .gz (gzipped) file.
    # Attempt to download the .txt file first
    download_url <- txt_url
    response <- httr::GET(download_url, httr::timeout(60))

    # If the response status code is not 200, the file is not available.
    if (httr::status_code(response) != 200) {
      # Fallback to .gz file if .txt is unavailable
      download_url <- gz_url
      response <- httr::GET(download_url, httr::timeout(60))
      if (httr::status_code(response) != 200) {
        warning("Both .txt and .gz downloads failed for link: ", link, "\n")
        failed_months <- c(failed_months, year_month_clean)
        next
      }
    }

    # Define the destination file name and path where the downloaded content will be saved as a .mbox file.
    dest <- file.path(save_folder_path, stringi::stri_c('kaiaulu_', year_month_clean, '.mbox'))

  ########## Write Downloaded File to Disk ##########
    # Print diagnostic info if verbose is TRUE
    if (verbose) {
      message("Downloading: ", download_url, "\n")
      message("Saving to: ", dest, "\n")
    }

    # Write the downloaded file to disk. If the file is a .gz file, it needs to be unzipped and converted to .mbox format.
    if (grepl("\\.gz$", download_url)) {
      # Download the .gz file to a temporary lomessageion.
      gz_file_path <- file.path(save_folder_path, stringi::stri_c('kaiaulu_', year_month_clean, '.mbox.gz'))
      httr::GET(download_url, httr::write_disk(gz_file_path, overwrite = TRUE), httr::timeout(60))

      # If a .gz file is downloaded, the function unzips it and converts it into an .mbox file.
      # The original .gz file is deleted after extraction to save space.
      # Unzip the .gz file and save the contents as a .mbox file.
      gz_con <- gzfile(gz_file_path, open = "rb")
      out_con <- file(dest, open = "wb")
      while (TRUE) {
        bytes <- readBin(gz_con, what = raw(), n = 1024 * 1024)
        if (length(bytes) == 0) break
        writeBin(bytes, out_con)
      }
      close(gz_con)
      close(out_con)

      # Remove the .gz file after unzipping to avoid storing duplimessagee data.
      file.remove(gz_file_path)
    } else {
      # If the .txt file is available, download it directly and save it as a .mbox file.
      httr::GET(download_url, httr::write_disk(dest, overwrite = TRUE), httr::timeout(60))
    }

    # Add the downloaded file path to the list of downloaded files.
    downloaded_files <- c(downloaded_files, dest)
  }

  ########## Summary of Downloads ##########
  if (length(failed_months) > 0) {
    warning("The following months could not be downloaded (no data available or other error):\n", paste(failed_months, collapse = ", "))
  }
  # List the files in the save_folder_path.
  downloaded_files_in_folder <- list.files(save_folder_path, pattern = "kaiaulu_\\d{6}\\.mbox$", full.names = FALSE)

  # The downloaded .mbox files are saved in the specified folder with the
  # naming convention kaiaulu_YYYYMM.mbox, where YYYYMM represents the year and month.
  # Extract the YYYYMM from the file names.
  downloaded_dates <- as.numeric(sub("kaiaulu_(\\d{6})\\.mbox", "\\1", downloaded_files_in_folder))

  # Create the expected list of YYYYMM between start_year_month and end_year_month.
  start_date <- as.Date(paste0(start_year_month, "01"), format = "%Y%m%d")
  end_date <- as.Date(paste0(end_year_month, "01"), format = "%Y%m%d")
  all_dates <- seq(start_date, end_date, by = "month")
  expected_dates <- as.numeric(format(all_dates, "%Y%m"))

  # Identify missing months.
  missing_months <- setdiff(expected_dates, downloaded_dates)

  # Determine the earliest and latest dates downloaded.
  if (length(downloaded_dates) > 0) {
    min_downloaded_date <- min(downloaded_dates)
    max_downloaded_date <- max(downloaded_dates)

    if (verbose) {
      message("\nSummary of Downloads:\n")
      message("save_folder_path contains mail from date ", min_downloaded_date, " to ", max_downloaded_date, "\n")
    }
  } else {
    if (verbose) {
      message("No files found in save_folder_path\n")
    }
  }

  if (length(missing_months) == 0) {
    if (verbose) {
      message("No missing months\n")
    }
  } else {
    warning("Months missing in the date range: ", paste(missing_months, collapse = ", "), "\n")
  }

  ########## Return List of Downloaded Files ##########
  # Return the list of downloaded .mbox files
  return(downloaded_files)
}

############## Pipermail Refresher ##############

#' Refresh mbox files downloaded via pipermail
#'
#' This function refreshes the mailing list files by checking the contents of a specified folder.
#' If the folder is empty, it calls \code{\link{download_pipermail}} to download all pipermail files from start_year_month to the current month.
#' If the folder contains already-downloaded mbox files, it identifies the most recent month, deletes that file, and redownloads it
#' along with all future months up to the current real-life month.
#'
#' The naming convention of files is `kaiaulu_YYYYMM.mbox`, and the function uses this pattern to identify the most recent month.
#' After deleting the most recent file, the function ensures that the month is redownloaded, along with all subsequent months up to the current month.
#' Redownloading the most recent file makes sure that any files added in that month after the latest refresh are included.
#'
#' @param mailing_list The URL of the mailing list being downloaded (e.g., \url{https://mta.openssl.org/pipermail/openssl-announce/})
#' @param start_year_month The year and month of the first file to be downloaded (format: 'YYYYMM').
#' @param save_folder_path The folder path in which all the downloaded pipermail files will be stored.
#' @param verbose if TRUE, prints diagnostic messages.
#' @return Returns `downloaded_files`, a vector of the newly downloaded files in the current working directory.
#' @export
refresh_pipermail <- function(mailing_list, start_year_month, save_folder_path, verbose = TRUE) {

  ########## Check if Folder is Empty ##########
  # Check the contents of the folder to see if any .mbox files are already present
  # The function looks for files that match the naming pattern 'kaiaulu_YYYYMM.mbox'
  files_in_folder <- list.files(save_folder_path, pattern = "kaiaulu_\\d{6}\\.mbox$")

  if (length(files_in_folder) == 0) {
    # If the folder is empty, download all pipermail files starting from the start_year_month
    # The end date is set to the current month based on the system date
    end_year_month <- format(Sys.Date(), "%Y%m")
    if (verbose) message("Folder is empty. Downloading from", start_year_month, "to", end_year_month, "\n")

    # Call the download_pipermail function to download files from start_year_month to end_year_month
    download_pipermail(mailing_list, start_year_month, end_year_month, save_folder_path)
  }
  ########## Identify the Most Recent Month ##########
  else {
    # If the folder is not empty, identify the most recent month based on the filenames
    # The filenames follow the pattern 'kaiaulu_YYYYMM.mbox', so we extract the YYYYMM part of the filenames
    year_months <- gsub("kaiaulu_(\\d{6})\\.mbox$", "\\1", files_in_folder)

    # Find the most recent month by taking the maximum of the extracted YYYYMM values
    recent_month <- max(year_months)

    # Delete the most recent file before redownloading it
    recent_file <- file.path(save_folder_path, stringi::stri_c("kaiaulu_", recent_month, ".mbox"))
    if (file.exists(recent_file)) {
      file.remove(recent_file)
      if (verbose) message("Deleted the most recent file:", recent_file, "\n")
    }

  ########## Redownload from the Most Recent Month ##########
    # Set the end_year_month to the current month (based on the system date)
    end_year_month <- format(Sys.Date(), "%Y%m")

    # Redownload files from the most recent month (that was just deleted) to the current month
    if (verbose) message("Redownloading from", recent_month, "to", end_year_month, "\n")

    # Call the download_pipermail function to redownload the deleted month and all subsequent months up to the current month
    download_pipermail(mailing_list, recent_month, end_year_month, save_folder_path)
  }
  ########## Process .gz Files After Refresh ##########
  # Call process_gz_to_mbox_in_folder to ensure all .gz files are converted to .mbox after the refresh
  if (verbose) message("Processing .gz files in the folder (if any) to convert them to .mbox format...\n")
  process_gz_to_mbox_in_folder(save_folder_path = save_folder_path, verbose = verbose)
}


#' Process .gz files in a folder and convert them to .mbox
#'
#' This function scans a specified folder for any .gz files, unzips them,
#' and renames them to the .mbox format. After unzipping, the original .gz files are deleted.
#' If a .mbox file with the same name already exists, it will be overwritten.
#' This makes sure that all the files in the folder are in .mbox format, ready for parsing.
#'
#' @param save_folder_path The path to the folder containing both .gz and .mbox files.
#' @param verbose if TRUE, prints diagnostic messages during processing.
#' @return A list of the .mbox files that were created or updated.
#' @export
process_gz_to_mbox_in_folder <- function(save_folder_path, verbose = TRUE) {

  # Get the list of all files in the folder, including full paths
  files <- list.files(save_folder_path, full.names = TRUE)

  # Identify .gz files from the list of files
  gz_files <- files[grepl("\\.gz$", files)]

  # If there are no .gz files, print a message (if verbose is TRUE) and return NULL
  if (length(gz_files) == 0) {
    if (verbose) message("This folder does not contain any .gz files.\n")
    return(invisible(NULL))
  }

  # Create a vector to store the names of the converted .mbox files
  converted_mbox_files <- c()

  ########## Process Each .gz File ##########
  # Iterate over each .gz file, unzip it, and convert it to .mbox
  for (gz_file in gz_files) {
    # Define the corresponding .mbox file path by replacing .gz with .mbox in the file name
    mbox_file <- gsub("\\.gz$", ".mbox", gz_file)

    if (verbose) message("Processing:", gz_file, " -> ", mbox_file, "\n")

    # Open the .gz file in binary mode for reading
    gz_con <- gzfile(gz_file, open = "rb")

    # Create a new .mbox file and open it in binary mode for writing
    out_con <- file(mbox_file, open = "wb")

    # Read the contents of the .gz file and write the chunks to the .mbox file
    while (TRUE) {
      bytes <- readBin(gz_con, what = raw(), n = 1024 * 1024)
      if (length(bytes) == 0) break
      writeBin(bytes, out_con)
    }

    # Close both the input (gz) and output (mbox) file connections
    close(gz_con)
    close(out_con)

    # After successfully converting the file, delete the original .gz file
    file.remove(gz_file)

    # Add the newly created .mbox file to the list of converted files
    converted_mbox_files <- c(converted_mbox_files, mbox_file)
  }

  # Return the vector of all the .mbox files that were created or updated
  return(converted_mbox_files)
}


############## Mod Mbox Downloader ##############

#' Download all mod_mbox files in a mailing list as mbox files
#'
#' @description This function downloads mod_mbox archives from a specified Apache Pony Mail mailing list as .mbox files.
#' It constructs the download URLs for each month based on the start and end date range and downloads the mbox files
#' in the format "YYYY-MM". The downloaded .mbox files are saved in the specified folder, with a naming convention
#' of kaiaulu_YYYYMM.mbox.
#'
#' The function loops through each month in the range specified by `start_year_month` and `end_year_month`,
#' and constructs the appropriate URL to download each month's data. If any download fails, an warning is issued for the failed months.
#' This means the file could not be found and that month's data may not exist.
#' At the end, the function summarizes the downloads, indicating the range of dates present and any missing months.
#'
#' @param mailing_list The URL of the Apache Pony Mail list from which mbox files are to be downloaded
#' (e.g., "https://lists.apache.org/list.html?announce@apache.org").
#' @param start_year_month The year and month of the first file to be downloaded (format: 'YYYYMM').
#' @param end_year_month The year and month of the last file to be downloaded (format: 'YYYYMM').
#' @param save_folder_path The folder path where all the downloaded mbox files will be stored.
#' @param verbose if TRUE, prints detailed messages during the download process.
#' @return Returns `save_folder_path`, the folder path where the mbox files are stored.
#' @export
download_mod_mbox <- function(mailing_list, start_year_month, end_year_month, save_folder_path, verbose = TRUE) {

  ########## Extract Mailing List Name ##########
  # Extract the mailing list name from the given URL. This is because the actual list name is
  # embedded within the URL (after the 'list.html?').
  # We are using 'sub()' to perform a simple string replacement, extracting everything after 'list.html?'.
  mailing_list_name <- sub(".*list.html\\?(.+)", "\\1", mailing_list)
  if (verbose) message("Base list extracted:", mailing_list_name, "\n")

  ########## Prepare Year and Month ##########
  # The start_year_month and end_year_month are in the format "YYYYMM".
  # Split them into year and month for easier looping.
  # Extract first 4 digits as start year, and last 2 digits as start month.
  start_year <- as.numeric(substr(start_year_month, 1, 4))
  start_month <- as.numeric(substr(start_year_month, 5, 6))
  # Extract first 4 digits as end year, and last 2 digits as end month.
  end_year <- as.numeric(substr(end_year_month, 1, 4))
  end_month <- as.numeric(substr(end_year_month, 5, 6))

  ########## Initialize Vectors for Failed Months ##########
  # Vectors to track failed downloads.
  failed_months <- character()

  ########## Download Loop ##########
  # Iterate over the years and months from start_year/month to end_year/month.
  # This is done by looping over the years, and for each year, looping over the 12 months.
  for (year in start_year:end_year) {
    for (month in 1:12) {
      # Skip months before the start_month or after the end_month for the start and end year.
      if (year == start_year && month < start_month) next
      if (year == end_year && month > end_month) break

      ######### Construct URL and Save Path ##########
      # Construct the month string (e.g., '2023-04') and the full download URL.
      # Make sure the month has two digits.
      month_str <- sprintf("%02d", month)
      # Create a string in the format "YYYY-MM"
      year_month_str <- sprintf("%04d-%02d", year, month)
      # This constructs the URL from which the mbox for the current year and month will be downloaded.
      # The format for the URL is fixed by Apache's Pony Mail service.
      download_url <- stringi::stri_c("https://lists.apache.org/api/mbox.lua?list=", mailing_list_name, "&date=", year_month_str)

      # Create the file name where the mbox will be saved locally, in the format ''kaiaulu_'YYYYMM.mbox'.
      file_name <- stringi::stri_c("kaiaulu_", year, month_str, ".mbox")
      file_path <- file.path(save_folder_path, file_name)

      if (verbose) {
        message("Constructed URL:", download_url, "\n")
        message("Saving to file:", file_path, "\n")
      }

      ########## Download Mbox File ##########
      # Download the file using httr::GET, saving it directly to the destination file path.
      response <- httr::GET(download_url, httr::write_disk(file_path, overwrite = TRUE))
      # Get the status code to see if the download succeeded.
      status_code <- httr::status_code(response)

      # Check for successful download (status code 200).
      if (status_code == 200) {
        if (verbose) message("Successfully downloaded:", download_url, "\n")
      } else {
        if (verbose) {
          message("Failed to download:", download_url, "\n")
          message("HTTP Status Code:", status_code, "\n")
        }
        # Remove failed download file.
        unlink(file_path)
        failed_months <- c(failed_months, year_month_str)
      }
    }
  }

  ########## Summary of Failed Downloads ##########
  if (length(failed_months) > 0) {
    warning("The following months could not be downloaded (no data available or other error):\n", paste(failed_months, collapse = ", "))
  }

  # List the files in the save_folder_path
  downloaded_files <- list.files(save_folder_path, pattern = "kaiaulu_\\d{6}\\.mbox$", full.names = FALSE)

  # Extract the YYYYMM from the file names
  downloaded_dates <- as.numeric(sub("kaiaulu_(\\d{6})\\.mbox", "\\1", downloaded_files))

  # Find the expected list of YYYYMM between start_year_month and end_year_month
  start_date <- as.Date(paste0(start_year_month, "01"), format = "%Y%m%d")
  end_date <- as.Date(paste0(end_year_month, "01"), format = "%Y%m%d")
  all_dates <- seq(start_date, end_date, by = "month")
  expected_dates <- as.numeric(format(all_dates, "%Y%m"))

  # Identify missing months
  missing_months <- setdiff(expected_dates, downloaded_dates)

  # Determine the earliest and latest dates downloaded
  if (length(downloaded_dates) > 0) {
    min_downloaded_date <- min(downloaded_dates)
    max_downloaded_date <- max(downloaded_dates)

    if (verbose) {
      message("\nSummary of Downloads:\n")
      message("save_folder_path contains mail from date", min_downloaded_date, "to", max_downloaded_date, "\n")
    }
  } else {
    if (verbose) {
      message("No files found in save_folder_path\n")
    }
  }

  if (length(missing_months) == 0) {
    if (verbose) {
      message("No missing months\n")
    }
  } else {
    warning("Months missing in the date range:", paste(missing_months, collapse = ", "), "\n")
  }

  ########## Return Save Path ##########
  # Return the folder path where all mbox files were saved.
  return(save_folder_path)
}


############## Mod Mbox Refresher ##############

#' Refresh mbox files downloaded via mod_mbox
#'
#' This function refreshes the mailing list files by checking the contents of a specified folder.
#' If the folder is empty, it calls \code{\link{download_mod_mbox}} to download all mod_mbox files from start_year_month to the current month.
#' If the folder contains already-downloaded mbox files, it identifies the most recent month, deletes that file, and redownloads it
#' along with all future months up to the current real-life month.
#'
#' The naming convention of files is `kaiaulu_YYYYMM.mbox`, and the function uses this pattern to identify the most recent month.
#' After deleting the most recent file, the function ensures that the month is redownloaded, along with all subsequent months up to the current month.
#' Redownloading the most recent file ensures any files added in that month after the latest refresh are included.
#'
#' @param mailing_list The URL of the mailing list being downloaded (e.g., \url{https://lists.apache.org/list.html?announce@apache.org})
#' @param start_year_month The year and month of the first file to be downloaded (format: 'YYYYMM').
#' @param save_folder_path The folder path in which all the downloaded mod_mbox files will be stored.
#' @param verbose if TRUE, prints diagnostic messages.
#' @return Returns `downloaded_files`, a vector of the newly downloaded files in the current working directory.
#' @export
refresh_mod_mbox <- function(mailing_list, start_year_month, save_folder_path, verbose = TRUE) {

  ########## Check if Folder is Empty ##########
  # Check the contents of the folder to see if any .mbox files are already present.
  # The function looks for files that match the naming pattern 'kaiaulu_YYYYMM.mbox'
  files_in_folder <- list.files(save_folder_path, pattern = "kaiaulu_\\d{6}\\.mbox$")

  if (length(files_in_folder) == 0) {
    # If the folder is empty, download all mod_mbox files starting from start_year_month
    # The end date is set to the current month based on the system date
    end_year_month <- format(Sys.Date(), "%Y%m")
    if (verbose) message("Folder is empty. Downloading from", start_year_month, "to", end_year_month, "\n")

    # Call the download_mod_mbox function to download files from start_year_month to end_year_month
    download_mod_mbox(mailing_list, start_year_month, end_year_month, save_folder_path, verbose = verbose)
  }
  ########## Identify the Most Recent Month ##########
  else {
    # If the folder is not empty, identify the most recent month based on the filenames
    # The filenames follow the pattern 'kaiaulu_YYYYMM.mbox', so we extract the YYYYMM part of the filenames
    year_months <- gsub("kaiaulu_(\\d{6})\\.mbox$", "\\1", files_in_folder)

    # Find the most recent month by taking the maximum of the extracted YYYYMM values
    recent_month <- max(year_months)

    # Delete the most recent file before redownloading it
    recent_file <- file.path(save_folder_path, stringi::stri_c("kaiaulu_", recent_month, ".mbox"))
    if (file.exists(recent_file)) {
      file.remove(recent_file)
      if (verbose) message("Deleted the most recent file:", recent_file, "\n")
    }

    ########## Redownload from the Most Recent Month ##########
    # Set the end_year_month to the current month (based on the system date)
    end_year_month <- format(Sys.Date(), "%Y%m")

    # Redownload files from the most recent month (that was just deleted) to the current month
    if (verbose) message("Redownloading from", recent_month, "to", end_year_month, "\n")

    # Call the download_mod_mbox function to redownload the deleted month and all subsequent months up to the current month
    download_mod_mbox(mailing_list, recent_month, end_year_month, save_folder_path, verbose = verbose)
  }
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
#' @param mbox_file_path path to mbox archive file (ends in .mbox)
#' @export
#' @family parsers
#' @param perceval_path path to perceval binary
#' @param mbox_file_path path to mbox archive file (ends in .mbox)
#' @export
#' @family parsers
parse_mbox <- function(perceval_path, mbox_file_path) {
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  perceval_path <- path.expand(perceval_path)
  mbox_file_path <- path.expand(mbox_file_path)

  # Remove ".mbox"
  mbox_uri <- stri_replace_last(mbox_file_path,replacement="",regex=".mbox")

  # Use percerval to parse mbox_path. --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- tryCatch({
    system2(perceval_path,
            args = c('mbox', mbox_uri, mbox_file_path, '--json-line'),
            stdout = TRUE,
            stderr = FALSE)
  }, error = function(e) {
    #print("Error running Perceval:")
    #print(e$message)
    stop("Perceval execution failed.")
  })

  # Filter JSON lines from Perceval output
  json_lines <- perceval_output[grepl("^\\{", perceval_output)]  # Escape the `{` character


  if (length(json_lines) == 0) {
    stop("No valid JSON lines found in Perceval output. Check the mbox file or Perceval configuration.")
  }

  # Parse JSON output as a data.table
  perceval_parsed <- tryCatch({
    # Parsed JSON output as a data.table.
    data.table(jsonlite::stream_in(textConnection(perceval_output),verbose=FALSE))
  }, error = function(e) {
    #print(e$message)
    stop("JSON parsing failed.")
  })


  columns_of_interest <- c("data.Message.ID", "data.In.Reply.To", "data.Date", "data.From", "data.To", "data.Cc", "data.Subject", "data.body.plain", "data.body")
  columns_rename <- c("reply_id", "in_reply_to_id", "reply_datetimetz", "reply_from", "reply_to", "reply_cc", "reply_subject", "reply_body", "reply_body")
  is_available_column <- columns_of_interest %in% colnames(perceval_parsed)

  columns_of_interest <- columns_of_interest[is_available_column]

  perceval_parsed <- perceval_parsed[, ..columns_of_interest]

  data.table::setnames(x = perceval_parsed,
                       old = colnames(perceval_parsed),
                       new = columns_rename[is_available_column])

  return(perceval_parsed)
}

#' Parse mbox latest date
#'
#' This function returns the name of the latest mod_mbox file downloaded in the specified folder
#' based on the naming convention `kaiaulu_YYYYMM.mbox`. For example: `kaiaulu_202401.mbox`.
#'
#' @param save_folder_path path to the folder containing the mbox files
#' @return `latest_mbox_file` the name of the latest mod_mbox file
#' @export
#' @family parsers
parse_mbox_latest_date <- function(save_folder_path) {
  # List all .mbox files in the folder with the expected naming pattern
  file_list <- list.files(save_folder_path, pattern = "kaiaulu_\\d{6}\\.mbox$")

  if (length(file_list) == 0) {
    warning("No .mbox files found in the folder.")
    return(invisible(NULL))
  }

  # Extract the dates from the filenames
  date_list <- sub("kaiaulu_(\\d{6})\\.mbox$", "\\1", file_list)

  # Convert dates to numeric for comparison
  date_numeric <- as.numeric(date_list)

  # Find the latest date
  latest_date <- max(date_numeric, na.rm = TRUE)

  # Find the file corresponding to the latest date
  latest_mbox_file <- file_list[date_numeric == latest_date]

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

 reply_from_full_info <- stringi::stri_c(reply_from_author, " <", reply_from_email, ">")
 reply_to_full_info <- stringi::stri_c(reply_to_author, " <", reply_to_email, ">")
 reply_cc_full_info <- stringi::stri_c(reply_cc_author, " <", reply_cc_email, ">")


  mbox_content <- stringi::stri_c(
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
    "\nX-Mailing-List: ", stringi::stri_c(mailing_list, "@example.com"),
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
  mbox_filepath <- file.path(folder_path, stringi::stri_c(file_name, ".mbox"))

  # Write the mbox content
  mbox_body <- stringi::stri_c(replies, collapse = "\n\n")
  io_make_file(mbox_filepath, mbox_body)

  return(mbox_filepath)
}
