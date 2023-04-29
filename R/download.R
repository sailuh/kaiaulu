#' Download project data (issues and comments) from bugzilla site
#' Note: The first comment in every issue is the issue description
#' @param bugzilla_site URL to specific bugzilla site
#' @param start_timestamp when to start bug retrieval (ex. 2023-01-01T00:14:57Z)
#' @param save_folder_path the full *folder* path where the bugzilla issues will be stored
#' @param limit_upperbound the number of issues saved in each page file. Some bugzilla sites have limits set on how many bugs
#' can be retrieved in one GET request, in which case, the limit set by the bugzilla site will be used in place of
#' limit_upperbound to ensure full bug retrieval.
#' @seealso \code{\link{parse_bugzilla_rest_issues}} a parser function to parse Bugzilla issues data
#' @seealso \code{\link{parse_bugzilla_rest_comments}} a parser function to parse Bugzilla comments data
#' @export
download_bugzilla_rest_issues_comments <- function(bugzilla_site, start_timestamp, save_folder_path, limit_upperbound = 500) {
  # Format link to retrieve data using Bugzilla REST API
  bugzilla_site <- paste(bugzilla_site, "/rest", sep="")

  # Make sure folder path is correctly formatted
  if (stringi::stri_sub(save_folder_path,-1) != "/"){
    save_folder_path <-paste0(save_folder_path, "/")
  }

  # Defines what bug to start from in bugs retrieved.
  offset <- 0
  # Defines name of the file. Each page contains 500 bugs.
  page <- 0
  # Defines the limit.
  limit <- limit_upperbound
  # Initialize to keep request or not
  keep_request <- TRUE

  while(keep_request){
    # Get request to get the project data
    issues <- httr::GET(paste0(bugzilla_site, "/bug", "?creation_time=", start_timestamp, "&include_fields=_default,comments", "&limit=", limit, "&offset=", offset))

    # Check if the limit being restrict or not
    if(as.integer(httr::content(issues)$limit) != limit){
      limit <- as.integer(httr::content(issues)$limit)
    }

    # Check if there is any issue created after specific date
    if(httr::content(issues)$total_matches > 0){
      issues_content <- httr::content(issues, "text")
      issues_content <- jsonlite::fromJSON(issues_content)
      jsonlite::write_json(issues_content, file.path(save_folder_path, paste0(page, ".json")), auto_unbox = TRUE)
      page <- page + 1
      offset <- offset + limit
    } else{
      keep_request <- FALSE
    }
  }
}


#' Download all pipermail files in an archive
#' @param url An url pointing to a pipermail archive
#' @return Returns `destination`, a vector of the downloaded files in the current working directory
#' @export
download_pipermail <- function(url) {

  #Get page
  pagedata <- httr::GET(url)

  #Parse html file into object
  tbls_xml <- XML::htmlParse(pagedata)

  #Extract href tablenodes from html table
  tableNodes <- XML::getNodeSet(tbls_xml, "//td/a[@href]")

  #Extract filenames from tablenode content with xmlGetAtrr
  hrefs <- sapply(tableNodes, XML::xmlGetAttr, 'href')

  #Create Vector
  files <- vector()

  #Compose download urls for both gunzipped and plain text files
  for (i in hrefs ){
    if (endsWith(i, ".txt.gz")){
          i <- paste0(url, i)
          files <- c(files, i)
    } else if (endsWith(i, ".txt")) {
          i <- paste0(url, i)
          files <- c(files, i)
    }
  }

  destination <- vector()
  # File downloading loop
  for (i in files){

    #split filename from url and create download destination out of it
    splits <- stringi::stri_split_fixed(i, "/")
    destination[[i]] <- paste0(splits[[1]][[length(splits[[1]])]])

    #download file and place it at the destination
    httr::GET(i, httr::write_disk(destination[[i]], overwrite=TRUE))
  }

  #Return filenames
  return(destination)

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
#' @param base_url An url pointing to the mod_mbox directory (e.g. "http://mail-archives.apache.org/mod_mbox") without trailing slashes
#' @param mailing_list Name of the project mailing list (e.g. apr-dev) in the mod_mbox directory
#' @param from_year First year in the range to be downloaded
#' @param to_year Last year in the range to be downloaded
#' @param save_folder_path the full *folder* path where the monthly downloaded mbox will be stored.
#' @param verbose Prints progress during execution
#' @return Returns the path of the downloaded mbox file.
#' @export
download_mod_mbox_per_month <- function(base_url, mailing_list, from_year, to_year, save_folder_path,verbose=FALSE) {


  #Initialize variables
  counter <- 0
  destination <- list()

  #Open file handle to output file
  output <- path.expand(save_folder_path)

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
      full_tmp_save_path <- file.path(output,destination[[counter]])
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
