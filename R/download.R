#' Download all pipermail files in an archive
#' @param url An url pointing to a pipermail archive
#' @return Returns `destination`, a vector of the downloaded files in the current working directory
#' @export
download_pipermail <- function(url) {

  #Get page
  pagedata <- httr::GET(url)

  #Parse html file into object
  tbls_xml <- XML::htmlParse(pagedata)

  #Exctract href tablenodes from html table
  tableNodes <- XML::getNodeSet(tbls_xml, "//td/a[@href]")

  #Exctract filenames from tablenode content with xmlGetAtrr
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
#' @param mailinglist Name of the project mailing list (e.g. apr-dev) in the mod_mbox directory
#' @param from First year in the range to be downloaded
#' @param to Last year in the range to be downloaded
#' @return Returns `output`, the name of the resulting .mbox file in the current working directory
#' @export
download_mod_mbox <- function(base_url, mailinglist, from, to) {

  #Initialize variables
  counter <- 0
  destination <- list()

  #Open file handle to output file
  output <- sprintf("%s.mbox", mailinglist)
  fileConn <- file(output, "w+")

  #Loop through time and compose the mbox file
  for (year in (from:to)) {

    for (month in 1:12) {
      counter <- counter + 1

      #Generate file destinations
      destination[[counter]] <- sprintf("%d%02d.mbox", year, month)

      #Try file download and save result
      x <- httr::GET(paste(base_url, mailinglist, destination[[counter]], sep = "/"), httr::write_disk(destination[[counter]], overwrite=TRUE))

      #If download was successful, write to mbox file, if not, delete file
      if (httr::http_error(x) == FALSE) {

        #Open read connection
        readCon <- file(destination[[counter]], "r")

        data <- readLines(destination[[counter]])

        #Write data to output
        writeLines(data, fileConn)

        #Close read connection
        close(readCon)
      }

      #Delete the file
      unlink(destination[[counter]], force = TRUE)

    }

  }

  #Close connection to target mbox file
  close(fileConn)

  #return output location
  return(output)
}
