#' Download all pipermail files in an archive
#' @param url An url pointing to a pipermail archive
#' @return Returns `destination`, a vector of the downloaded files in the current working directory
#' @export 
download_pipermail <- function(url) {

  #Parse html file into object
  tbls_xml <- XML::htmlParse(url)
  
  #Exctract href tablenodes from html table
  tableNodes <- XML::getNodeSet(tbls_xml, "//td/a[@href]")
  
  #Exctract filenames from tablenode content with xmlGetAtrr
  hrefs <- sapply(tableNodes, xmlGetAttr, 'href')

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
    httr::GET(i, write_disk(destination[[i]], overwrite=TRUE))
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
  for (i in filelist[]){
    
    readCon <- file(i, "r")
  
    data <- readLines(i)
    
    #Find email headers to send to 'at' to @ replacer
    for (i in 1:length(data)) {
      
      data[i] <- sub("From:? \\S+ at \\S+", pipermail_atreplacer(data[i]), data[i])
      
    }
    
    #Write files to output
    writeLines(data, fileConn)
    
    #Close read connection
    close(readCon)
    
    #Delete the file
    unlink(i)
  }
  
  #Close connection to mbox file
  close(fileConn)
  
  #return output location
  return(output)
}