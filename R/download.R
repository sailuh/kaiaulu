# Scrape the urls from a web page and return a list of downloadable urls
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

#Convert files to mbox format
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