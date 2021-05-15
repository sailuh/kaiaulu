library(httr)
library(utils)
library(stringr)
library(XML)

# Scrape the urls from a web page and return a list of downloadable urls
get_pipermail_archives <- function(url) {

  #Parse html file into object
  tbls_xml <- htmlParse(url)
  
  #Exctract href tablenodes from html table
  tableNodes <- getNodeSet(tbls_xml, "//td/a[@href]")
  
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
    splits <- str_split(i, "/")
    destination[[i]] <- paste0( splits[[1]][[length(splits[[1]])]])
    
    #download file and place it at the destination
    download.file(i, destination[[i]])
  }
  
  #Return filenames
  return(destination)
  
}

#at to @ replace function
pipermail_atreplacer <- function(string) {
  
  rstring <- sub(" at ", "@", string)
  
  return(rstring)
  
}

#Convert files to mbox format
pipermail_mbox_conversion <- function(filelist) {
  
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



pipermail_to_mbox <- function(url) {
  
  files <- get_pipermail_archives(url)
  
  output <- pipermail_mbox_conversion(files)
  
  return(output)
  
}



pipermail_to_mbox("http://lists.openstack.org/pipermail/openstack-announce/")



