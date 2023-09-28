# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Create Temporary File
#' @param content Textual content to be added to the file
#' @param extension File extension (e.g. .c, .txt, .md, etc)
#' @export
make_temporary_file <- function(content,extension) {
  temp <- tempfile(fileext = extension)
  #on.exit(unlink(temp))
  writeLines(content, con = temp)
  return(temp)
}
#' Read Temporary File
#' @param temp A temporary file created by \code{make_temporary_file}
#' @export
read_temporary_file <- function(temp){
  return(readLines(temp))
}

#' This function creates a sample file
#'
#' @param save_filepath The path of the file that is being saved
#' @param body The contents of the file
#' @return file with specified path and body
#' @export
io_make_sample_file <- function(save_filepath,body) {

  error <- system2('echo',
                   args = c("\"print('hello world!')\"",
                            '>',
                            file_path),
                   stdout = TRUE,
                   stderr = FALSE)

  # Create cmd
  cmd <- paste("echo", shQuote(body), ">", shQuote(save_filepath))

  # Execute the command using system2
  result <- system2(cmd, stdout = TRUE, stderr = FALSE)


}

#' This function creates a new folder
#'
#' @param folderName Name of the folder you are creating
#' @param body The contents of the folder, what files it will consist of
#' @return The new folder with its updated contents
#' @export
io_create_folder <- function(folderName, body) {

  file_path <- file.path(folder_path,"hello.R")

  #echo "print('hello world!')" >  path/to/folder/hello.R
  error <- system2('echo',
                   args = c("\"print('hello world!')\"",
                            '>',
                            file_path),
                   stdout = TRUE,
                   stderr = FALSE)

  # git init path/to/folder
  error <- system2('git',
                   args = c('init',
                            folder_path),
                   stdout = TRUE,
                   stderr = FALSE)

}




