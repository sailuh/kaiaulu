# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Make Temporary File
#'
#' Creates a temporary file
#'
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
#'
#' Read a temporary file.
#'
#' @param temp A temporary file created by \code{make_temporary_file}
#' @export
read_temporary_file <- function(temp){
  return(readLines(temp))
}

#' Make File
#'
#' This function creates a local file on disk with
#' the specified content.
#'
#' @param file_path The path of the file that is being saved
#' @param body The contents of the file
#' @return file with specified path and body
#' @export
io_make_file <- function(file_path, body) {

  error <- system2('echo',
                   args = c(shQuote(body),
                            '>',
                            file_path),
                   stdout = TRUE,
                   stderr = FALSE)

}

#' Make Folder
#'
#' Creates a new folder on disk.
#'
#' @param folder_name Name of newly created folder
#' @param folder_path Path of folder where new folder will be, default set to /tmp
#' @return The new folder_path with its updated contents
#' @export
io_make_folder <- function(folder_path="/tmp", folder_name) {
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  folder_path <- path.expand(folder_path)
  folder_path <- file.path(folder_path, folder_name)

  #mkdir path/to/folder/sample
  error <- system2('mkdir',
                   args = c(folder_path),
                   stdout = TRUE,
                   stderr = FALSE)

  return(folder_path)
}


#' Delete Folder
#'
#' Deletes a folder on disk.
#'
#' @param folder_path Path of folder where the folder to be deleted is. Default set to /tmp
#' @param folder_name Name of folder to be deleted
#' @return The deleted folder_path
#' @export
io_delete_folder <- function(folder_path="/tmp",folder_name) {
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  folder_path <- path.expand(folder_path)
  folder_path <- file.path(folder_path, folder_name)

  #mkdir path/to/folder/sample
  error <- system2('rm',
                   args = c('-r',
                            folder_path),
                   stdout = TRUE,
                   stderr = FALSE)

  return(folder_path)
}



