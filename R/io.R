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
