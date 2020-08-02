# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @export
make_temporary_file <- function(content,extension) {
  temp <- tempfile(fileext = extension)
  #on.exit(unlink(temp))
  writeLines(content, con = temp)
  return(temp)
}
#' @export
read_temporary_file <- function(temp){
  return(readLines(temp))
}
