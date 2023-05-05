# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Creates srcML XML
#'
#' Parses src code zip, folder or file and outputs the annotated
#' XML representation of the file.
#'
#' @param srcml_path The path to srcML binary
#' @param src_folder The path to the source code zip, folder or file of analysis
#' @param srcml_filepath The path, filename and extension (.xml) of the output XML file.
#'
#' @return The path where the output xml was saved (i.e. srcml_filepath)
#' @references For details, see \url{https://www.srcml.org/tutorials/creating-srcml.html}.
#' @seealso \code{\link{query_src_text}} to query the output file.
#' @export
#'
annotate_src_text <- function(srcml_path,src_folder,srcml_filepath){
  srcml_path <- path.expand(srcml_path)
  src_folder <- path.expand(src_folder)
  srcml_filepath <- path.expand(srcml_filepath)

  srcml_output <- system2(srcml_path,
                             args = c(src_folder, '--output',srcml_filepath),
                             stdout = FALSE,
                             stderr = FALSE)

  return(srcml_filepath)
}


#' Query srcML XML
#'
#' Queries srcML XML for code units (e.g. function names, declarations, etc.).
#' For a list of code units and languages supported by srcML XML see:
#' \url{https://www.srcml.org/documentation.html}.
#'
#' Note a query, unless explicitly specified to be parsed (e.g. by using the
#' string() scrML function), is a srcML XML itself, which can be also queried.
#'
#' @param srcml_path The path to srcML binary
#' @param xpath_query The XPath query to be performed on the .xml
#' @param srcml_filepath The path to the srcML file to be queried
#' (see \code{\link{annotate_src_text}}).
#'
#' @return The path where the output xml was saved (i.e. srcml_filepath)
#' @references For details, see \url{https://www.srcml.org/tutorials/xpath-query.html}.
#' @export
query_src_text <- function(srcml_path,xpath_query,srcml_filepath){
  srcml_path <- path.expand(srcml_path)
  xpath_query <- path.expand(xpath_query)
  srcml_filepath <- path.expand(srcml_filepath)

  #srcml --xpath "//src:class/src:name" depends.xml

  srcml_output <- system2(srcml_path,
                          args = c('--xpath',paste0('"',xpath_query,'"'),
                                   srcml_filepath),
                          stdout = TRUE,
                          stderr = FALSE)

  return(srcml_output)
}

#' Query srcML Class Names
#'
#' This is a convenience function to parse class names out of a project.
#' \url{https://www.srcml.org/documentation.html}.
#'
#'
#' @param srcml_path The path to srcML binary
#' @param srcml_filepath The path to the srcML file to be queried
#' (see \code{\link{annotate_src_text}}).
#'
#' @return A data.table containing filepath and class name.
#' @references For details, see \url{https://www.srcml.org/documentation.html}.
#' @export
query_src_text_class_names <- function(srcml_path,srcml_filepath){
  srcml_path <- path.expand(srcml_path)
  srcml_filepath <- path.expand(srcml_filepath)

  xpath_query <- "//src:class/src:name"

  srcml_output <- query_src_text(srcml_path,xpath_query,srcml_filepath)

  srcml_output <- XML::xmlTreeParse(srcml_output)
  srcml_root <- XML::xmlRoot(srcml_output)

  # The children of the root node is a list of unit nodes
  srcml_class_names <- XML::xmlChildren(srcml_root)
  # Each unit node is of the form:
  # <unit revision="1.0.0" language="Java" filename="/path/to/file.java" item="2"><name>someClassName</name></unit>


  parse_filepath_and_class_name <- function(unit){
    # The class name is a child node of each node
    class_name <- XML::xmlValue(unit[[1]])
    # The attribute filename contains the filename the class belongs to
    filepath <- XML::xmlGetAttr(unit,"filename")
    return(data.table(filepath=filepath,classname=class_name))
  }
  dt_filepath_classname <- rbindlist(lapply(srcml_class_names,parse_filepath_and_class_name))

  return(dt_filepath_classname)

}
