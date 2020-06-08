# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @export
parse_rfile_ast <- function(filepath){
  parsed_file <- parse(path.expand(filepath))
  return(data.table(getParseData(parsed_file, includeText = TRUE)))
}
#' @export
parse_r_function_definition <-function(parsed_r_file){
  parsed_r_file$row_id <- 1:nrow(parsed_r_file)
  # Obtain function definitions
  src_line_functions_start <- parsed_r_file[token == "FUNCTION"]$line1

  # src_line_function_end is not on of parsed_r_file[token == "FUNCTION"]$line2.
  # but rather on metadata_line_function_start$line2.

  get_metadata_first_row_id_where <- function(src_line_function_start,parsed_r_file){
    return(which(src_line_function_start == parsed_r_file$line1)[1])
  }
  # First ocurrence of src_line_function_start in $line1
  metadata_line_functions_start <- sapply(src_line_functions_start,get_metadata_first_row_id_where,parsed_r_file)
  # Before obtaining the ending line of each function, let's filter out anonymous functions
  # Non-anonymous function have parsed_r_file[metadata_line_function_start + 3]$token == "LEFT_ASSIGN"
  which_metadata_line_functions_is_non_anonymous <- parsed_r_file[metadata_line_functions_start + 3]$token == "LEFT_ASSIGN"
  metadata_line_functions_start <- metadata_line_functions_start[which_metadata_line_functions_is_non_anonymous]
  # We also filter the src_line_functions_start for consistency
  src_line_functions_start <- src_line_functions_start[which_metadata_line_functions_is_non_anonymous]
  # Now we obtain the metadata end line of the functions we care about
  src_line_functions_end <- parsed_r_file[metadata_line_functions_start]$line2
  # Now we repeat to src_line_function_end the logic we used for src_line_function_start
  # to obtain it's end on the metadata
  metadata_line_functions_end <- sapply(src_line_functions_end,get_metadata_first_row_id_where,parsed_r_file)

  # Function names are metadata_line_function_start + 1
  src_functions_name <- parsed_r_file[metadata_line_functions_start + 1]$text

  # Collects all the indices above
  function_definition <- data.table(src_functions_name,
                                    src_line_functions_start,
                                    src_line_functions_end,
                                    metadata_line_functions_start,
                                    metadata_line_functions_end
                                    #filepath_r
                                    )

  return(function_definition)
}
#' @export
parse_r_function_dependencies <- function(parsed_r_file,function_definition){
  # Get metadata of function calls
  src_line_functions_call_start <- parsed_r_file[token == "SYMBOL_FUNCTION_CALL"]$line1
  src_line_functions_call_end <- parsed_r_file[token == "SYMBOL_FUNCTION_CALL"]$line2
  src_functions_call_name <- parsed_r_file[token == "SYMBOL_FUNCTION_CALL"]$text

  # Use function_definitions src starting line and call position to find ownerships
  get_function_it_belongs_to <- function(src_line_function_call,src_line_functions_start){
    function_it_belongs_to <- tail(which(src_line_function_call > src_line_functions_start),1)
    return(ifelse(length(function_it_belongs_to) == 0,NA,function_it_belongs_to))
  }
  # An edge case is library() calls in an R file.
  # sapply will return a list instead of a vector,if not handled properly.
  # in essence, the which below will return integer(0) since it can't find a function.
  # the ifelse handles that
  function_it_belongs_to <- sapply(src_line_functions_call_start,
                                   get_function_it_belongs_to,
                                   function_definition$src_line_functions_start)

  src_functions_call_owner_name <- function_definition[function_it_belongs_to]$src_functions_name
  # Use function call metadata and function definition ownership names to construct edgelist of all calls
  # note src_functions_call_owner_name will have value <NA> if the function call does not have
  # a function owner
  function_call_edgelist <- data.table(src_functions_call_name,
                                       src_functions_call_owner_name,
                                       src_line_functions_call_start,
                                       src_line_functions_call_end)
  return(function_call_edgelist)
}
