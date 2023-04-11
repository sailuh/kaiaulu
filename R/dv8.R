# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Transforms a DV8 binary cluster file to a json cluster file.
#'
#' Converts a clustering binary in the dv8 format *-clsx.dv8-cslx into a
#'  *-clsx.json.
#'
#' @param dv8_path path to dv8 binary
#' @param clsxb_path path to clsx to convert
#' @param clsxj_path path to save output file
#' @export
#' @family dv8
dv8_clsxb_to_clsxj <- function(dv8_path, clsxb_path, clsxj_path){
  system2(dv8_path, args=c('core:export-cluster', '-outputFile', clsxj_path, clsxb_path), stdout=FALSE, stderr=FALSE)
}

#' Parse a DV8 json cluster file to R data.table.
#'
#' Parses a *-clsx.json into a data table.
#'
#' @param clsxj_path path to clustering json (obtained from dv8_clsxb_to_clsxj)
#' @export
#' @family parsers
#' @family dv8
parse_dv8_clusters <- function(clsxj_path){
  # Read in the clustering file
  dv8_clusters_json <- jsonlite::read_json(clsxj_path)

  # Get number of cluster layers in the json
  num_layers <- length(dv8_clusters_json$structure)

  # The structure field holds the relevant data
  structure <- dv8_clusters_json[["structure"]]

  # Will hold each row (a list of the current layer, module, and file) for the data table
  cluster_list <- list()

  # Iterate through each cluster
  for (i in 1: num_layers) {
    # Get the layer number (L0, L1, L2, ...)
    layer_i <- structure[[i]]$name

    # Get the number of modules for the current layer
    num_modules <- length(structure[[i]]$nested)

    # Iterate through each module
    for (j in 1: num_modules) {
      # Get the module number (M0, M1, M2, ...)
      LM_name <- stringr::str_split(structure[[i]]$nested[[j]]$name, "/")
      module_j <- LM_name[[1]][2]

      # Get number of files for the layer/module pair
      num_files_per_LM <- length(structure[[i]]$nested[[j]]$nested)

      # Iterate through each file
      for (k in 1: num_files_per_LM) {
        # Get the file name
        filename_k <- structure[[i]]$nested[[j]]$nested[[k]]$name

        # Represents a row in the table with the layer #, module #, and filename
        row_ijk <- list(filename_k, module_j, layer_i)

        # Append current row (made up of the layer number, module number, & filename) to a list
        cluster_list <- append(cluster_list, list(row_ijk))
      }
    }
  }

  # Create table from list of layer,module,file rows
  cluster_parsed <- data.table::rbindlist(cluster_list)

  # Rename result columns
  cluster_parsed <- data.table::setnames(cluster_parsed, c("V1", "V2", "V3"), c("filename", "module", "layer"))
  # Return parsed cluster data table
  return(cluster_parsed)
}

#' Convert dependencies table to a sdsm.json
#'
#' Converts table of dependencies from \code{\link{parse_dependencies}} into an *-sdsm.json binary.
#' In the sdsm.json, the Variables are all files/methods or any variables under analysis
#' (rows/columns in dependency matrix) and the Cells (matrix cell) contain all the relations of
#'  variable (src & dest) pairs.
#'
#' @param depends_table parsed dependencies table
#' @param project_name project name for the sdsm.json
#' @param is_sorted whether the json is sorted by src and dest
#' @export
#' @family dv8
#' @seealso \code{\link{parse_dependencies}}
dependencies_to_sdsmj <- function(depends_table, project_name, is_sorted=FALSE){

  # Get the files in the src and dest columns
  depends_files_src <- unique(list(depends_table$src)[[1]])
  depends_files_dest <- unique(list(depends_table$dest)[[1]])
  depends_files <- c(depends_files_src, depends_files_dest)

  # Get unique file names
  depends_files <- unique(depends_files)

  # Sort the file names
  variables <- sort(depends_files, method="radix")

  # Make indices for the filenames
  variables_indices <- 1:length(variables)
  # Make named list with filenames as keys and indices as values
  names(variables_indices) <- variables

  # List to hold cells' indices
  cells_indices <- 1:nrow(depends_table)


  # get the cell from each row in the depends table
  getCell <- function(i){
    depends_src <- depends_table[["src"]][[i]]
    depends_dest <- depends_table[["dest"]][[i]]

    # Look at first value in parse_depends table, get matching index in vars array
    src_index <- variables_indices[depends_src] - 1
    dest_index <- variables_indices[depends_dest] - 1

    values = list()
    parameters = list("Call", "Import", "Use", "Parameter", "Contain", "Create", "Extend",
                      "Return","Implement", "Cast", "Throw", "Annotation")

    # Get all the parameter values for a specific cell
    for (j in 1: length(parameters)) {
      current_param <- depends_table[i][[parameters[[j]]]]
      if (current_param > 0){
        values[[parameters[[j]]]] <- current_param
      }
    }

    return(list(src=src_index, dest=dest_index, values=data.frame(values)))
  }

  # Get the cells
  cells <- lapply(cells_indices, getCell)
  cells_df <- data.table::data.table(jsonlite::fromJSON(jsonlite::toJSON(cells, auto_unbox = TRUE)))

  # Sort by the src_index and dest_index if is_sorted is true
  if (is_sorted==TRUE){
    data.table::setorder(cells_df, cols = "src", "dest")
  }

  sdsm_json <- list(schemaVersion="1.0", name=paste0(project_name, "-sdsm"), variables=variables, cells=cells_df)

  json_df <- jsonlite::fromJSON(jsonlite::toJSON(sdsm_json, auto_unbox = TRUE))

  # Unbox each of the cell values (should be values: object, not values: [object])
  json_df$cells$values <- lapply(json_df$cells$values, jsonlite::unbox)

  # Save the json to a file
  jsonlite::write_json(json_df, paste0("../rawdata/dv8/", project_name, ".json"), auto_unbox=TRUE)
}

#' Convert gitlog table to a hdsm.json
#'
#' Converts a gitlog table into an *-hdsm.json binary.
#' In the hdsm.json, the Variables are all files/methods or any variables under analysis
#' (rows/columns in dependency matrix) and the Cells (matrix cell) contain all the relations of
#'  variable (src & dest) pairs. The Cochange is the number of times the src & dest
#'   were committed together.
#'
#' @param gitlog_table parsed gitlog table
#' @param project_name project name for the hdsm.json
#' @param is_sorted whether the json is sorted by src and dest
#' @export
#' @family dv8
gitlog_to_hdsmj <- function(gitlog_table, project_name, is_sorted=FALSE){
  # Call preliminary functions to get graph and cochange for the files
  gitlog_graph <- transform_gitlog_to_bipartite_network(gitlog_table, mode ="commit-file")
  cochange_table <- bipartite_graph_projection(gitlog_graph, mode = FALSE, is_intermediate_projection = FALSE)

  # List of two tables. 2nd table had the filenames and cochange
  cochange_table <- cochange_table[[2]]

  # Get the files in the from (src) and to (dest) columns, concatenate them into one list, sort alphabetically
  # Getting files in the "from" column
  gitlog_files_src <- unique(cochange_table[["from"]])
  # Getting files in the "to" column
  gitlog_files_dest <- unique(cochange_table[["to"]])
  # Combining src and dest columns
  gitlog_files <- c(gitlog_files_src, gitlog_files_dest)

  # Taking the unique combined columns
  variables <- unique(gitlog_files)

  # Sort the file names
  variables <- sort(variables, method="radix")
  # Make indices for the file names
  variables_indices <- 1:length(variables)
  # Make named list with file names as keys and indices as values
  names(variables_indices) <- variables

  # List to hold cells
  cells_indices <- 1:nrow(cochange_table)

  getCell <- function(i) {
    cochange_src <- cochange_table[["from"]][[i]]
    cochange_dest <- cochange_table[["to"]][[i]]

    src_index <- variables_indices[cochange_src] - 1
    dest_index <- variables_indices[cochange_dest] - 1

    # Add Cochange parameter for the values field in the json
    Cochange  <- cochange_table[["weight"]][[i]]

    # Add the cells field items for index i of the sdsm_json based on the data
    # in the current parsed_gitlog row
    return(list(src=src_index, dest=dest_index, values=data.frame(Cochange)))
  }

  # Sorted list
  cells <- lapply(cells_indices, getCell)
  cells_df <- data.table::data.table(jsonlite::fromJSON(jsonlite::toJSON(cells, auto_unbox = TRUE)))

  if (is_sorted == TRUE){
    data.table::setorder(cells_df, cols = "src", "dest")
  }

  # Create the final json
  hdsm_json <- list(schemaVersion="1.0", name=paste0(project_name, "-hdsm"), variables=variables, cells=cells_df)

  json_df <- jsonlite::fromJSON(jsonlite::toJSON(hdsm_json, auto_unbox = TRUE))

  # Unbox each of the cell values (should be values: object, not values: [object])
  json_df$cells$values <- lapply(json_df$cells$values, jsonlite::unbox)

  # Save the json to a file
  jsonlite::write_json(json_df, paste0("../rawdata/dv8/", project_name, ".json"), auto_unbox=TRUE)
}
