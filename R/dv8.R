# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Transform parsed dependencies into a structural dsm.json file.
#'
#' Converts table of dependencies from \code{\link{parse_dependencies}} into an *-sdsm.json.
#' In the sdsm.json, the Variables are all files/methods or any variables under analysis
#' (rows/columns in dependency matrix) and the Cells (matrix cell) contain all the relations of
#'  variable (src & dest) pairs.
#'
#' @param project_dependencies A parsed depends project by \code{\link{parse_dependencies}}.
#' @param sdsmj_path the path to save the structural dsm (*-sdsm.json).
#' @param is_sorted whether to sort the variables (filenames) in the sdsm.json file (optional).
#' @export
#' @family edgelists
#' @family dv8
#' @seealso \code{\link{parse_dependencies}} to get a table of parsed dependencies needed as input into \code{\link{transform_dependencies_to_sdsmj}},
#' \code{\link{transform_gitlog_to_hdsmj}} to perform a similar transformation into a *-dsm.json using a gitlog,
#' \code{\link{transform_temporal_gitlog_to_adsmj}} to perform a similar transformation into a *-dsm.json using a temporal gitlog,
#' \code{\link{graph_to_dsmj}} to generate a *-dsm.json file.
transform_dependencies_to_sdsmj <- function(project_dependencies, sdsmj_path, is_sorted=FALSE){
  # Make copy of table to do changes
  project_depends <- copy(project_dependencies)

  # Convert table to long form
  project_depends[["edgelist"]] <- melt(project_depends[["edgelist"]],id.vars <- c("src_filepath","dest_filepath"), variable.name = "label")

  setnames(x=project_depends[["nodes"]], old = c("filepath"), new = c("name"))

  setnames(x=project_depends[["edgelist"]], old = c("src_filepath","dest_filepath", "value"),
           new = c("from","to", "weight"))

  # Put the weight column in front of the label column
  setcolorder(project_depends[["edgelist"]], c("from", "to", "weight", "label"))

  # This is a directed graph, so no duplication of edges
  graph_to_dsmj(project_depends, sdsmj_path, dsmj_name="sdsm", is_directed=TRUE, is_sorted)
}

#' Transform parsed git repo into a history dsm.json file.
#'
#' Converts a gitlog table into an *-hdsm.json.
#' In the hdsm.json, the Variables are all files/methods or any variables under analysis
#' (rows/columns in dependency matrix) and the Cells (matrix cell) contain all the relations of
#'  variable (src & dest) pairs. The Co-change is the number of times the src & dest were committed together.
#' Note that the co-change between a file and its renamed variant will not be considered
#' using this function, so those cells won't appear in the final *-hdsm.json.
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}.
#' @param hdsmj_path the path to save the history dsm (*-hdsm.json).
#' @param is_sorted whether to sort the variables (filenames) in the hdsm.json file (optional).
#' @export
#' @family edgelists
#' @family dv8
#' @seealso \code{\link{parse_gitlog}} to get a table of a parsed git project needed as input into \code{\link{transform_gitlog_to_hdsmj}},
#' \code{\link{transform_temporal_gitlog_to_adsmj}} to perform a similar transformation into a *-dsm.json using a temporal gitlog,
#' \code{\link{transform_dependencies_to_sdsmj}} to perform a similar transformation into a *-dsm.json using dependencies from Depends,
#' \code{\link{graph_to_dsmj}} to generate a *-dsm.json file.
transform_gitlog_to_hdsmj <- function(project_git, hdsmj_path, is_sorted=FALSE){
  # Call preliminary functions to get graph and cochange for the files
  git_bipartite <- transform_gitlog_to_bipartite_network(project_git, mode ="commit-file")
  cochange_table <- bipartite_graph_projection(git_bipartite, mode = FALSE,
                                               weight_scheme_function = weight_scheme_count_deleted_nodes)

  # Add label column with Cochange value
  cochange_table[["edgelist"]][["label"]] <- "Cochange"

  # This is an undirected graph, so there is duplication of edges
  graph_to_dsmj(cochange_table, hdsmj_path, dsmj_name="hdsm", is_directed=FALSE, is_sorted)
}

#' Transform parsed git repo into an author dsm.json file.
#'
#' Converts a temporal gitlog table into an *-adsm.json.
#' In the adsm.json, the Variables are all the authors under analysis
#' (rows/columns in dependency matrix) and the Cells (matrix cell) contain all the relations of
#'  variable (src & dest) pairs. The Collaborate value is the number of times the src author and dest author changed the same file.
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}.
#' @param adsmj_path the path to save the author dsm (*-adsm.json).
#' @param is_sorted whether to sort the variables (filenames) in the adsm.json file (optional).
#' @export
#' @family edgelists
#' @family dv8
#' @seealso \code{\link{parse_gitlog}} to get a table of a parsed git project needed as input into \code{\link{transform_gitlog_to_hdsmj}},
#' \code{\link{transform_gitlog_to_hdsmj}} to perform a similar transformation into a *-dsm.json using a gitlog,
#' \code{\link{transform_dependencies_to_sdsmj}} to perform a similar transformation into a *-dsm.json using dependencies from Depends,
#' \code{\link{graph_to_dsmj}} to generate a *-dsm.json file.
transform_temporal_gitlog_to_adsmj <- function(project_git, adsmj_path, is_sorted=FALSE){
  # Call preliminary functions to get graph and collaborators for the files
  author_table <- transform_gitlog_to_temporal_network(project_git, mode=c("author"))

  # Add label column with Collaborate value
  author_table[["edgelist"]][["label"]] <- "Collaborate"

  # This is a directed graph, so no duplication of edges
  graph_to_dsmj(author_table, adsmj_path, dsmj_name="adsm", is_directed=TRUE, is_sorted)
}

#' Transform parsed git repo into an edgelist
#'
#' @param project_git A parsed git project by \code{\link{parse_gitlog}}.
#' @param mode The network of interest: author-entity, committer-entity, commit-entity, author-committer
#' @export
#' @family edgelists
transform_gitlog_to_bipartite_network <- function(project_git, mode = c("author-file","committer-file","commit-file",'author-committer')){
  author_name_email <- author_datetimetz <- commit_hash <- committer_name_email <- committer_datetimetz <- lines_added <- lines_removed <- NULL # due to NSE notes in R CMD check
  # Check user did not specify a mode that does not exist
  mode <- match.arg(mode)
  # Select and rename relevant columns. Key = commit_hash.
  project_git <- project_git[,.(author=author_name_email,
                                author_date=author_datetimetz,
                                commit_hash=commit_hash,
                                committer=committer_name_email,
                                committer_date = committer_datetimetz,
                                file = file_pathname,
                                added = lines_added,
                                removed = lines_removed)]
  if(mode == "author-file"){
    git_graph <- model_directed_graph(project_git[,.(from=author,to=file)],
                                      is_bipartite=TRUE,
                                      color=c("black","#f4dbb5"))
  }else if(mode == "committer-file"){
    git_graph <- model_directed_graph(project_git[,.(from=committer,to=file)],
                                      is_bipartite=TRUE,
                                      color=c("#bed7be","#f4dbb5"))
  }else if(mode == "commit-file"){
    git_graph <- model_directed_graph(project_git[,.(from=commit_hash,to=file)],
                                      is_bipartite=TRUE,
                                      color=c("#afe569","#f4dbb5"))
  }else if(mode == "author-committer"){
    git_graph <- model_directed_graph(project_git[,.(from=author,to=committer)],
                                      is_bipartite=TRUE,
                                      color=c("black","#bed7be"))
  }
  return(git_graph)

}

#' Transforms a gitlog table to a historical DSM JSON file.
#'
#' Converts a gitlog table into an *-hdsm.json.
#' In the hdsm.json, the Variables are all files/methods or any variables under analysis
#' (rows/columns in dependency matrix) and the Cells (matrix cell) contain all the relations of
#'  variable (src & dest) pairs. The Co-change is the number of times the src & dest
#'   were committed together.
#'
#' @param project_gitlog parsed gitlog table created by \code{\link{parse_gitlog}}
#' @param hdsmj_path path to save output file
#' @param is_sorted whether the json is sorted by src and dest
#' @return the hdsmj_path
#' @export
#' @seealso \code{\link{dv8_dsmj_to_dsmb}} to convert to `*-hdsm.dv8-dsm` and
#' \code{\link{dv8_hdsmb_sdsmb_to_mdsmb}} to merge DSMs into `*-merge.dv8-dsm`.
#' @family dv8
gitlog_to_hdsmj <- function(project_gitlog, hdsmj_path, is_sorted=FALSE){
  # Call preliminary functions to get graph and cochange for the files
  gitlog_graph <- transform_gitlog_to_bipartite_network(project_gitlog, mode ="commit-file")
  cochange_table <- bipartite_graph_projection(gitlog_graph,
                                               mode = FALSE,
                                               weight_scheme_function = weight_scheme_count_deleted_nodes)

  # Get the nodes and edgelist tables
  nodes_table <- cochange_table[[1]]
  edgelist_table <- cochange_table[[2]]

  # Get and sort the file names
  variables <- sort(unique(nodes_table[["name"]]), method="radix")
  # Make indices for the file names
  variables_indices <- 1:length(variables)
  # Make named list with file names as keys and indices as values
  names(variables_indices) <- variables

  # List to hold cells
  cells_indices <- 1:nrow(edgelist_table)

  getCell <- function(i) {
    cochange_src <- edgelist_table[["from"]][[i]]
    cochange_dest <- edgelist_table[["to"]][[i]]

    src_index <- variables_indices[cochange_src] - 1
    dest_index <- variables_indices[cochange_dest] - 1

    # Add Cochange parameter for the values field in the json
    Cochange  <- edgelist_table[["weight"]][[i]]

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
  jsonlite::write_json(json_df, hdsmj_path, auto_unbox=TRUE)

  return(hdsmj_path)
}

#' Transforms a git log to a git numstat file.
#'
#' Comverts a git folder \*.git, to a gitlog numstat file \*.txt
#'
#' @param git_repo_path path to local project git repository
#' @param git_numstat_path path to save gitlog numstat
#'
#' @return the git_numstat_path
#' @export
#' @seealso \code{\link{dv8_gitnumstat_to_hdsmb}} to covert to `*-hdsm.dv8-dsm` and
#' \code{\link{dv8_hdsmb_sdsmb_to_mdsmb}} to merge DSMs into `*-merge.dv8-dsm`.
#'
dv8_gitlog_to_gitnumstat <- function(git_repo_path, git_numstat_path) {
  system2(command = "git",
          args = c("--git-dir",
                   git_repo_path,
                   "log",
                   "--numstat",
                   "--date=iso"),
          stdout = git_numstat_path
  )
  return(git_numstat_path)
}

#' Converts a git log text file into a hdsmb binary file
#'
#' An input file \*.txt is converted to \*.dv8-dsm
#'
#' @param dv8_path path to DV8 binary
#' @param git_numstat_path path to local git log text file obtained by \code{\link{dv8_gitlog_to_gitnumstat}}
#' @param max_cochange_count maximum count of co-changed files per commit, default 1000
#' @param hdsmb_path name of output hdsmb binary file, defaults to name of input
#' @param params_output_file output file used to record parameters
#'
#' @return the hdsmb_path
#' @export
#' @seealso \code{\link{dv8_hdsmb_sdsmb_to_mdsmb}} to merge DSMs into `*-merge.dv8-dsm`.
#'
dv8_gitnumstat_to_hdsmb <- function(dv8_path,
                                    git_numstat_path,
                                    hdsmb_path,
                                    max_cochange_count=1000,
                                    params_output_file="") {

  dv8_path <- path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s", hdsmb_path)
  #start_arg <- sprintf("-start %s", start)
  #stop_arg <- sprintf("-stop %s", stop)
  max_cochange_count_arg <- sprintf("-maxCochangeCount %i", max_cochange_count)
  params_output_file_arg <- ""

  if (params_output_file != "") {
    params_output_file_arg <- sprintf("-paramsOutputFile %s ", params_output_file)
  }

  system2(command=dv8_path,
          args=c("scm:history:gittxt:convert-matrix",
                 #                 start_arg,
                 #                 stop_arg,
                 output_file_arg,
                 #                 max_cochange_count_arg,
                 #                 params_output_file_arg,
                 git_numstat_path),
          stdout=TRUE)

  return(hdsmb_path)
}

#' Transforms a dependencies table to a structural DSM JSON file.
#'
#' Converts table of dependencies from \code{\link{parse_dependencies}} into an *-sdsm.json.
#' In the sdsm.json, the Variables are all files/methods or any variables under analysis
#' (rows/columns in dependency matrix) and the Cells (matrix cell) contain all the relations of
#'  variable (src & dest) pairs.
#'
#' @param project_dependencies parsed dependencies table created by \code{\link{parse_dependencies}}
#' @param sdsmj_path path to save output file
#' @param is_sorted whether the json is sorted by src and dest
#' @return the sdsmj_path
#' @export
#' @family dv8
#' @seealso \code{\link{dv8_dsmj_to_dsmb}} to convert to `*-sdsm.dv8-dsm` and
#' \code{\link{dv8_hdsmb_sdsmb_to_mdsmb}} to merge DSMs into `*-merge.dv8-dsm`.
dependencies_to_sdsmj <- function(project_dependencies, sdsmj_path, is_sorted=FALSE){

  # Get the nodes and edgelist tables
  nodes_table <- project_dependencies[["nodes"]]
  edgelist_table <- project_dependencies[["edgelist"]]

  # Get and sort the file names
  variables <- sort(unique(nodes_table[["filepath"]]), method="radix")

  # Make indices for the filenames
  variables_indices <- 1:length(variables)
  # Make named list with filenames as keys and indices as values
  names(variables_indices) <- variables

  # List to hold cells' indices
  cells_indices <- 1:nrow(edgelist_table)

  # Get parameters that will be used for the cells
  parameters <- list("Call", "Import", "Use", "Parameter", "Contain", "Create", "Extend",
                     "Return","Implement", "Cast", "Throw", "Annotation")
  # Only use parameters in the dataframe
  parameters <- unlist(intersect(colnames(edgelist_table), parameters))

  # get the cell from each row in the depends table
  getCell <- function(i){
    depends_src <- edgelist_table[["src_filepath"]][[i]]
    depends_dest <- edgelist_table[["dest_filepath"]][[i]]

    # Look at first value in parse_depends table, get matching index in vars array
    src_index <- variables_indices[depends_src] - 1
    dest_index <- variables_indices[depends_dest] - 1

    values <- list()
    # Get all the parameter values for a specific cell
    for (j in 1: length(parameters)) {
      current_param <- edgelist_table[i][[parameters[[j]]]]
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
  jsonlite::write_json(json_df,sdsmj_path, auto_unbox=TRUE)

  return(sdsmj_path)
}


#' Creates dependencies using Depends
#'
#' @param depends_jar_path path to depends jar
#' @param git_repo_path path to git repo (ends in .git)
#' @param language the language of the .git repo (accepts cpp, java, ruby, python, pom)
#' @param sdsmj_path the path to save the sdsm-dv8.dsm file.
#' @return the sdsmj_path
#' @export
#' @seealso \code{\link{dv8_dsmj_to_dsmb}} to convert to `*-sdsm.dv8-dsm` and
#' \code{\link{dv8_hdsmb_sdsmb_to_mdsmb}} to merge DSMs into `*-merge.dv8-dsm`.
#' @family dv8
dv8_depends_to_sdsmj <- function(depends_jar_path,git_repo_path,language,sdsmj_path) {
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  depends_jar_path <- path.expand(depends_jar_path)
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  src_folder_path <- stri_replace_last(git_repo_path,replacement="",regex=".git")

  sdsmj_path <- path.expand(sdsmj_path)
  split_sdsmj_path <- stri_split_regex(sdsmj_path,pattern="/")[[1]]
  sdsmj_folder_path <- stri_c(split_sdsmj_path[1:(length(split_sdsmj_path)-1)],collapse = "/")
  sdsmj_file_name <- split_sdsmj_path[length(split_sdsmj_path)]
  #depends auto adds .json. We should remove from user specified filepath.
  sdsmj_file_name <- stri_split_regex(sdsmj_file_name,pattern="\\.")[[1]][1]
  # Use Depends to parse the code folder.
  system2("java",
          args = c("-jar",depends_jar_path,
                   language,
                   src_folder_path,
                   sdsmj_file_name,
                   paste0('--dir=',sdsmj_folder_path),
                   '--auto-include',
                   '--granularity=file', '--namepattern=/',
                   '--format=json'),
          stdout = FALSE,
          stderr = FALSE)

  return(sdsmj_path)
}


#' Transforms a DV8 binary DSM file to a DSM JSON file.
#'
#' Converts a a hdsm.json \*(-hdsm.json) to history DSM (\*-hdsm.dv8-dsm),
#' a sdsm.json (\*-sdsm.json) to structural DSM (\*-sdsm.dv8-dsm) or a
#' a mdsm.json (\*-merge.json) to structural DSM (\*-merge.dv8-dsm).
#'
#' @param dv8_path path to dv8 binary
#' @param dsmj_path path to JSON DSM created by \code{\link{gitlog_to_hdsmj}},
#' \code{\link{dependencies_to_sdsmj}}, \code{\link{dv8_depends_to_sdsmj}}, or
#' \code{\link{dv8_dsmb_to_dsmj}}
#' @param dsmb_path path to save binary DSM
#' @return the dsmb_path
#' @export
#' @seealso \code{\link{dv8_hdsmb_sdsmb_to_mdsmb}} to merge DSMs into `*-merge.dv8-dsm`.
#' @family dv8
dv8_dsmj_to_dsmb <- function(dv8_path, dsmj_path, dsmb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  dsmj_path <- path.expand(dsmj_path)
  dsmb_path <- path.expand(dsmb_path)

  # Run system2 command
  system2(dv8_path, args=c('core:convert-matrix', '-outputFile', dsmb_path, dsmj_path), stdout=TRUE, stderr=FALSE)

  return(dsmb_path)
}


#' Export a dsmb binary file as a separate json file
#'
#' Takes a \*.dv8-dsm file and exports as \*.json file
#'
#' @param dv8_path path to DV8 binary
#' @param dsmb_path path to dsmb binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param dsmj_path path to json file
#' @return the dsmj_path
#' @export
#'
dv8_dsmb_to_dsmj <- function(dv8_path, dsmb_path, dsmj_path) {
  dv8_path = path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s", dsmj_path)

  system2(command=dv8_path,
          args=c("core:export-matrix",
                 output_file_arg,
                 dsmb_path),
          stdout=dsmj_path)

  return(dsmj_path)
}

#' Merge history DSM and structural DSM to merged DSM.
#'
#' A history DSM (\*-hdsm.dv8-dsm) and a structural DSM (\*-sdsm.dv8-dsm) are
#' merged to a merge DSM (*.dv8-dsm)
#'
#' @param dv8_path path to dv8 binary
#' @param hdsmb_path path to historical DSM binary
#' created by \code{\link{dv8_gitnumstat_to_hdsmb}} or (\code{\link{gitlog_to_hdsmj}} and \code{\link{dv8_dsmj_to_dsmb}}).
#' @param sdsmb_path path to structural DSM binary created by (\code{\link{dependencies_to_sdsmj}},
#' or \code{\link{dependencies_to_sdsmj}}) and \code{\link{dv8_depends_to_sdsmj}}
#' @param mdsmb_path path to save the merge DSM binary
#' @return the mdsmb_path
#' @export
#' @seealso \code{\link{dv8_mdsmb_drhier_to_excel}} to export a `*-merged.dv8-dsm` file to `merged.xlsx` for DSM visualization.
#' For analysis, see \code{\link{dv8_mdsmb_to_decoupling_level}} to calculate decoupling level `*-dl.json`, or
#' \code{\link{dv8_mdsmb_to_flaws}} to generate the architectural flaws `folder`.
#' For conversions, see \code{\link{dv8_mdsmb_to_hierclsxb}} to cluster the DSM to `*-clsx.dv8-clsx`, or
#' \code{\link{dv8_dsmb_to_dsmj}} to convert a `*-merged.dv8-dsm` binary file as a JSON `-*merged.json` file
#' @family dv8
dv8_hdsmb_sdsmb_to_mdsmb <- function(dv8_path, hdsmb_path, sdsmb_path, mdsmb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  hdsmb_path <- path.expand(hdsmb_path)
  sdsmb_path <- path.expand(sdsmb_path)
  mdsmb_path <- path.expand(mdsmb_path)

  # Run system2 command
  system2(dv8_path, args=c('core:merge-matrix', '-outputFile', mdsmb_path, hdsmb_path, sdsmb_path), stdout=FALSE, stderr=FALSE)

  return(mdsmb_path)
}

#' Computes the Architectural Flaws folder
#'
#' @param dv8_path path to dv8 binary
#' @param mdsmb_path path to the merge binary DSM created by \code{\link{dv8_hdsmb_sdsmb_to_mdsmb}}.
#' Note you can also use a hdsmb or sdsmb separately to compute architectural flaws.
#' @param flaws_path path to save the architectural flaws folder
#' @param is_file_only_metric default is FALSE. If TRUE, uses DV8 computed file metrics. Caution: While much faster,
#' these metrics can *not* be used to aggregate. Choosing FALSE will generate the file to flaw id mapping. Choosing
#' TRUE will generate the aggregation from DV8.
#' @param cliqueDepends (For Clique detection) Filtered dependencies for clique detection. Multiple dependencies should be delimited using ","
#' @param crossingCochange (For Crossing detection) Threshold of co-change between two files
#' @param crossingFanIn (For Crossing detection) The number of other files that depend on it >= "crossingFanIn"
#' @param crossingFanOut (For Crossing detection) The number of other files it depends on >= "crossingFanOut"
#' @param mvCochange (For Modularity Violation detection) Threshold of co-change between two files
#' @param uiCochange (For Unstable Interface detection) Threshold of co-change between two files
#' @param uihDepends (For Unhealthy Inheritance detection) Filtered dependencies for unhealthy inheritance detection. Multiple dependencies should be delimited using ","
#' @param uihInheritance (For Unhealthy Inheritance detection) Dependencies for unhealthy inheritance detection. Multiple dependencies should be delimited using ","
#' @param uiHistoryImpact (For Unstable Interface detection) Threshold of the number of co-changed (more than "co-change" times) files
#' @param uiStructImpact (For Unstable Interface detection) Threshold of value < 1 means percentage of all files are dependents
#' @return the path to the flaws folder
#'
#' @export
#' @seealso \code{\link{parse_dv8_architectural_flaws}} to create a `.csv`
#' mapping of file to architectural flaw.
#' @family dv8
dv8_mdsmb_to_flaws <- function(dv8_path,
                               mdsmb_path,
                               flaws_path,
                               is_file_only_metric = FALSE,
                               cliqueDepends='call,use',
                               crossingCochange=2,
                               crossingFanIn=4,
                               crossingFanOut=4,
                               mvCochange=2,
                               uiCochange=2,
                               uihDepends='call,use',
                               uihInheritance='extend,implement,public,private,virtual',
                               uiHistoryImpact=10,
                               uiStructImpact=0.01){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  mdsmb_path <- path.expand(mdsmb_path)
  flaws_path <- path.expand(flaws_path)

  if(is_file_only_metric){
    file_only_metric_flag <- "-fileStat"
  }else{
    file_only_metric_flag <- ""
  }

  # Run system2 command with appropriate values for options
  system2(dv8_path, args=c('arch-issue:arch-issue',
                           file_only_metric_flag,
                           '-cliqueDepends', cliqueDepends,
                           '-crossingCochange', crossingCochange,
                           '-crossingFanIn', crossingFanIn,
                           '-crossingFanOut', crossingFanOut,
                           '-mvCochange', mvCochange,
                           '-uiCochange', uiCochange,
                           '-uihDepends', uihDepends,
                           '-uihInheritance', uihInheritance,
                           '-uiHistoryImpact', uiHistoryImpact,
                           '-uiStructImpact', uiStructImpact,
                           '-outputFolder', flaws_path,
                           mdsmb_path),
          stdout=FALSE, stderr=FALSE)

  return(flaws_path)
}


#' Parse Architecture Flaws Map
#'
#' Computes from the folder template a table containing the
#' file assignment to identified architectural flaws. Note
#' this is a computational expensive function, and may take
#' awhile to finish.
#'
#' @param dv8_path path to dv8 binary
#' @param flaws_path path to architecture folder created by \code{\link{dv8_mdsmb_to_flaws}}
#' @param dsm_type the type of dsm that should be used available in the folder (i.e. merge, hdsm, or sdsm)
#' @param keep_intermediate_files TRUE if the user wishes to keep the intermediate files generated by this function, FALSE otherwise
#' @param progress_bar a boolean specifying if a progress bar should be shown.
#' @return data.table object with file to flaw mapping
#' @export
#'
#' @family dv8
parse_dv8_architectural_flaws <- function(dv8_path, flaws_path, dsm_type="merge", keep_intermediate_files=FALSE,progress_bar = NULL) {

  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  flaws_path <- path.expand(flaws_path)

  #folders_expected <- c("clique", "modularity-violation", "package-cycle", "unhealthy-inheritance")
  flaw_folders <- list.dirs(flaws_path,recursive = FALSE)

  generate_file_paths <- function(folder_path, flaws_path) {
    # Initialize an empty data.table to store the file paths
    dt_files <- data.table::data.table(file_path = character(), architecture_issue_type = character(), architecture_issue_id = character())


    folder_name <- stri_split_regex(folder_path,pattern="/")[[1]]
    folder_name <- folder_name[length(folder_name)]

    # Check if the sub-folder exists
    if (dir.exists(folder_path)) {
      # Get a list of all sub-folders in the current folder
      subdirs <- list.dirs(folder_path)

      # removes the path with no sub-folders
      subdirs <- subdirs[-1]

      # Loop over each sub-folders in the current folder
      for (subdir in subdirs) {
        #        if(!is.null(progress_bar)){
        #          progress_bar_i <- progress_bar_i + 1
        #          print(progress_bar_i)
        #          setTxtProgressBar(progress_bar,progress_bar_i)
        #        }
        # Get a list of all files in the current sub-folder
        files <- list.files(subdir, full.names = TRUE)

        # Get a list of all *merge.dv8-dsm file
        files_merged <- grep(dsm_type, files, value = TRUE)

        # If there are any files that contain the word 'merge'
        if (length(files_merged) > 0) {
          # loop through each file path in files_merged
          for (file in files_merged) {
            # call dv8_dsmb_to_dsmj to convert binary DSM to json file
            json_path <- file.path(dirname(file), basename(subdir))
            result <- dv8_dsmb_to_dsmj(dv8_path, file, json_path)

            if (length(result) > 0){
              result <- file.path(dirname(json_path), paste(basename(json_path), 'json', sep='.'))
              # Get variables (which contains the file paths) from json file
              file_path <- jsonlite::fromJSON(result)
              file_path <- file_path$variables
              dt <- data.table::as.data.table(file_path)

              # Get issue_id based on folder's name
              issue_id <- basename(subdir)

              # Add and fill architecture_issue type column with folder name and architecture_issue_id column with issue_id
              dt[, architecture_issue_type := folder_name]
              dt[, architecture_issue_id := issue_id]

              # Combine data.table from current json data with previous json data
              dt_files <- rbind(dt_files, dt, fill=TRUE)

              # Remove file generated by this function if keep_intermediate_files is TRUE
              if (!keep_intermediate_files){
                file.remove(result)
              }
            }

          }
        }
      }
    }

    # Return the data.table of file paths
    return(dt_files)
  }

  if(length(list.dirs(flaw_folders)) > 0){

    if(!is.null(progress_bar)){
      #list.dirs add a folder/ in addition to folder/1, folder/2 etc. The [-1] removes `folder/`
      # so it contain only the sub-folders.
      #all_instance_folder_paths <- unlist(sapply(flaw_folders,function(x)list.dirs(x)[-1],USE.NAMES = FALSE))
      # all folder instances across all flaw types
      #n_instances <- length(all_instance_folder_paths)
      n_instances <- length(flaw_folders)

      # This variable is "global" within this function scope to track the iterations
      progress_bar <- txtProgressBar(min = 0,
                                     max = n_instances,
                                     style = 3)
      progress_bar_i <- 0
    }


    result <- data.table::data.table(file_path = character(), architecture_issue_type = character(), architecture_issue_id = character())
    # Loop over the folder exist in the given folder
    for (folder_path in flaw_folders) {
      # If at least one of the folder exists (e.g. clique, package-cycle, etc.),
      # call generate_file_path function
      result <- rbind(result, generate_file_paths(folder_path, flaws_path), fill=TRUE)
      progress_bar_i <- progress_bar_i + 1
      setTxtProgressBar(progress_bar,progress_bar_i)
    }
  }else{
    stop(paste0("No flaws sub-folders were detected. Is ",folder_path,"the correct flaws folder path?"))
  }

  close(progress_bar)
  return (result)
}

#' Computes the Decoupling Level from a mdsm binary file
#'
#' Creates a \*.json file with decoupling metrics from \*.dv8-dsm file
#'
#' @param dv8_path path to DV8 binary
#' @param mdsmb_path path to mdsm binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param dl_path name of output file
#'
#' @return the dl_path
#' @export
#' @seealso \code{\link{parse_dv8_metrics_decoupling_level}} to parse decoupling level JSON file `-dl.json`
#'
dv8_mdsmb_to_decoupling_level <- function(dv8_path, mdsmb_path, dl_path) {
  dv8_path <- path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s ", dl_path)

  system2(command=dv8_path,
          args=c("metrics:decoupling-level",
                 output_file_arg,
                 mdsmb_path),
          stdout=dl_path)

  return(dl_path)
}




#' Computes the design rule hierarchy for a mdsm binary file
#'
#' Creates a \*.dv8-clsx file from a \*.dv8-dsm file
#'
#' @param dv8_path path to DV8 binary
#' @param mdsmb_path path to mdsm binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param max_depth recursion depth limit
#' @param modules merge modules following same design rules if switched on
#' @param recursive use recursive algorithm if switched on
#' @param hierclsxb_path name of output file
#'
#' @return the hierclsxb_path
#' @export
#' @seealso \code{\link{dv8_mdsmb_drhier_to_excel}} for exporting `*-clsx.dv8-clsx` to excel `-clsx.xlsx`, or
#' \code{\link{dv8_clsxb_to_clsxj}} and \code{\link{parse_dv8_clusters}} for conversion and parsing.
#'
dv8_mdsmb_to_hierclsxb <- function(dv8_path,
                                   mdsmb_path,
                                   hierclsxb_path,
                                   max_depth="",
                                   modules=FALSE,
                                   recursive=FALSE) {
  dv8_path <- path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s", hierclsxb_path)
  max_depth_arg <- ""
  modules_arg <- ""
  recursive_arg <- ""

  if (max_depth_arg != "") {
    max_depth_arg <- sprintf("-maxDepth %i ", max_depth)
  }

  if (modules) {
    modules_arg <- "-modules "
  }

  if (recursive) {
    recursive_arg <- "-recursive "
  }

  system2(command=dv8_path,
          args=c("dr-hier:dr-hier",
                 max_depth_arg,
                 modules_arg,
                 recursive_arg,
                 output_file_arg,
                 mdsmb_path),
          stdout=hierclsxb_path)

  return(hierclsxb_path)
}

#' Export a mdsmb binary file into a spreadsheet
#'
#' Creates a \*.xlsx from a \*.dv8-dsm file
#'
#' @param dv8_path path to DV8 binary
#' @param mdsmb_path path to mdsmb binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param excel_path path to output excel file
#' @param hierclsxm_path path to clustering file used to order files in spreadsheet from \code{\link{dv8_mdsmb_to_hierclsxb}}
#' @param detail if TRUE, include details within each cell in spreadsheet, default FALSE
#' @param drhier if TRUE, use recursive drh clustering in spreadsheet, default FALSE
#' @param namespace if TRUE, use namespace clustering in spreadsheet, default FALSE
#'
#' @return the excel_path
#' @export
#'
dv8_mdsmb_drhier_to_excel <- function(dv8_path,
                                      mdsmb_path,
                                      excel_path,
                                      hierclsxm_path="",
                                      detail=FALSE,
                                      drhier=TRUE,
                                      namespace=FALSE) {
  dv8_path = path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s", excel_path)
  cluster_path_arg <- ""
  detail_arg <- ""
  drhier_arg <- ""
  namespace_arg <- ""

  if (namespace) {
    namespace_arg <- "-namespace "
  }

  if (drhier) {
    drhier_arg <- "-drhier "
    namespace_arg <- ""
  }

  if (cluster_path_arg != "") {
    cluster_path_arg = sprintf("-cluster %s ", hierclsxm_path)
    namespace_arg <- ""
    drhier_arg <- ""
  }

  if (detail) {
    detail_arg = "-detail "
  }

  system2(command=dv8_path,
          args=c("export-spreadsheet",
                 cluster_path_arg,
                 detail_arg,
                 drhier_arg,
                 namespace_arg,
                 output_file_arg,
                 mdsmb_path),
          stdout=FALSE)

  return(excel_path)
}


#' Parses the decoupling level JSON file
#'
#' Creates a data.table object from a *.json file
#'
#' @param dl_path path to decoupling metric json file created by \code{\link{dv8_mdsmb_to_decoupling_level}}
#'
#' @return data.table object with decoupling metrics
#' @export
#'
parse_dv8_metrics_decoupling_level <- function(dl_path) {
  json <- jsonlite::read_json(dl_path)
  dl_table <- setDT(json)

  return(dl_table)
}


#' Transforms a DV8 binary cluster file to a json cluster file.
#'
#' Converts a clustering binary in the dv8 format *-clsx.dv8-cslx into a
#'  *-clsx.json.
#'
#' @param dv8_path path to dv8 binary
#' @param clsxb_path path to clsx to convert created by \code{\link{dv8_mdsmb_to_hierclsxb}}
#' @param clsxj_path path to save output file
#' @return the clsxj_path
#' @export
#' @family dv8
dv8_clsxb_to_clsxj <- function(dv8_path, clsxb_path, clsxj_path){
  system2(dv8_path, args=c('core:export-cluster', '-outputFile', clsxj_path, clsxb_path), stdout=FALSE, stderr=FALSE)
  return(clsxj_path)
}



#' Convert a json cluster file, clsxj, to a binary cluster file, clsxb.
#'
#' An input file \*-hier.json is converted to \*-clsx.dv8-clsx.
#'
#' @param dv8_path path to dv8 binary
#' @param clsxj_path path to JSON cluster created by \code{\link{dv8_mdsmb_to_hierclsxb}}
#' @param clsxb_path path to save the DV8 binary cluster
#' @return the clsxb_path
#' @export
#' @family dv8
dv8_clsxj_to_clsxb <- function(dv8_path, clsxj_path, clsxb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  clsxj_path <- path.expand(clsxj_path)
  clsxb_path <- path.expand(clsxb_path)

  # Run system2 command
  system2(dv8_path, args=c('core:convert-cluster', '-outputFile', clsxb_path, clsxj_path), stdout=FALSE, stderr=FALSE)
}


#' Parses a DV8 json cluster file to R data.table.
#'
#' Parses a cluster *-clsx.json into a data table.
#'
#' @param clsxj_path path to clustering json created by \code{\link{dv8_clsxb_to_clsxj}}
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
      LM_name <- stringi::stri_split_regex(structure[[i]]$nested[[j]]$name, pattern = "/")
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
  cluster_parsed <- data.table::setnames(cluster_parsed, c("V1", "V2", "V3"), c("file_path", "module", "layer"))
  # Return parsed cluster data table
  return(cluster_parsed)
}
