# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Convert from a JSON DSM to a binary DSM.
#' Note: This function converts
#'                    a hdsm.json (*.json) to history DSM (*.dv8-dsm)
#'                    a sdsm.json (*.json) to structural DSM (*.dv8-dsm)
#'
#' @param dv8_path path to dv8 binary
#' @param dsmj_path path to JSON DSM
#' @param dsmb_path path to save binary DSM
#' @export
#' @family dv8
dv8_dsmj_to_dsmb <- function(dv8_path, dsmj_path, dsmb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  dsmj_path <- path.expand(dsmj_path)
  dsmb_path <- path.expand(dsmb_path)
  
  # Run system2 command
  system2(dv8_path, args=c('core:convert-matrix', '-outputFile', dsmb_path, dsmj_path), stdout=FALSE, stderr=FALSE)
}

#' Merge history DSM and structural DSM to merged DSM.
#' A history DSM (*.dv8-dsm) and a structural DSM (*.dv8-dsm) is merged to a merged DSM (*.dv8-dsm)
#'
#' @param dv8_path path to dv8 binary
#' @param hdsmb_path path to the matrix from history binary DSM
#' @param sdsmb_path path to the matrix from structural binary DSM
#' @param hsdsmb_path path to save the merged matrices in binary DSM
#' @export
#' @family dv8
dv8_hdsmb_sdsmb_to_hsdsmb <- function(dv8_path, hdsmb_path, sdsmb_path, hsdsmb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  hdsmb_path <- path.expand(hdsmb_path)
  sdsmb_path <- path.expand(sdsmb_path)
  hsdsmb_path <- path.expand(hsdsmb_path)
  
  # Run system2 command
  system2(dv8_path, args=c('core:merge-matrix', '-outputFile', hsdsmb_path, hdsmb_path, sdsmb_path), stdout=FALSE, stderr=FALSE)
}

#' Convert a json cluster file, clsxj, to a binary cluster file, clsxb.
#' An input file *-hier.json is converted to *.dv8-clsx.
#'
#' @param dv8_path path to dv8 binary
#' @param clsxj_path path to JSON cluster
#' @param clsxb_path path to DV8 binary cluster
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

#' Detect Architecture anti-patterns
#'
#' @param dv8_path path to dv8 binary
#' @param hsdsmb_path path to the merged binary DSM
#' @param flaws_path path to architecture folder
#' @param cliqueDepends (For Clique detection) Filtered dependencies for clique detection. Multiple dependencies should be delimited using ","
#' @param crossingCochange (For Crossing detection) Threshold of co-change between two files
#' @param crossingFanIn (For Crossing detection) The number of other files that depend on it >= "crossingFanIn"
#' @param crossingFanOut (For Crossing detection) The number of other files it depends on >= "crossingFanOut"
#' @param mvCochange (For Modularity Violation detection) Threshold of co-change between two files
#' @param uiCochange (For Unstable Interface detection) Threshold of co-change between two files
#' @param uihDepends (For Unhealthy Inheritance detection) Filtered dependencies for unhealthy inheritance detection. Multiple dependencies should be delimited using ","
#' @param uihInheritance (For Unhealthy Inheritance detection) Dependencies for unhealthy inheritance detection. Multiple dependencies should be delimited using ","
#' @param historyImpact (For Unstable Interface detection) Threshold of the number of co-changed (more than "co-change" times) files
#' @param uiStructImpact (For Unstable Interface detection) Threshold of value < 1 means percentage of all files are dependents
#'
#' @export
#' @family dv8
dv8_hsdsmb_to_flaws <- function(dv8_path,
                                hsdsmb_path,
                                flaws_path,
                                cliqueDepends='call,use',
                                crossingCochange=2,
                                crossingFanIn=4,
                                crossingFanOut=4,
                                mvCochange=2,
                                uiCochange=2,
                                uihDepends='call,use',
                                uihInheritance='extend,implement,public,private,virtual',
                                historyImpact=10,
                                uiStructImpact=0.01){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  hsdsmb_path <- path.expand(hsdsmb_path)
  flaws_path <- path.expand(flaws_path)
  
  # Run system2 command with appropriate values for options
  system2(dv8_path, args=c('arch-issue:arch-issue',
                           '-cliqueDepends', cliqueDepends,
                           '-crossingCochange', crossingCochange,
                           '-crossingFanIn', crossingFanIn,
                           '-crossingFanOut', crossingFanOut,
                           '-mvCochange', mvCochange,
                           '-uiCochange', uiCochange,
                           '-uihDepends', uihDepends,
                           '-uihInheritance', uihInheritance,
                           '-historyImpact', historyImpact,
                           '-uiStructImpact', uiStructImpact,
                           '-outputFolder', flaws_path,
                           hsdsmb_path),
          stdout=FALSE, stderr=FALSE)
}

#' Parse Architecture Flaws
#'
#' @param dv8_path path to dv8 binary
#' @param flaws_path path to architecture folder
#' @param keep_intermediate_files TRUE if the user wishes to keep the intermediate files generated by this function, FALSE otherwise
#'
#' @export
#' @family dv8
parse_dv8_architectural_flaws <- function(dv8_path, flaws_path, keep_intermediate_files=FALSE) {
  
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_path <- path.expand(dv8_path)
  flaws_path <- path.expand(flaws_path)
  
  folders_expected <- c("clique", "modularity-violation", "package-cycle", "unhealthy-inheritance")
  folders_exist <- list.files(flaws_path)
  
  check_folders_exist <- function(folders_expected, folder) {
    folder_exists <- FALSE
    
    if (folder %in% folders_expected) {
      folder_exists <- TRUE
    }
    
    return(folder_exists)
  }
  
  generate_file_paths <- function(folder_name, flaws_path) {
    # Initialize an empty data.table to store the file paths
    dt_files <- data.table::data.table(file_path = character(), architecture_issue_type = character(), architecture_issue_id = character())
    
    # Get the full path to the current folder
    folder_path <- file.path(flaws_path, folder_name)
    
    # Check if the sub-folder exists
    if (dir.exists(folder_path)) {
      # Get a list of all sub-folders in the current folder
      subdirs <- list.dirs(folder_path)
      subdirs <- subdirs[-1]
      
      # Loop over each sub-folders in the current folder
      for (subdir in subdirs) {
        
        # Get a list of all files in the current sub-folder
        files <- list.files(subdir, full.names = TRUE)
        
        # Get a list of all *merge.dv8-dsm file
        files_merged <- grep('merge', files, value = TRUE)
        
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
  
  result <- data.table::data.table(file_path = character(), architecture_issue_type = character(), architecture_issue_id = character())
  
  # Loop over the folder exist in the given folder
  for (dir in folders_exist) {
    # If one of the folder exists, call generate_file_path function
    # Ex. If clique folder exists, call generate_file_path function
    if (check_folders_exist(folders_expected, dir)) {
      result <- rbind(result, generate_file_paths(dir, flaws_path), fill=TRUE)
    }
  }
  
  return (result)
}

#' Creates a gitlog numstat file from a git file
#'
#' From a git file *.git, a gitlog numstat file *.txt is created
#'
#' @param git_repo_path path to local repository
#' @param git_numstat_path file name of git log output
#'
#' @return path to git log output
#' @export
#' @seealso \code{\link{dv8_gitnumstat_to_hdsmb}} for creating hsdsmb binary file from a git numstat text file
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
#' An input file *.txt is converted to *.dv8-dsm
#'
#' @param dv8_path path to DV8 binary
#' @param git_numstat_path path to local git log text file obtained by \code{\link{dv8_gitlog_to_gitnumstat}}
#' @param start start of date/time range in ISO format ie. 2017-07-08T00:00:00Z
#' @param stop end of date/time range in ISO format ie. 2018-07-08T00:00:00Z
#' @param max_cochange_count maximum count of co-changed files per commit, default 1000
#' @param hdsmb_path name of output hdsmb binary file, defaults to name of input
#' @param params_output_file output file used to record parameters
#'
#' @return path to hdsmb binary file
#' @export
#' @seealso \code{\link{dv8_hsdsmb_to_decoupling_level}} to calculate decoupling level from hsdsmb binary file
#' @seealso \code{\link{dv8_hsdsmb_to_hierclsxb}} to compute design rule hierarchy from hsdsmb binary file
#' @seealso \code{\link{dv8_hsdsmb_drhier_to_excel}} to export a hsdsmb binary file as an excel file
#' @seealso \code{\link{dv8_hsdsmb_dsmb_to_dsmj}} to export a hsdsmb binary file as a json file
#'
dv8_gitnumstat_to_hdsmb <- function(dv8_path,
                                    git_numstat_path,
                                    hdsmb_path,
                                    start,
                                    stop,
                                    max_cochange_count=1000,
                                    params_output_file="") {
  
  dv8_path <- path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s", hdsmb_path)
  start_arg <- sprintf("-start %s", start)
  stop_arg <- sprintf("-stop %s", stop)
  max_cochange_count_arg <- sprintf("-maxCochangeCount %i", max_cochange_count)
  params_output_file_arg <- ""
  
  if (params_output_file != "") {
    params_output_file_arg <- sprintf("-paramsOutputFile %s ", params_output_file)
  }
  
  system2(command=dv8_path,
          args=c("scm:history:gittxt:convert-matrix",
                 start_arg,
                 stop_arg,
                 output_file_arg,
                 max_cochange_count_arg,
                 params_output_file_arg,
                 git_numstat_path),
          stdout=hdsmb_path)
  
  return(hdsmb_path)
}


#' Calculates the Decoupling Level from a hsdsm binary file
#'
#' Creates a *.json file with decoupling metrics from *.dv8-dsm file
#'
#' @param dv8_path path to DV8 binary
#' @param hsdsmb_path path to hsdsm binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param json_path name of output file
#'
#' @return path of output json file
#' @export
#' @seealso \code{\link{parse_dv8_metrics_decoupling_level}} to parse decoupling level json file
#'
dv8_hsdsmb_to_decoupling_level <- function(dv8_path, hsdsmb_path, json_path) {
  dv8_path <- path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s ", json_path)
  
  system2(command=dv8_path,
          args=c("metrics:decoupling-level",
                 output_file_arg,
                 hsdsmb_path),
          stdout=json_path)
  
  return(json_path)
}


#' Computes the design rule hierarchy for a hsdsm binary file
#'
#' Creates a *.dv8-clsx file from a *.dv8-dsm file
#'
#' @param dv8_path path to DV8 binary
#' @param hsdsmb_path path to hsdsm binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param max_depth recursion depth limit
#' @param modules merge modules following same design rules if switched on
#' @param recursive use recursive algorithm if switched on
#' @param hierclsxb_path name of output file
#'
#' @return path of output file
#' @export
#' @seealso \code{\link{dv8_hsdsmb_drhier_to_excel}} for using *.dv8-clsx file when exporting hsdsmb binary file as an excel file
#'
dv8_hsdsmb_to_hierclsxb <- function(dv8_path,
                                    hsdsmb_path,
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
                 hsdsmb_path),
          stdout=hierclsxb_path)
  
  return(hierclsxb_path)
}


#' Export a hsdsmb binary file into a spreadsheet
#'
#' Creates a *.xlsx from a *.dv8-dsm file
#'
#' @param dv8_path path to DV8 binary
#' @param hsdsmb_path path to hsdsmb binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param excel_path path to output excel file
#' @param hierclsxm_path path to clustering file used to order files in spreadsheet from \code{\link{dv8_hsdsmb_to_hierclsxb}}
#' @param detail if TRUE, include details within each cell in spreadsheet, default FALSE
#' @param drhier if TRUE, use recursive drh clustering in spreadsheet, default FALSE
#' @param namespace if TRUE, use namespace clustering in spreadsheet, default FALSE
#'
#' @return path to output excel file
#' @export
#'
dv8_hsdsmb_drhier_to_excel <- function(dv8_path,
                                       hsdsmb_path,
                                       excel_path,
                                       hierclsxm_path="",
                                       detail=FALSE,
                                       drhier=FALSE,
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
                 hsdsmb_path),
          stdout=hierclsxm_path)
  
  return(excel_path)
}

#' Export a dsmb binary file as a separate json file
#'
#' Takes a *.dv8-dsm file and exports as *.json file
#'
#' @param dv8_path path to DV8 binary
#' @param dsmb_path path to dsmb binary file created by \code{\link{dv8_gitnumstat_to_hdsmb}}
#' @param json_path path to json file
#'
#' @return path to output json file
#' @export
#'
dv8_dsmb_to_dsmj <- function(dv8_path, dsmb_path, json_path) {
  dv8_path = path.expand(dv8_path)
  output_file_arg <- sprintf("-outputFile %s", json_path)
  
  system2(command=dv8_path,
          args=c("core:export-matrix",
                 output_file_arg,
                 dsmb_path),
          stdout=json_path)
  
  return(json_path)
}

#' Parses the decoupling level JSON file
#'
#' Creates a data.table object from a *.json file
#'
#' @param json_path path to decoupling metric json file created by \code{\link{dv8_hsdsmb_to_decoupling_level}}
#'
#' @return data.table object with decoupling metrics
#' @export
#'
parse_dv8_metrics_decoupling_level <- function(json_path) {
  json <- read_json(json_path)
  dl_table <- setDT(json)
  
  return(dl_table)
}
