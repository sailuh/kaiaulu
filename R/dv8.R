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
