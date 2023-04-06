#' Convert from sdsm.json to a dependency matrix in sdsm.dv8-dsm file
#'
#' @param dsmj_path path to sdsm.json
#' @param dsmb_path path to save dv8-dsm file
#' @export
dv8_dsmj_to_dsmb <- function(dsmj_path, dsmb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dsmj_path <- path.expand(dsmj_path)
  dsmb_path <- path.expand(dsmb_path)

  # Run system2 command
  system2('dv8-console', args=c('core:convert-matrix', '-outputFile', dsmb_path, dsmj_path), stdout=FALSE, stderr=FALSE)
}

#' Merge matrices from dv8-dsm file to one new matrix in hsdsm.dv8-dsm
#'
#' @param hdsmb_path path to the matrix from hdsmb file
#' @param sdsmb path to the matrix from sdsmb file
#' @param hsdsmb_path path to save the merged matrices in hsdsm.dv8-dsm file
#' @export
dv8_hdsmb_sdsmb_to_hsdsmb <- function(hdsmb_path, sdsmb_path, hsdsmb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  hdsmb_path <- path.expand(hdsmb_path)
  sdsmb_path <- path.expand(sdsmb_path)
  hsdsmb_path <- path.expand(hsdsmb_path)

  # Run system2 command
  system2('dv8-console', args=c('core:merge-matrix', '-outputFile', hsdsmb_path, hdsmb_path, sdsmb_path), stdout=FALSE, stderr=FALSE)
}

#' Convert a json cluster file, clsxj, to a binary cluster file, clsxb.
#' An input file *-hier.json is converted to *.dv8-clsx.
#'
#' @param clsxj_path path to the input file project-hier.json
#' @param clsxb_path path to clustering file (*.dv8-clsx)
#' @export
dv8_clsxj_to_clsxb <- function(clsxj_path, clsxb_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  clsxj_path <- path.expand(clsxj_path)
  clsxb_path <- path.expand(clsxb_path)

  # Run system2 command
  system2('dv8-console', args=c('core:convert-cluster', '-outputFile', clsxb_path, clsxj_path), stdout=FALSE, stderr=FALSE)
}

#' Detect Architecture anti-patterns
#'
#' @param hsdsmb_path path to the merged matrix input file dv8-dsm
#' @param flaws_path path to architecture folder
#' @export
dv8_hsdsmb_to_flaws <- function(hsdsmb_path, flaws_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  hsdsmb_path <- path.expand(hsdsmb_path)
  flaws_path <- path.expand(flaws_path)

  # Run system2 command with appropriate values for options
  system2('dv8-console', args=c('arch-issue:arch-issue',
                                '-uiCochange', '2',
                                '-crossingFanIn', '4',
                                '-crossingFanOut', '4',
                                '-cliqueDepends', 'call,use',
                                '-uihDepends', 'call,use',
                                '-uihInheritance', 'extend,implement,public,private,virtual',
                                '-outputFolder', flaws_path,
                                hsdsmb_path),
          stdout=FALSE, stderr=FALSE)
}

#' Parse Architecture Flaws
#'
#' @param file_measure_report_path path to file-measure-report.csv
#' @param flaws_path path to architecture folder
#' @export
parse_dv8_architectural_flaws <- function(file_measure_report_path, flaws_path) {
  folders_expected <- c("clique", "modularity-violation", "package-cycle", "unhealthy-inheritance")
  folders_exist <- list.files(flaws_path)

  check_folders_exist <- function(folders_expected, folder) {
    folder_exists <- FALSE

    if (folder %in% folders_expected) {
      folder_exists <- TRUE
    }

    return(folder_exists)
  }

  convert_dsm_binary_to_json <- function(dsm_binary_file_path, json_path) {
    hierjson_path <- file.path(json_path, 'hier.json')
    system2('dv8-console', args=c('core:export-matrix', '-outputFile', hierjson_path, dsm_binary_file_path), stdout=FALSE, stderr=FALSE)
    return (hierjson_path)
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
          for (file_path in files_merged) {
            # call convert_dsm_binary_to_json for each dv8-dsm file
            save_path <- dirname(file_path)
            result <- convert_dsm_binary_to_json(file_path, save_path)

            # Get variables (which contains the file paths) from json file
            file_paths <- jsonlite::fromJSON(result)
            files <- file_paths$variables
            file.remove(result)

            for (project_file in files){
              # add the file path and folder name to the data.table
              issue_id <- gsub("[^0-9]", "", gsub(folder_path, '', subdir))
              dt_files <- rbind(dt_files, data.table::data.table(file_path = project_file, architecture_issue_type = folder_name, architecture_issue_id = issue_id))
            }

          }
        }
      }
    }

    # Return the data.table of file paths
    return(dt_files)
  }

  result <- data.table::data.table(file_path = character(), architecture_issue_type = character(), architecture_issue_id = character())

  for (dir in folders_exist) {
    if (check_folders_exist(folders_expected, dir)) {
      result <- rbind(result, generate_file_paths(dir, flaws_path))
    }
  }

  return (result)
}
