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
