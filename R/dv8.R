#' Convert from sdsm.json to a dependency matrix in sdsm.dv8-dsm file
#'
#' @param sdsm_json_path path to sdsm.json
#' @param dv8_dsm_save_path path to save dv8-dsm file
#' @export
dv8_dsmj_to_dsmb <- function(sdsm_json_path, dv8_dsm_save_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  sdsm_json_path <- path.expand(sdsm_json_path)
  dv8_dsm_save_path <- path.expand(dv8_dsm_save_path)

  # Run system2 command
  system2('dv8-console', args=c('core:convert-matrix', '-outputFile', dv8_dsm_save_path, sdsm_json_path), stdout=FALSE, stderr=FALSE)
}

#' Merge matrices from sdsm.dv8-dsm file to one new matrix in hsdsm.dv8-dsm
#'
#' @param dv8_dsm_path_1 path to the matrix from sdsm.dv8-dsm file
#' @param dv8_dsm_path_2 path to the matrix from sdsm.dv8-dsm file
#' @param merged_dv8_dsm_path path to save the merged matrices in hsdsm.dv8-dsm file
#' @export
dv8_sdsmb_to_hsdsmb <- function(dv8_dsm_path_1, dv8_dsm_path_2, merged_dv8_dsm_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  dv8_dsm_path_1 <- path.expand(dv8_dsm_path_1)
  dv8_dsm_path_2 <- path.expand(dv8_dsm_path_2)
  merged_dv8_dsm_path <- path.expand(merged_dv8_dsm_path)

  # Run system2 command
  system2('dv8-console', args=c('core:merge-matrix', '-outputFile', merged_dv8_dsm_path, dv8_dsm_path_1, dv8_dsm_path_2), stdout=FALSE, stderr=FALSE)
}

#' Convert an input file project-hier.json to a clustering file (*.dv8-clsx)
#'
#' @param input_path path to the input file project-hier.json
#' @param output_path path to clustering file (*.dv8-clsx)
#' @export
dv8_clsxj_to_clsxb <- function(input_path, output_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  input_path <- path.expand(input_path)
  output_path <- path.expand(output_path)

  # Run system2 command
  system2('dv8-console', args=c('core:convert-cluster', '-outputFile', output_path, input_path), stdout=FALSE, stderr=FALSE)
}
