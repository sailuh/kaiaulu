# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Parsers ##############

#' Parse NVD Feed CVEs, descriptions and CWE ids
#'
#' @param nvdfeed_folder_path Folderpath for nvd feed files
#' under schema 1.1 (e.g. nvdcve-1.1-2018.json)
#' @export
#' @family parsers
parse_nvdfeed <- function(nvdfeed_folder_path){
  folder_path <- path.expand(nvdfeed_folder_path)
  all_files_path <- list.files(folder_path,full.names = TRUE)

  parse_single_feed <- function(nvd_feed_json){
    all_cves <- nvd_feed_json[["CVE_Items"]]
    n_cves <- nvd_feed_json[["CVE_data_numberOfCVEs"]]
    cve_list <- vector(mode = "list", length = n_cves)
    for (i in 1:as.integer(n_cves)){
      cve <- all_cves[[i]][["cve"]]
      cve_id <- cve[["CVE_data_meta"]][["ID"]]
      cwe_metadata <- cve[["problemtype"]][["problemtype_data"]][[1]][["description"]]
      # can be missing if CVE is rejected. See CVE-1999-0020 as example on 2002 feed file.
      if(length(cwe_metadata) > 0){
        cwe_id <- cwe_metadata[[1]][["value"]]
      }else{
        cwe_id <- NA
      }
      cve_description <- cve[["description"]][["description_data"]][[1]][["value"]]
      cve_list[[i]] <- data.table(cve_id,cwe_id,cve_description)
    }
    return(rbindlist(cve_list))
  }
  n_nvdfeeds <- length(all_files_path)
  cve_list <- vector(mode = "list", length = n_nvdfeeds)
  for(i in 1:n_nvdfeeds){
    nvd_json <- jsonlite::read_json(all_files_path[i])
    cve_list[[i]] <- parse_single_feed(nvd_json)
  }
  return(rbindlist(cve_list))
}

############## Network Transform ##############

#' Transform parsed cveid and nvdfeed into a network
#'
#' @param project_cve A parsed cve edgelist by \code{\link{transform_commit_message_id_to_network}}.
#' @param nvd_feed  Parsed  nvdfeed by \code{\link{parse_nvdfeed}}.
#' @export
#' @family edgelists
transform_cve_cwe_file_to_network <- function(project_cve,nvd_feed){
  commit_message_id <- cwe_id <- name <- color <- src <- dest <- weight <- NULL # due to NSE notes in R CMD check

  cve_nodes <- project_cve[["nodes"]]
  cve_edgelist <- project_cve[["edgelist"]]
  # Find the edges from CVE ids to CWE ids
  cwe_edgelist <- merge(
    cve_edgelist,
    nvd_feed,
    by.x="from",
    by.y = "cve_id",
    all.x = TRUE)[,.(from,cwe_id)]
  # Edges from CVE ids without a matching CWE should be removed
  cwe_edgelist <- cwe_edgelist[!is.na(cwe_id)]
  # Add all new CWE IDs to the list of nodes with a different color
  # Type is dropped, as graph viz tools can't distinguish between 3 types of nodes
  cve_nodes <- cve_nodes[,.(name,color)]
  cwe_nodes <- data.table(name=unique(cwe_edgelist$cwe_id),
                          color="#D44942")
  # Set Union Nodes
  cve_cwe_file_nodes <- rbind(cve_nodes,cwe_nodes)
  # Network will be 3 modal, rename columns to avoid confusion
  colnames(cve_edgelist) <- c("src","dest","weight")
  colnames(cwe_edgelist) <- c("src","dest")
  # For each cve id, only 1 edge is added, hence weight is always 1
  cwe_edgelist$weight <- rep(1,nrow(cwe_edgelist))
  # Set union the cve and cwe edgelists
  cve_cwe_file_edgelist <- rbind(cve_edgelist,cwe_edgelist)
  # Return the set union as nodes and edgelist.
  cve_cwe_file_network <- list()
  cve_cwe_file_network[["nodes"]] <- cve_cwe_file_nodes
  cve_cwe_file_network[["edgelist"]] <- cve_cwe_file_edgelist
  return(cve_cwe_file_network)
}
