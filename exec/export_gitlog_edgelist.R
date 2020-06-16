#!/usr/bin/env Rscript
library(kaiaulu)
library(stringi)
library(yaml)
library(cli)
library(data.table)
library(magrittr)

args = commandArgs(trailingOnly=TRUE)
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
cli_alert("Parsing config file...")
conf <- yaml::read_yaml(args[1])
save_path <- args[2]

perceval_path <- conf[["tool"]][["perceval"]][["bin"]]
git_repo_path <- conf[["data_path"]][["git"]]
nvdfeed_folder_path <- conf[["data_path"]][["nvd_feed"]]
cveid_regex <- conf[["commit_message_id_regex"]][["cve_id"]]

# Filters
file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]
substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]
cli_alert_success("Finished parsing config file!")
# Parse gitlog
cli_alert("Starting to parse git log...")
project_git <- parse_gitlog(perceval_path,git_repo_path)
cli_alert_success("Finished parsing git log!")
# Filter file
cli_alert("Filtering extensions as specified on config...")
project_git <- project_git  %>%
  filter_by_file_extension(file_extensions,"file")  %>%
  filter_by_filepath_substring(substring_filepath,"file")

# Extract CVEs
cli_alert("Starting to parse CVEs...")
project_commit_message_id_edgelist <- parse_commit_message_id_network(project_git,
                                                                      commit_message_id_regex = cveid_regex)

cli_alert_success("Finished parsing CVEs!")
cli_alert("Saving rds object...")
saveRDS(project_commit_message_id_edgelist,save_path)
cli_alert_success("Script is done!")
