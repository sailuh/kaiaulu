#!/usr/local/bin/Rscript

# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


require(yaml,quietly=TRUE)
require(cli,quietly=TRUE)
require(docopt,quietly=TRUE)
require(kaiaulu,quietly=TRUE)
require(data.table,quietly=TRUE)
require(magrittr,quietly=TRUE)
require(stringi,quietly=TRUE)
require(reactable,quietly=TRUE)
require(doParallel,quietly=TRUE)
require(doSNOW,quietly=TRUE)



doc <- "
USAGE:
  git.R tabulate help
  git.R tabulate <tools.yml> <project_conf.yml> <cli_conf.yml> <save_file_name_path>
  git.R entity help
  git.R entity <tools.yml> <project_conf.yml> <cli_conf.yml> <gitlog_file_name_path> <save_file_name_path>
  git.R entity parallel help
  git.R entity parallel <tools.yml> <project_conf.yml> <cli_conf.yml> <gitlog_file_name_path> <save_folder_name_path>
  git.R (-h | --help)
  git.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Git. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["tabulate"]] & arguments[["help"]]){
  cli_alert_info("Tabulates a git log using parse_gitlog(). Optionally filters
                 files and matches identities according to the configuration.")
}else if(arguments[["tabulate"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  cli_path <- arguments[["<cli_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  cli <- yaml::read_yaml(cli_path)

  id_match <- cli[["git"]][["file"]][["identity"]][["match"]]
  id_columns <- cli[["git"]][["file"]][["identity"]][["columns"]]
  id_names_only <- cli[["git"]][["file"]][["identity"]][["names_only"]]

  perceval_path <- path.expand(tool[["perceval"]])
  git_repo_path <- path.expand(conf[["version_control"]][["log"]])

  # File Filters
  filter <- cli[["git"]][["file"]][["filter"]]
  file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]
  substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]
  filter_commit_size <- conf[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  project_git <- parse_gitlog(perceval_path,git_repo_path)

  # Filter files
  if (filter) {
    if (length(file_extensions) > 0) project_git <- project_git  %>% filter_by_file_extension(file_extensions,"file_pathname")
    if (length(substring_filepath) > 0) project_git <- project_git  %>%  filter_by_filepath_substring(substring_filepath,"file_pathname")
    if (length(filter_commit_size) > 0) project_git <- project_git %>% filter_by_commit_size(commit_size = filter_commit_size)
  }

  # Identity match
  if (id_match){
    project_log <- list(project_git=project_git)
    project_log <- identity_match(project_log,
                                  name_column=id_columns,
                                  assign_exact_identity,
                                  use_name_only=id_names_only,
                                  label = "raw_name")
    project_git <- project_log[["project_git"]]
  }

  data.table::fwrite(project_git,save_path)
  cli_alert_success(paste0("Tabulated git log was saved at: ",save_path))
}else if(arguments[["entity"]] & arguments[["parallel"]] & arguments[["help"]]){
  cli_alert_info("Parses changed entities using parse_gitlog_entity() in
                 parallel when analysing multiple time windows. Time
                 windows are preferentially determined by ranges
                 specified in the configuration file. If no ranges are
                 specified, time windows are generated based on the
                 window size in days starting at the first commit's date.
                 Optionally filters files and matches identities according to
                 the configuration.")
}else if(arguments[["entity"]] & arguments[["parallel"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  cli_path <- arguments[["<cli_conf.yml>"]]
  gitlog_path <- arguments[["<gitlog_file_name_path>"]]
  save_path <- arguments[["<save_folder_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  cli <- yaml::read_yaml(cli_path)

  time <- cli[["git"]][["entity"]][["window"]][["time"]]
  start_include <- cli[["git"]][["entity"]][["window"]][["start_include"]]
  end_include <- cli[["git"]][["entity"]][["window"]][["end_include"]]
  id_match <- cli[["git"]][["entity"]][["identity"]][["match"]]
  id_columns <- cli[["git"]][["entity"]][["identity"]][["columns"]]
  id_names_only <- cli[["git"]][["entity"]][["identity"]][["names_only"]]

  # 3rd Party Tools
  utags_path <- tool[["utags"]]

  # Local Git Repo Folder Path
  git_repo_path <- conf[["version_control"]][["log"]]

  # File Filters
  filter <- cli[["git"]][["entity"]][["filter"]]
  file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]
  substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]
  filter_commit_size <- conf[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  # Ctags Line Types
  kinds <- conf[["tool"]][["uctags"]][["keep_lines_type"]]

  # Window size or ranges
  window_size <- conf[["analysis"]][["window"]][["size_days"]]
  ranges <- conf[["analysis"]][["window"]][["ranges"]]

  # Read git log
  project_git <- data.table::fread(gitlog_path)

  # Filter files
  if (filter) {
    if (length(file_extensions) > 0) project_git <- project_git  %>% filter_by_file_extension(file_extensions,"file_pathname")
    if (length(substring_filepath) > 0) project_git <- project_git  %>%  filter_by_filepath_substring(substring_filepath,"file_pathname")
    if (length(filter_commit_size) > 0) project_git <- project_git %>% filter_by_commit_size(commit_size = filter_commit_size)
  }

  project_git$author_datetimetz <- as.POSIXct(project_git$author_datetimetz,
                                              format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")
  project_git$committer_datetimetz <- as.POSIXct(project_git$committer_datetimetz,
                                              format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")

  # The start and end dates correspond to the date of the earliest and latest commit, respectively.
  start_date <- min(project_git[[time]],na.rm=TRUE)
  end_date <- max(project_git[[time]],na.rm=TRUE)

  # Create time windows
  if (length(ranges) > 0) {
    # Use exact ranges if specified
    time_window <- as.POSIXct(ranges)
    size_time_window <- length(time_window)
  } else {
    # Create windows with a specified length if ranges are not specified
    # Define all timestamps in number of days since the very first commit of the repo
    # Format time window for posixT
    window_size_f <- stringi::stri_c(window_size," day")

    # Note if end_date is not (and will likely not be) a multiple of window_size,
    # then the ending incomplete window is discarded so the metrics are not calculated
    # in a smaller interval
    time_window <- seq.POSIXt(from=start_date,to=end_date,by=window_size_f)
    size_time_window <- length(time_window)
  }

  # Configure the cluster
  n.cores <- parallel::detectCores() # Use all available cores
  cluster <- parallel::makeCluster(n.cores, outfile="")
  registerDoSNOW(cluster) # Register the cluster for foreach loops

  # Progress bar
  pb <- txtProgressBar(min=0, max=size_time_window-1, style=3, char=" ", width=85)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  k <- 0
  foreach(
    j = 2:size_time_window,
    .packages = c("kaiaulu", "reactable", "data.table", "stringi", "cli"),
    .options.snow = opts
  ) %dopar% {
    i <- j - 1
    k <- k + 1

    # If the time window is of size 1, then there have been less than
    # "window_size_f" days from the start date.
    if(length(time_window) == 1){
      start_day <- start_date
      end_day <- end_date
    }else{
      start_day <- time_window[i]
      end_day <- time_window[j]
    }

    # Define the save path for the entity log
    start_day_formatted <- format(start_day, "%Y-%m-%d")
    end_day_formatted <- format(end_day, "%Y-%m-%d")
    entity_save_dir <- file.path(save_path, paste(start_day_formatted, end_day_formatted, sep="_"))
    dir.create(entity_save_dir, recursive = TRUE)
    save_file_name <- paste("entities_", start_day_formatted,
                            "_", end_day_formatted, ".csv", sep="")
    entity_save_path = file.path(entity_save_dir, save_file_name)

    # Obtain all commits from the gitlog which are within a particular window_size
    if (start_include && end_include) {
      project_git_slice <- project_git[(project_git[[time]] >= start_day) &
                                         (project_git[[time]] <= end_day), ]
    } else if (start_include && !end_include) {
      project_git_slice <- project_git[(project_git[[time]] >= start_day) &
                                         (project_git[[time]] < end_day), ]
    } else if (!start_include && end_include)  {
      project_git_slice <- project_git[(project_git[[time]] > start_day) &
                                         (project_git[[time]] <= end_day), ]
    } else {
      project_git_slice <- project_git[(project_git[[time]] > start_day) &
                                         (project_git[[time]] < end_day), ]
    }

    # Perform entity analysis
    changed_entities <- setNames(data.table(matrix(nrow = 0, ncol = 24)),
                                 c("identity_id",
                                   "author_name_email",
                                   "row_id",
                                   "commit_hash",
                                   "entity_definition_name",
                                   "entity_type",
                                   "entity_definition_line_start",
                                   "entity_definition_line_end",
                                   "author_name",
                                   "author_email",
                                   "author_timestamp",
                                   "author_tz",
                                   "committer_name",
                                   "committer_email",
                                   "committer_timestamp",
                                   "commiter_tz",
                                   "committer_summary",
                                   "n_lines_changed",
                                   "author_datetimetz",
                                   "committer_name_email",
                                   "committer_datetimetz",
                                   "entity",
                                   "weight",
                                   "raw_name"))

    if (nrow(project_git_slice) > 0) {
      changed_entities <- parse_gitlog_entity(git_repo_path,utags_path,project_git_slice,kinds,progress_bar=TRUE)

      if (nrow(changed_entities) > 0) {
        # Naming
        changed_entities[,c("author_name_email",
                            "author_datetimetz",
                            "committer_name_email",
                            "commit_hash",
                            "committer_datetimetz",
                            "entity",
                            "weight"):=list(stri_c(changed_entities$author_name,changed_entities$author_email,sep= " "),
                                            as.POSIXct(as.integer(changed_entities$author_timestamp),
                                                       origin="1970-01-01",tz = "UTC"),
                                            stri_c(changed_entities$committer_name,changed_entities$committer_email,sep= " "),
                                            changed_entities$commit_hash,
                                            as.POSIXct(as.integer(changed_entities$committer_timestamp),
                                                       origin="1970-01-01",tz = "UTC"),
                                            changed_entities$entity_definition_name,
                                            changed_entities$n_lines_changed
                                            )]

        # Identity match
        if (id_match){
          project_log <- list(project_git=changed_entities)
          project_log <- identity_match(project_log,
                                        name_column=id_columns,
                                        assign_exact_identity,
                                        use_name_only=id_names_only,
                                        label = "raw_name")
          changed_entities <- project_log[["project_git"]]
        }
      }
    }
    data.table::fwrite(changed_entities,entity_save_path)
  }

  close(pb)
  stopCluster(cluster)

  cli_alert_success(paste0("Changed entities were saved at: ", save_path))
}else if(arguments[["entity"]] & arguments[["help"]]){
  cli_alert_info("Parses changed entities using parse_gitlog_entity().
                 Optionally filters files and matches identities according to
                 the configuration.")
}else if(arguments[["entity"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  cli_path <- arguments[["<cli_conf.yml>"]]
  gitlog_path <- arguments[["<gitlog_file_name_path>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  cli <- yaml::read_yaml(cli_path)

  id_match <- cli[["git"]][["entity"]][["identity"]][["match"]]
  id_columns <- cli[["git"]][["entity"]][["identity"]][["columns"]]
  id_names_only <- cli[["git"]][["entity"]][["identity"]][["names_only"]]

  # 3rd Party Tools
  utags_path <- tool[["utags"]]

  # Local Git Repo Folder Path
  git_repo_path <- conf[["version_control"]][["log"]]

  # File Filters
  filter <- cli[["git"]][["entity"]][["filter"]]
  file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]
  substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]
  filter_commit_size <- conf[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  # Ctags Line Types
  kinds <- conf[["tool"]][["uctags"]][["keep_lines_type"]]

  # Read git log
  project_git <- data.table::fread(gitlog_path)

  # Filter files
  if (filter) {
    if (length(file_extensions) > 0) project_git <- project_git  %>% filter_by_file_extension(file_extensions,"file_pathname")
    if (length(substring_filepath) > 0) project_git <- project_git  %>%  filter_by_filepath_substring(substring_filepath,"file_pathname")
    if (length(filter_commit_size) > 0) project_git <- project_git %>% filter_by_commit_size(commit_size = filter_commit_size)
  }

  project_git$author_datetimetz <- as.POSIXct(project_git$author_datetimetz,
                                              format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")
  project_git$committer_datetimetz <- as.POSIXct(project_git$committer_datetimetz,
                                              format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")

  changed_entities <- parse_gitlog_entity(git_repo_path,utags_path,project_git,kinds,progress_bar=TRUE)

  # Identity match
  if (id_match){
    project_log <- list(project_git=changed_entities)
    project_log <- identity_match(project_log,
                                  name_column=id_columns,
                                  assign_exact_identity,
                                  use_name_only=id_names_only,
                                  label = "raw_name")
    changed_entities <- project_log[["project_git"]]
  }

  data.table::fwrite(changed_entities,save_path)
  cli_alert_success(paste0("Changed entities were saved at: ", save_path))
}
