#!/usr/local/bin/Rscript

# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

seed <- 1
set.seed(seed)
require(kaiaulu,quietly=TRUE)
require(stringi,quietly=TRUE)
require(data.table,quietly=TRUE)
require(knitr,quietly=TRUE)
require(visNetwork,quietly=TRUE)
require(cli,quietly=TRUE)


doc <- "
USAGE:
  smells.R tabulate help
  smells.R all_branches <tools.yml> <project_conf.yml> <save_file_name_path>
  smells.R (-h | --help)
  smells.R --version

DESCRIPTION:
  Provides a suite of functions to interact with Social Smells. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"



arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')
if(arguments[["all_branches"]] & arguments[["help"]]){
  cli_alert_info("Tabulates smells of all branches (see social smells notebook for details).")
}else if(arguments[["all_branches"]]){

  # Load Config Parameters
  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]
  save_path <- arguments[["<save_file_name_path>"]]

  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)

  scc_path <- tool[["scc"]]

  oslom_dir_path <- tool[["oslom_dir"]]
  oslom_undir_path <- tool[["oslom_undir"]]

  perceval_path <- tool[["perceval"]]
  git_repo_path <- conf[["version_control"]][["log"]]
  git_branches <- conf[["version_control"]][["branch"]]

  #start_commit <- conf[["analysis"]][["window"]][["start_commit"]]
  start_date <- conf[["analysis"]][["window"]][["start_datetime"]]
  #end_commit <- conf[["analysis"]][["window"]][["end_commit"]]
  end_date <- conf[["analysis"]][["window"]][["end_datetime"]]
  window_size <- conf[["analysis"]][["window"]][["size_days"]]

  mbox_path <- conf[["mailing_list"]][["mbox"]]
  github_replies_path <- conf[["issue_tracker"]][["github"]][["replies"]]
  jira_issue_comments_path <- conf[["issue_tracker"]][["jira"]][["issue_comments"]]

  ## Filters
  file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]
  substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]

  all_branches_smells_interval <- NULL
  for(git_branch in git_branches){

    cli_alert_info(paste0("Starting to calculate smells for branch: [",git_branch,"]"))

    # Parse and filter Git Log #
    git_checkout(git_branch,git_repo_path)
    project_git <- parse_gitlog(perceval_path,git_repo_path)
    project_git <- project_git  %>%
      filter_by_file_extension(file_extensions,"file_pathname")  %>%
      filter_by_filepath_substring(substring_filepath,"file_pathname")


    # Parse Git Log Timestamps #

    project_git$author_tz <- sapply(stringi::stri_split(project_git$author_datetimetz,
                                                        regex=" "),"[[",6)
    project_git$author_datetimetz <- as.POSIXct(project_git$author_datetimetz,
                                                format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


    project_git$committer_tz <- sapply(stringi::stri_split(project_git$committer_datetimetz,
                                                           regex=" "),"[[",6)
    project_git$committer_datetimetz <- as.POSIXct(project_git$committer_datetimetz,
                                                   format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


    # Parse Replies and Timestamps #

    project_mbox <- NULL
    project_jira <- NULL
    project_github_replies <- NULL



    if(!is.null(mbox_path)){
      project_mbox <- parse_mbox(perceval_path,mbox_path)

      project_mbox$reply_tz <- sapply(stringi::stri_split(project_git$reply_datetimetz,
                                                          regex=" "),"[[",6)

      project_mbox$reply_datetimetz <- as.POSIXct(project_mbox$reply_datetimetz,
                                                  format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC")


    }
    if(!is.null(jira_issue_comments_path)){
      project_jira <- parse_jira_replies(parse_jira(jira_issue_comments_path))

      # Timezone is embedded on separated field. All times shown in UTC.
      project_jira$reply_tz <- "0000"

      project_jira$reply_datetimetz <- as.POSIXct(project_jira$reply_datetimetz,
                                                  format = "%Y-%m-%dT%H:%M:%S.000+0000", tz = "UTC")
    }
    if(!is.null(github_replies_path)){
      project_github_replies <- parse_github_replies(github_replies_path)


      # Timezone is not available on GitHub timestamp, all in UTC
      project_github_replies$reply_tz <- "0000"

      project_github_replies$reply_datetimetz <- as.POSIXct(project_github_replies$reply_datetimetz,
                                                            format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    }

    # Combine Replies #

    project_reply <- rbind(project_mbox,
                           project_jira,
                           project_github_replies)

    project_git <- project_git[order(author_datetimetz)]

    project_reply <- project_reply[order(reply_datetimetz)]

    # Read Window Size

    window_size <- window_size # 90 days

    # Perform Identity Match #

    project_log <- list(project_git=project_git,project_reply=project_reply)
    project_log <- identity_match(project_log,
                                  name_column = c("author_name_email","reply_from"),
                                  assign_exact_identity,
                                  use_name_only=TRUE,
                                  label = "raw_name")

    project_git <- project_log[["project_git"]]
    project_reply <- project_log[["project_reply"]]

    # Calculate Smells #

    #start_date <- get_date_from_commit_hash(project_git,start_commit)
    start_date <- as.POSIXct(start_date,format="%Y-%m-%d %H:%M:%S",tz="UTC")
    #end_date <- get_date_from_commit_hash(project_git,end_commit)
    end_date <- as.POSIXct(end_date,format="%Y-%m-%d %H:%M:%S",tz="UTC")
    datetimes <- project_git$author_datetimetz
    reply_datetimes <- project_reply$reply_datetimetz

    # Format time window for posixT
    window_size_f <- stringi::stri_c(window_size," day")

    # Note if end_date is not (and will likely not be) a multiple of window_size,
    # then the ending incomplete window is discarded so the metrics are not calculated
    # in a smaller interval
    time_window <- seq.POSIXt(from=start_date,to=end_date,by=window_size_f)

    # Create a list where each element is the social smells calculated for a given commit hash
    smells <- list()
    size_time_window <- length(time_window)
    for(j in 2:size_time_window){

      # Initialize
      commit_interval <- NA
      start_day <- NA
      end_day <- NA
      org_silo <- NA
      missing_links <- NA
      radio_silence <- NA
      primma_donna <- NA
      st_congruence <- NA
      communicability <- NA
      num_tz <- NA
      code_only_devs <- NA
      code_files <- NA
      ml_only_devs <- NA
      ml_threads <- NA
      code_ml_both_devs <- NA

      i <- j - 1

      # If the time window is of size 1, then there has been less than "window_size_f"
      # days from the start date.
      if(length(time_window)  == 1){
        # Below 3 month size
        start_day <- start_date
        end_day <- end_date
      }else{
        start_day <- time_window[i]
        end_day <- time_window[j]
      }


      # Obtain all commits from the gitlog which are within a particular window_size
      project_git_slice <- project_git[(author_datetimetz >= start_day) &
                                         (author_datetimetz < end_day)]

      # Obtain all email posts from the reply which are within a particular window_size
      project_reply_slice <- project_reply[(reply_datetimetz >= start_day) &
                                             (reply_datetimetz < end_day)]

      # Check if slices contain data
      gitlog_exist <- (nrow(project_git_slice) != 0)
      ml_exist <- (nrow(project_reply_slice) != 0)


      # Create Networks
      if(gitlog_exist){
        i_commit_hash <- data.table::first(project_git_slice[author_datetimetz == min(project_git_slice$author_datetimetz,na.rm=TRUE)])$commit_hash

        j_commit_hash <- data.table::first(project_git_slice[author_datetimetz == max(project_git_slice$author_datetimetz,na.rm=TRUE)])$commit_hash

        # Parse networks edgelist from extracted data
        network_git_slice <- transform_gitlog_to_bipartite_network(project_git_slice,
                                                                   mode="author-file")

        # Community Smells functions are defined base of the projection networks of
        # dev-thread => dev-dev, and dev-file => dev-dev. This creates both dev-dev via graph projections

        git_network_authors <- bipartite_graph_projection(network_git_slice,
                                                          mode = TRUE,
                                                          weight_scheme_function = weight_scheme_sum_edges)

        code_clusters <- community_oslom(oslom_undir_path,
                                         git_network_authors,
                                         seed=seed,
                                         n_runs = 1000,
                                         is_weighted = TRUE)

      }
      if(ml_exist){
        network_reply_slice <- transform_reply_to_bipartite_network(project_reply_slice)


        reply_network_authors <- bipartite_graph_projection(network_reply_slice,
                                                            mode = TRUE,
                                                            weight_scheme_function = weight_scheme_sum_edges)

        # Community Detection

        mail_clusters <- community_oslom(oslom_undir_path,
                                         reply_network_authors,
                                         seed=seed,
                                         n_runs = 1000,
                                         is_weighted = TRUE)

      }
      # Metrics #

      if(gitlog_exist){
        commit_interval <- stri_c(i_commit_hash,"-",j_commit_hash)
        # Social Network Metrics
        code_only_devs <- length(unique(project_git_slice$identity_id))
        code_files <- length(unique(project_git_slice$file_pathname))

      }
      if(ml_exist){
        # Smell

        radio_silence <- length(smell_radio_silence(mail.graph=reply_network_authors,
                                                    clusters=mail_clusters))

        # Social Technical Metrics
        ml_only_devs <- length(unique(project_reply_slice$identity_id))
        ml_threads <- length(unique(project_reply_slice$reply_subject))
      }
      if (ml_exist & gitlog_exist){
        # Smells
        org_silo <- length(smell_organizational_silo(mail.graph=reply_network_authors,
                                                     code.graph=git_network_authors))

        missing_links <- length(smell_missing_links(mail.graph=reply_network_authors,
                                                    code.graph=git_network_authors))
        # Social Technical Metrics
        st_congruence <- smell_sociotechnical_congruence(mail.graph=reply_network_authors,
                                                         code.graph=git_network_authors)
        #    communicability <- community_metric_mean_communicability(reply_network_authors,git_network_authors)
        num_tz <- length(unique(c(project_git_slice$author_tz,
                                  project_git_slice$committer_tz,
                                  project_reply_slice$reply_tz)))
        code_ml_both_devs <- length(intersect(unique(project_git_slice$identity_id),
                                              unique(project_reply_slice$identity_id)))

      }

      # Aggregate Metrics
      smells[[stringi::stri_c(start_day,"|",end_day)]] <- data.table(commit_interval,
                                                                     start_datetime = start_day,
                                                                     end_datetime = end_day,
                                                                     org_silo,
                                                                     missing_links,
                                                                     radio_silence,
                                                                     #primma_donna,
                                                                     st_congruence,
                                                                     #communicability,
                                                                     num_tz,
                                                                     code_only_devs,
                                                                     code_files,
                                                                     ml_only_devs,
                                                                     ml_threads,
                                                                     code_ml_both_devs)
    }
    smells_interval <- rbindlist(smells)
    smells_interval$branch <- git_branch
    all_branches_smells_interval <- rbind(all_branches_smells_interval,
                                          smells_interval)
  # End of per branch loop socio smells
  }



  # End all branches Smell Calculation #

  cli_alert_success(paste0("Tabulated smells of all branches was saved at: ",save_path))

  data.table::fwrite(all_branches_smells_interval,save_path)
}
