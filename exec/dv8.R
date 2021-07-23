#!/usr/bin/env Rscript
library(kaiaulu)
library(stringi)
library(yaml)
library(cli)
library(data.table)

parse_flaws <- function(dv8_path,
                        sourceCodePath,
                        language,
                        inputFolder = "/tmp/",
                        outputFolder = "/tmp/",
                        projectName = "Architecture-analysis-result",
                        cochangeMaxThreshold = 30,
                        crossingCochange = "2",
                        crossingFanIn = "4",
                        crossingFanOut = "4",
                        uiHistoryImpact = "10",
                        mvCochange = "2",
                        uiStructImpact = "0.01",
                        uiCochange = "2",
                        cliqueDepends = "call,use",
                        uihDepends = "call,use",
                        uihInheritance = "extend,implement,public,private,virtual public"
){

  sourceCodePath <- path.expand(sourceCodePath)
  # Remove ".git"
  sourceCodePath <- stri_replace_last(git_repo_path,replacement="",regex=".git")

  cli_alert("Creating project-archreport.properties...")
  # Create project-archreport.properties
  f_con <- file("/tmp/project-archreport.properties")
  archreport_properties <- stri_c("\noutputFolder: ",outputFolder,
                                  "\nprojectName: ",projectName,
                                  "\nsourceType: code",
                                  "\nsourceCodePath: ",sourceCodePath,
                                  "\nsourceCodeLanguage: ",language,
                                  "\nrunMetrics: on",
                                  "\nrunArchissue: on",
                                  "\nrunArchroot: off",
                                  "\nrunHotspot: off",
                                  "\nrunReportDoc: off",
                                  "\nrunClustering: on",
                                  "\nrunFileStat: on",
                                  "\nrunCompress: off",
                                  "\nrevisionHistoryCochangeThreshold: ",cochangeMaxThreshold,
                                  "\nrunCleanUpStructuralOutput: on",
                                  #"\clusteringOutputFormat: clsx",
                                  #"\clusteringRecursive: on",
                                  "\n#revisionHistoryStart: 2016-01-01T00:00:00Z",
                                  "\n#revisionHistoryStop: 2016-01-01T00:00:00Z",
                                  "\narchissueCrossingCochange: ",crossingCochange,
                                  "\narchissueCrossingFanIn: ",crossingFanIn,
                                  "\narchissueCrossingFanOut: ",crossingFanOut,
                                  "\narchissueUiHistoryImpact: ",uiHistoryImpact,
                                  "\narchissueMvCochange: ",mvCochange,
                                  "\narchissueUiStructImpact: ",uiStructImpact,
                                  "\narchissueUiCochange: ",uiCochange,
                                  "\narchissueCliqueDepends: ",cliqueDepends,
                                  "\narchissueUihDepends: ",uihDepends,
                                  "\narchissueUihInheritance: ",uihInheritance)
  writeLines(archreport_properties,f_con)
  close(f_con)
  cli_alert_success("Finished! => /tmp/project-archreport.properties")
  cli_alert("Calculating DV8 Flaws and metrics...")
  # Sys call dv8 to run arch-report
  # /Applications/DV84/bin/dv8-console arch-report -paramsFile project-archreport.properties
  system2("sh",
          args = c(dv8_path,"arch-report",
                   "-paramsFile",
                   "/tmp/project-archreport.properties")
  )
  cli_alert_success("Finished! => /tmp/Architecture-analysis-result")
  #cli_alert("Replacing `.` with `/` and file extension `_` for `.` on Architecture-analysis-report-per-file.csv")
  #flaws <- fread("/tmp/Architecture-analysis-result/Architecture-analysis-report-per-file.csv")
  flaws <- fread("/tmp/Architecture-analysis-result/dv8-analysis-result/file-measure-report.csv")
  #flaws$FileName <- stri_replace_all_regex(str = flaws$FileName,pattern ="\\.",replacement = "/")
  #flaws$FileName <- stri_replace_last_regex(str = flaws$FileName,pattern ="_",replacement = "\\.")
  flaws <- fwrite(flaws,"/tmp/dv8_flaws.csv")

  # collect the Flaws here: /tmp/Architecture-analysis-result/issue/arch-issue/ArchIssue-summary.csv
  # Collect the metrics here: /tmp/Architecture-analysis-result/metrics/decoupling-level.json
  # Collect the metrics here: /tmp/Architecture-analysis-result/metrics/independence-level.json
  # Collect the metrics here: /tmp/Architecture-analysis-result/metrics/propagation-cost.json
}


# Main

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
cli_alert("Parsing config file...")
tool <- yaml::read_yaml(args[1])
conf <- yaml::read_yaml(args[2])


# Git repo
git_repo_path <- conf[["data_path"]][["git"]]

# Depends parameters
depends_jar_path <- tool[["depends"]]
language <- conf[["tool"]][["depends"]][["code_language"]]
keep_dependencies_type <- conf[["tool"]][["depends"]][["keep_dependencies_type"]]

# DV8
dv8_path <- tool[["dv8"]]

# Filters
file_extensions <- conf[["filter"]][["keep_filepaths_ending_with"]]
substring_filepath <- conf[["filter"]][["remove_filepaths_containing"]]

cli_alert_success("Finished parsing config file!")
parse_flaws(dv8_path,
            git_repo_path,
            language)
