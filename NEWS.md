kaiaulu [0.0.0.9500](https://github.com/sailuh/kaiaulu/milestone/5) (in development)
=========================

### NEW FEATURES

  * `parser.R` and `network.R` API now abide by a standardized nomenclature for the data columns, instead of using third party software nomenclature, which led to multiple names when data overlapped among third party software. The Network module function prefix was also replaced from parse_\*_network to transform_\*_network. Various transformation functions were also renamed to explicitly indicate it generates bipartite networks (previously it did not), instead of temporal. The network functions to transform git logs, be it bipartite or temporal now account for all types of networks (i.e. author-file, author-entity, committer-file, committer-entity, etc). The "mode" parameter is also more explicit on what types of functions it can create. [#43](https://github.com/sailuh/kaiaulu/issues/43)

  * Parser functions no longer normalize the timezone to UTC. This is now exemplified in all Notebooks instead for when time slices are needed. Therefore, it is now possible to implement the socio-technical metric `num.tz`. To minimize risk timestamps are no longer aligned, datetimes are left as strings instead of parsed as posix.ct objects. [#89](https://github.com/sailuh/kaiaulu/issues/89) 

### MINOR IMPROVEMENTS

 * All notebooks now use the new identity match interface from [#56](https://github.com/sailuh/kaiaulu/issues/56), consequently users can now choose to display to either bipartite or temporal transformations whether to display the nodes with the project's name and e-mail or their id, if publishing information online to protect the project's developers privacy. [#90](https://github.com/sailuh/kaiaulu/issues/90)

### BUG FIXES

* `identity_match()` was performed on the slice level, resulting in non-congruent identity_id's across slices. Using the mail-adresses and names in the slices introduces the noise that `identity_match()` normally solves. Identity match is now performed across all git log and mbox instead of slices. [#97](https://github.com/sailuh/kaiaulu/issues/97)

### DOCUMENTATION FIXES

 * the gitlog_showcase Notebook was renamed to "Explore Git Log", and now contains extensive textual documentation explaining all the file functions, both bipartite and temporal. It also briefly introduces the information used from the project configuration file. Some notebooks which had redundant content were also deleted and re-organized on this one. The software vulnerabilities notebook was also renamed to "Issues, Software Vulnerabilities and Weaknesses", and now focuses on commit log message parsing only. The notebook which presents the method to parse git log entities was renamed to "Extending Git Logs from Files to Entities", it was also reorganized so as to not depend on a saved local rds file. It now loads a very small amount of data so the documentation generation does not take too long as the processing of a full log takes awhile. [#91](https://github.com/sailuh/kaiaulu/issues/91)


kaiaulu [0.0.0.9000](https://github.com/sailuh/kaiaulu/milestone/1) (04/24/2021)
=========================

### NEW FEATURES
  * added a dependencies parser for R using utils abstract syntax tree parser, and API functions. Kaiaulu architecture is used for showcase, which should facilitate understanding new functions being added and their dependencies. See kaiaulu_architecture.Rmd for details. [#84](https://github.com/sailuh/kaiaulu/issues/84)
  * added an interface to OSLOM community detection algorithm. [#81](https://github.com/sailuh/kaiaulu/issues/81)
  * added git log parser at lower granularity, entity (e.g. function git log parser) `parse_gitlog_entity()`, and associated network 
  functions to visualize both author-entity bipartite network `parse_gitlog_entity_network()` and temporal `parse_gitlog_entity_temporal_network()`. It is therefore now possible to compare networks at file or any type of entity of interest, with different network construction methods. See vignettes/gitlog_entity_showcase.Rmd for details. [#79](https://github.com/sailuh/kaiaulu/issues/79)
  * added a new `parse_gitlog_temporal_network()` which provides a directed network for collaboration at file level. [#78](https://github.com/sailuh/kaiaulu/issues/78)
  * modify `parse_line_type()` to `parse_line_type_file()` to take as input information from git history instead of a local computer file, so it can be used to analyze git log changes. [#2](https://github.com/sailuh/kaiaulu/issues/2)
  * add `git_blame()` wrapper and parser. [#68](https://github.com/sailuh/kaiaulu/issues/68)
  * several fixes and improvements to R/string.R to assign identity under different ways to define name and e-mail between different sources. All tests now pass, and assign_exact_identity() can also perform identity match based on name only, should the e-mails be redacted (e.g. Google Groups) or missing. [#72](https://github.com/sailuh/kaiaulu/issues/72)  
  * add unit tests framework testthat and several tests for identity service. [#38](https://github.com/sailuh/kaiaulu/issues/38)
  * add new module R/git.R to facilitate checking current branch `git_head()` and checkout to a particular commit `git_checkout()`, the later required to analyze multiple intervals with static code analysis such as `parse_line_metrics()` and `parse_dependencies()`. A vignette will be added at a later date showcasing the functions. [#62](https://github.com/sailuh/kaiaulu/issues/62)
  * add source code line type identification using universal ctags `parse_line_type()`. See line_type_showcase.Rmd for usage. [#60](https://github.com/sailuh/kaiaulu/issues/60)
  * adds various file line metrics `parse_line_metrics()`. See line_metrics_showcase.Rmd for example usage. [#59](https://github.com/sailuh/kaiaulu/issues/59)
  * adds gitlog parser for java code refactorings `parse_java_code_refactoring_json()`. See refactoringminer_showcase.Rmd for example usage. [#57](https://github.com/sailuh/kaiaulu/issues/57)
  * file-cve-cwe networks can now be obtained by parsing nvd feeds for cve-cwe mapping `parse_nvdfeed()` and `parse_cve_cwe_file_network()`. See gitlog_showcase.Rmd for example usage. [#51](https://github.com/sailuh/kaiaulu/issues/51)
  * users can now specify the dependency types they wish to see for Depends on config file [#49](https://github.com/sailuh/kaiaulu/issues/49)
  * the number of commit messages which contains a given id can now be computed with `commit_message_id_coverage()`. See example on gitlog_showcase.Rmd. [#46](https://github.com/sailuh/kaiaulu/issues/46)
  * git log commit messages can now be parsed `parse_commit_message_id_network()`. example of interesting labels are issue ids and cve-ids. You can now also specify them directly on the config files (see conf folder). Vignettes/gitlog_showcase.Rmd has been updated to showcase a cve-id network. [#46](https://github.com/sailuh/kaiaulu/issues/46)
  * adds a built-in R static code parser relying on base R Abstract Syntax Tree Parser (vignette will be added in a future commit showcasing the network). [#47](https://github.com/sailuh/kaiaulu/issues/47) 
  * a new vignettes/interval_and_metric_showcase.Rmd was added to replace vignettes/churn_metrics.Rmd [#19](https://github.com/sailuh/kaiaulu/issues/19).
  * churn metric functions in R/metric.R, `metric_churn_per_commit_interval()` `metric_churn_per_commit_per_file()` logic were substantially simplified, and can now be used with interval/R [#19](https://github.com/sailuh/kaiaulu/issues/19). 
  * add minimal interval analysis support with `interval.R/interval_commit_metric()` and `parsers.R/filter_by_commit_interval()`. [#44](https://github.com/sailuh/kaiaulu/issues/44) 
  * add filters `filter_by_file_extension()` `filter_by_filepath_substring()` for files not relevant for metrics. config file schema also has been extended to provide parameters to the filters. [#30](https://github.com/sailuh/kaiaulu/issues/30)
  * config files per project have been defined, and used across all showcase vignettes. [#41](https://github.com/sailuh/kaiaulu/issues/41)
  * add a simple identity mapping function, `assign_identity()`, which assigns a single id from authors who use different names and emails in `parse_gitlog()`, `parse_mbox()`, or across both data. This allows `parse_gitlog_network()` and `parse_mbox_network()` to be merged into a single network. See vignettes/merging_networks_showcase.Rmd for details. A  normalized edit distance function was also added for future implementation of partial matching `normalized_levenshtein()`. [#31](https://github.com/sailuh/kaiaulu/issues/31)
  * add CONTRIBUTION.md and some tips on signed-off-by via `commit -s`. [#36](https://github.com/sailuh/kaiaulu/issues/36)
  * add NEWS.md [#37](https://github.com/sailuh/kaiaulu/issues/37)
  * churn metric is now available. `metric_churn()` and `metric_commit_interval_churn()`. See vignettes/churn_metrics.Rmd for details. [#19](https://github.com/sailuh/kaiaulu/issues/19)
  * provides gitlog data via interface to Perceval `parse_gitlog()`, and edgelist export for network libraries `parse_gitlog_network()`. See vignettes/gitlog_showcase.Rmd for details. [#1](https://github.com/sailuh/kaiaulu/issues/1)
  * provides mailing list data via interface to Perceval `parse_mbox()`, and edgelist export for network libraries `parse_mbox_network()`. See vignettes/mailinglist_showcase.Rmd for details. [#4](https://github.com/sailuh/kaiaulu/issues/4)
  * provides file dependency data via interface to Depends `parse_dependencies()`, and edgelist export for network libraries `parse_dependencies_network()`. See vignettes/depends_showcase.Rmd for details. [#8](https://github.com/sailuh/kaiaulu/issues/8)
  * project is now licensed under MPL 2.0 [#12](https://github.com/sailuh/kaiaulu/issues/12)
  

### MINOR IMPROVEMENTS

  * heavily refactored R/network.R API into R/graph.R to separate graph representation and algorithms from various types of networks that can be constructed from git logs, mailing lists, etc. [#81](https://github.com/sailuh/kaiaulu/issues/81)
  * refactored git_log() from parse_gilog and parse_commit_message_id() from R/parsers.R. [#74](https://github.com/sailuh/kaiaulu/issues/74)
  * various functions were moved to different files to clarify the API. [#73](https://github.com/sailuh/kaiaulu/issues/73)
  * config files were refactored for clarity and to accomodate dv8 wrapper. [#50](https://github.com/sailuh/kaiaulu/issues/50)
  * vignettes dependencies_showcase.Rmd and gitlog_showcase.Rmd now also make use of the chosen heuristics to filter files. Up to this point only interval_and_metric_showcase.Rmd used them. [#30](https://github.com/sailuh/kaiaulu/issues/30) 
  * various functions which assumed tables to have certain column names now require the name by parameter. this is work in progress to define a common interface as more data parsing is added to this codebase. [#43](https://github.com/sailuh/kaiaulu/issues/43)
  * added a logo to the project. [#35](https://github.com/sailuh/kaiaulu/issues/35)
  * removed unecessary step to parse gitlogs from Perceval. [#33](https://github.com/sailuh/kaiaulu/issues/33)
  * igraph is no longer a dependence of the package. `parse_log_*()` functions now provide edgelist instead of igraph objects. vignettes were adjusted to showcase usage. [#14](https://github.com/sailuh/kaiaulu/issues/14)
  * lubridate dependency was removed, this package now uses base R POSIXct to handle dates. [#13](https://github.com/sailuh/kaiaulu/issues/13)
  * stringr was replaced by stringi to respect license terms of this and stringr packages. [#21](https://github.com/sailuh/kaiaulu/issues/21)

### BUG FIXES

  * non defined function parameter on mbox has been fixed. [#25](https://github.com/sailuh/kaiaulu/issues/25)
  * incorrect parameter removal of .git has been fixed. [#22](https://github.com/sailuh/kaiaulu/issues/22)

### DOCUMENTATION FIXES

  * add pkgdown documentation to repo [#26](https://github.com/sailuh/kaiaulu/issues/26)
  * README.md now provides example vignettes according to data of interest. [#24](https://github.com/sailuh/kaiaulu/issues/24)
