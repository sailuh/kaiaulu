__kaiaulu [0.0.0.9600](https://github.com/sailuh/kaiaulu/milestone/5) (in development)__
=========================

### NEW FEATURES
 * DV8 Showcase vignette now uses gitlog and dependency functions transformers, which enable using Kaiaulu filters. [#184](https://github.com/sailuh/kaiaulu/issues/184)  
 * R/graph.R function was modularized to allow for different weight schemes. [#184](https://github.com/sailuh/kaiaulu/issues/184)  
 * Adds DV8 Notebook showcasing various DV8 Functions. The project configuration file of APR has been expanded to demonstrate available parameters for DV8. [#186](https://github.com/sailuh/kaiaulu/issues/186) and [#182](https://github.com/sailuh/kaiaulu/issues/182).  
 * Adds functions `dv8_clsxb_to_clsxj`, `parse_dv8_clusters`, `dependencies_to_sdsmj`, and `gitlog_to_hdsmj` for DV8 integration with Kaiaulu. [#168](https://github.com/sailuh/kaiaulu/issues/168) 
 * Adds functions `dv8_gitlog_to_gitnumstat` `dv8_gitnumstat_to_hdsmb` `dv8_hsdsmb_to_decoupling_level` `dv8_hsdsmb_to_hierclsxb` `dv8_hsdsmb_drhier_to_excel` `parse_dv8_metrics_decoupling_level` [#169](https://github.com/sailuh/kaiaulu/issues/169) 
 * Adds issue commit flow. See `issue_social_smell_showcase.Rmd` vignette for details. [#144](https://github.com/sailuh/kaiaulu/issues/144)
 * Adds a new `download_mod_mbox_per_month()` function which allows for the intermediate mbox downloaded files to be saved to the chosen folder (as opposed to tmp). The function is showcases on `download_mod_mbox.Rmd` vignette. [#141](https://github.com/sailuh/kaiaulu/issues/141)
 * A CLI interface for calculating smells over multiple branches was added. Consistent with other interfaces, the input is the tools.yml, the project configuration file, and the file save path. [#132](https://github.com/sailuh/kaiaulu/issues/132)
 * Re-implements the socio technical congruence metric, using the built-in graph model. [#137](https://github.com/sailuh/kaiaulu/issues/137)
 * Social smell notebook now uses the project configuration file to determine which kind of reply data (mbox, github issues and pull requests comments or jira issue comments) to use. Moreover, in case multiple branches are specified only the first (top will be executed). This fully automates the notebook based on the project configuration file. [#132](https://github.com/sailuh/kaiaulu/issues/132)
 * Kaiaulu now uses a new format for project configuration files which improves readability and account for new notebooks added during previous releases. More documentation was also added as comments to the project configuration file so it is more self contained. [#111](https://github.com/sailuh/kaiaulu/issues/111)
 * `download_github_comments.Rmd` now include author and committer name and e-mail to support identity matching. [#133](https://github.com/sailuh/kaiaulu/issues/133)
 * The social smell notebook now performs git checkout before `parse_gitlog`. The branch parameter, which is also used later in the notebook to reset the branch after performing git checkout to calculate line metrics, is now a project configuration file parameter. [#132](https://github.com/sailuh/kaiaulu/issues/132)
 * Combining JIRA Issue Comments, GitHub Issue Comments, GitHub Pull Request Comments, and Mailing Lists is now possible and showcased on the `social_smell_showcase.Rmd` Notebook. Moreover, both `download_jira_data.Rmd` and `download_github_comments.Rmd` have been standardized to provide the raw json data, whereas `parse_jira_replies()` and `parse_github_replies()` provide the same formatted `reply` table as `parse_mbox()`, which allows combining the various sources simply by using native `rbind()` function.  [#133](https://github.com/sailuh/kaiaulu/issues/133). 
 * Kaiaulu can now download project's communication from GitHub issue comments and GitHub pull request comments. See the new notebook `download_github_comments.Rmd` for example usage. [#130](https://github.com/sailuh/kaiaulu/issues/130)
 * A module to use the GitHub API has been added, built on top of the gh library. Three types of functions were added on as need basis: Functions to obtain an end point response, functions to iterate over the pages of the responses, and functions to parse the raw data format (json) into tables. The iterator function also provides an optional parameter to save the raw data. Note while the json data is provided "as-is", the parser functions only tabulates a portion of the data in the interest of time. If additional columns are needed for your particular, please open an issue.  [#86](https://github.com/sailuh/kaiaulu/issues/86)
 * Kaiaulu is now public. [#124](https://github.com/sailuh/kaiaulu/issues/124) 
 * A CLI has been added using Kaiaulu API. With this, users can use some of Kaiaulu features directly, without requiring knowledge of R. Available functionality is currently limited, and more will be added in the future based on user preference. [#123](https://github.com/sailuh/kaiaulu/issues/123)
 * With version 0.0.0.9600, Three social smells (org silo, missing link, and radio silence) are refactored to the master branch. The social smells no longer have a dependency to igraph, and OSLOM is used for community detection instead of igraph's random walk. Because of the closer integration to source, the social smell notebook includes a new section where any time slice can be explored to assess the social metrics, including coloring by community. A separate branch will contain a notebook comparing the re-implementation to the existing metric. While org silo and missing link should result in the same metric value, radio silence results will be different due to the use of a different community detection algorithm. [#114](https://github.com/sailuh/kaiaulu/issues/114).

### MINOR IMPROVEMENTS
 * Flaws folder is no longer hardcoded to "apr-"
 * DV8 Notebook no longer hardcodes project names to "apr-". [#190](https://github.com/sailuh/kaiaulu/issues/190)
 * Updated parse_dependencies() with `output_dir` folder for more flexibility. [#168](https://github.com/sailuh/kaiaulu/issues/168) 
 * Adds SetUp and TearDown unit tests for a sample git log. [#154](https://github.com/sailuh/kaiaulu/issues/154)
 * Added new citation for Kaiaulu work on README and references of works using Kaiaulu. [#143](https://github.com/sailuh/kaiaulu/issues/143)
 * Moves suggested rawdata folder assumed on configuration files one level above to avoid rawdata git logs being incorrectly parsed when parsing Kaiaulu architecture during documentation generation. Minor function documentation was also fixed. [#142](https://github.com/sailuh/kaiaulu/issues/142)
 * For multi-branch analysis, specifying a single commit hash will not work as it will only apply to a single branch. The CLI has been modified to rely on the start_datetime and end_datetime instead. [#132](https://github.com/sailuh/kaiaulu/issues/132)
 * a kaiaulu.conf has been added. Now that GitHub API is available, we can measure the social smells of the tool via the `social_smell_showcase.Rmd`. [#133](https://github.com/sailuh/kaiaulu/issues/133)
 * Sometimes, mbox files contain the e-mail body under the `body.plain` or `body.plain.simple`. The `parse_mbox()` function now handles both cases. [#133](https://github.com/sailuh/kaiaulu/issues/133)

### BUG FIXES
 
 * In the Gitlog explore Notebook, co-change was being calculated with the wrong weight scheme (weight sum). This is now fixed to eliminated node counts. [#184](https://github.com/sailuh/kaiaulu/issues/184)  
 * parse_dependencies() now returns a list of nodes and edgelist as opposed to just edgelist tables. Prior to this change, files that contain no dependency to other files would be missed (as they would not exist in the edgelist table, and only in the node table). This is consistent to both Depends and DV8 tables, as there is a 1 to 1 mapping from Depends/DV8 variables field on the generated JSON and the node table in Kaiaulu's graph memory representation, and the Depends/DV8 cells field, and Kaiaulu's graph edgelist table. [#189](https://github.com/sailuh/kaiaulu/issues/189). 
 * parse_gitlog() now renames files on their first commit renames, instead of when the renamed file is first modified. Prior logic led to situations where, when a file is renamed and never again modified, the new file name is never mentioned on the git log. This was due to how Perceval encoded file renaming, by including in a rarely present field called "newfile" instead of including the new file name under "file". [#184](https://github.com/sailuh/kaiaulu/issues/184)
 * parse_dependencies() no longer truncates full file paths, but instead turn them into relative paths. Dependencies notebook also now show a sample of the table and dependency graph. [#172](https://github.com/sailuh/kaiaulu/issues/172)   
 * parse_mbox() can now parse .mbox files that contain less fields [#185](https://github.com/sailuh/kaiaulu/issues/185)
 * The CLI interface for git and mailinglist has been updated to conform to the new project configuration file format. [#111](https://github.com/sailuh/kaiaulu/issues/111)
 * Fixes incorrect column name usage when calculating churn, which resulted in churn returning 0 as metric. [#135](https://github.com/sailuh/kaiaulu/issues/135)
 * If OSLOM detected developers to belong to more than one community, the radio silence function would throw warnings when said developer was a neighboor of others, choosing the first of the available groups. This is because the original smell function used a community detection algorithm which did not assign multiple groups. The warning is fixed by implementing what it did by default, i.e. choosing the first group of those assigned. In the future, the smell function can be improved to account for more than one group. [#134](https://github.com/sailuh/kaiaulu/issues/134)
 * The ordering of rows was currently done alphabetically over the date. It is now correctly done based on time. [#126](https://github.com/sailuh/kaiaulu/issues/126)
 * In `social_smell_showcase.Rmd`, the variables `i_commit_hash` and `j_commit_hash` were subject to the ordering of the rows as input. The code now correctly chooses the earliest date and latest date within a time window, instead of assuming the first row and last row are such. In turn, this now reflects in the correct commit hash interval being reported in the final table, and the correct git checkouts being applied to line metrics. [#126](https://github.com/sailuh/kaiaulu/issues/126)
 * In `smells.R`, used by `social_smell_showcase.Rmd`, smell_organizational_silo, smell_missing_links, and smell_radio_silence mapping of text to numerical identities was incorrect or missing. One side effect of this error as reported in the issue, is that different orderings of the rows provided as input to the function caused different metric values. However, the metric should be independent of the ordering regardless. This issue address the ordering side effect and corrects the metric value. [#126](https://github.com/sailuh/kaiaulu/issues/126)
 * In `download_jira_data.Rmd`, the jira issue downloader's output contained a mismatch between column names and values when converting the json to table. The conversion is now done in Kaiaulu instead of the external package, and the external package is only used to obtain the json. In addition, `parse_jira_comments()` has been refactored into `parse_jira()`, which handles both issues and/or comments jsons obtained from the external package. [#120](https://github.com/sailuh/kaiaulu/issues/120)
 * OSLOM now assign cluster ids to isolated nodes for consistency [#115](https://github.com/sailuh/kaiaulu/issues/115)

### DOCUMENTATION FIXES
 * Added new paper citation, and moved references from .bib to a vignette. 
 * README was substantially updated, and made more concise. Additional third party tool documentation was moved to the wiki where it can expand more freely. [#191](https://github.com/sailuh/kaiaulu/issues/191)
 * Kaiaulu docs now use the most recent version of pkgdown, which now includes a search field. 
 * Forward Referencing and Backward Referencing navigation across all functions have been added. "Check" was also used for incorrect referencing on ghost parameters or functions. Dangling stringr call was replaced by stringi. [#190](https://github.com/sailuh/kaiaulu/issues/190)
 * Add Malia Liu, and Nicholas Lee as contributors on DESCRIPTION file. 
 * Fixes various inconsistencies across documentation, missing parameter hyperlinking, seealso, etc. Functions in R/dv8.R were re-order to follow expected order of function call in an analysis, which is consistent to their ordering in _pkgdown.yml. [#186](https://github.com/sailuh/kaiaulu/issues/186)
 * New vignette for DV8 functions called "dv8.Rmd" [#168](https://github.com/sailuh/kaiaulu/issues/168) 
 * New configuration file for the Apache Thrift project called "thrift.yml". [#148](https://github.com/sailuh/kaiaulu/issues/148)
 * Fixed minor grammar mistakes and vague wording. [#151] (https://github.com/sailuh/kaiaulu/issues/151)
 
__kaiaulu [0.0.0.9500](https://github.com/sailuh/kaiaulu/milestone/5) __
=========================

### NEW FEATURES

  * `mailinglist_showcase.Rmd` has been renamed to `reply_communication_showcase.Rmd` to account for issue tracker network communication. Likewise, `transform_mbox_to_bipartite_network` has been renamed to `transform_reply_to_bipartite_network` to reflect accepting both mbox and jira reply data as parameter. The notebook also now presents how to load jira issue comment networks (obtained using `download_jira_data.Rmd`, and combining the networks. A new function, `parse_jira_comments()` was also added to standardized the input to conform to Kaiaulu nomenclature of communication data. [#113](https://github.com/sailuh/kaiaulu/issues/113)
  * mod_mbox_downloader now accepts a save_path compatible with the project configuration file instead of saving to working directory, and has a verbose mode to display progress. A new notebook showcasing how to use the function has also been added `download_mod_mbox.Rmd`. [#112](https://github.com/sailuh/kaiaulu/issues/112)
  * Two new R notebooks, `download_jira_data.Rmd` and `bug_count.Rmd`, and one project configuration file, `geronimo.yml` now demonstrate how JIRA issue data can be downloaded and used to calculate file bug count using existing Kaiaulu functionality and an external JIRA API R package. In combination with the existing `gitlog_vulnerabilities_showcase.Rmd`, Kaiaulu can now download and parse both software vulnerabilities (CVEs) and issue IDs. The `download_jira_data.Rmd` can also be used to obtain issue comment data, which may be used to construct communication networks in combination to mailing list data. [#110](https://github.com/sailuh/kaiaulu/issues/110)
  * Existing network visualizations can now be re-colored using `recolor_network_by_community`. [#94](https://github.com/sailuh/kaiaulu/issues/94)
  * Added `download_mod_mbox()` function to download.R module, allowing the composition of .mbox files from Apache mod\_mbox archives. [#99](https://github.com/sailuh/kaiaulu/issues/93).
  * Added download.R module enabling downloading and conversion of pipermail archives into the .mbox format using the `download_pipermail()` and `convert_pipermail_to_mbox` functions [#93](https://github.com/sailuh/kaiaulu/issues/93).
  * adds built-in bipartite graph projection transformation to `graph.R` `bipartite_graph_projection()` [#75](https://github.com/sailuh/kaiaulu/issues/75). 
  * `parser.R` and `network.R` API now abide by a standardized nomenclature for the data columns, instead of using third party software nomenclature, which led to multiple names when data overlapped among third party software. The Network module function prefix was also replaced from parse_\*_network to transform_\*_network. Various transformation functions were also renamed to explicitly indicate it generates bipartite networks (previously it did not), instead of temporal. The network functions to transform git logs, be it bipartite or temporal now account for all types of networks (i.e. author-file, author-entity, committer-file, committer-entity, etc). The "mode" parameter is also more explicit on what types of functions it can create. [#43](https://github.com/sailuh/kaiaulu/issues/43)

  * Parser functions no longer normalize the timezone to UTC. This is now exemplified in all Notebooks instead for when time slices are needed. Therefore, it is now possible to implement the socio-technical metric `num.tz`. To minimize risk timestamps are no longer aligned, datetimes are left as strings instead of parsed as posix.ct objects. [#89](https://github.com/sailuh/kaiaulu/issues/89) 

### MINOR IMPROVEMENTS

 * All notebooks now use the new identity match interface from [#56](https://github.com/sailuh/kaiaulu/issues/56), consequently users can now choose to display to either bipartite or temporal transformations whether to display the nodes with the project's name and e-mail or their id, if publishing information online to protect the project's developers privacy. [#90](https://github.com/sailuh/kaiaulu/issues/90)
 
 * Fixes the column naming for the `parse_dependencies()`. Previously `src` and `dest`, and now `from` and `to`, consistent to other networks derived from `graph.R`. [#75](https://github.com/sailuh/kaiaulu/issues/75)
 
 * Fixes tools.yml to use the correct `undir` and `dir` of OSLOM (previously the paths were inverted). [#75](https://github.com/sailuh/kaiaulu/issues/75)

### BUG FIXES

 * Fixes incorrect datetime assignment from committer to author in `gitlog_showcase.Rmd`. [#110](https://github.com/sailuh/kaiaulu/issues/110)
 * Fixes outdated column names in `commit_message_id_coverage`. [#110](https://github.com/sailuh/kaiaulu/issues/110)
 * Fixes `download_mod_mbox` missing leading zeros. [#107](https://github.com/sailuh/kaiaulu/issues/107)

### DOCUMENTATION FIXES

 * CONTRIBUTING.md now contains details on how to contribute code to Kaiaulu. [#102](https://github.com/sailuh/kaiaulu/issues/102) 
 * README.md has been updated to reflect current functionality, examples and how to cite. A pointer to this NEWS.md file has also been added. [#105](https://github.com/sailuh/kaiaulu/issues/105)
 * the gitlog_showcase Notebook was renamed to "Explore Git Log", and now contains extensive textual documentation explaining all the file functions, both bipartite and temporal. It also briefly introduces the information used from the project configuration file. Some notebooks which had redundant content were also deleted and re-organized on this one. The software vulnerabilities notebook was also renamed to "Issues, Software Vulnerabilities and Weaknesses", and now focuses on commit log message parsing only. The notebook which presents the method to parse git log entities was renamed to "Extending Git Logs from Files to Entities", it was also reorganized so as to not depend on a saved local rds file. It now loads a very small amount of data so the documentation generation does not take too long as the processing of a full log takes awhile. [#91](https://github.com/sailuh/kaiaulu/issues/91)


__kaiaulu [0.0.0.9000](https://github.com/sailuh/kaiaulu/milestone/1) (04/24/2021)__
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
