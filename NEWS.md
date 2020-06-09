kaiaulu [0.0.0.9000](https://github.com/sailuh/kaiaulu/milestone/1) (in development)
=========================

### NEW FEATURES

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
