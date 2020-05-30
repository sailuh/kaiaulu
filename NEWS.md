kaiaulu [0.0.0.9000](https://github.com/sailuh/kaiaulu/milestone/1) (in development)
=========================

### NEW FEATURES

  # add CONTRIBUTION.md and some tips on signed-off-by via `commit -s` #36 
  * add NEWS.md #37
  * churn metric is now available `metric_churn()` and `metric_commit_interval_churn()`. See vignettes/churn_metrics.Rmd for details. #19
  * provides gitlog data via interface to Perceval `parse_gitlog()`, and edgelist export for network libraries `parse_gitlog_network()`. See vignettes/gitlog_showcase.Rmd for details. #1
  * provides mailing list data via interface to Perceval `parse_mbox()`, and edgelist export for network libraries `parse_mbox_network()`. See vignettes/mailinglist_showcase.Rmd for details. #4
  * provides file dependency data via interface to Depends `parse_dependencies()`, and edgelist export for network libraries `parse_dependencies_network()`. See vignettes/depends_showcase.Rmd for details. #8
  * project is now licensed under MPL 2.0 #12
  

### MINOR IMPROVEMENTS

  * added a logo to the project. #35
  * removed unecessary step to parse gitlogs from Perceval. #33
  * igraph is no longer a dependence of the package. `parse_log_*()` functions now provide edgelist instead of igraph objects. vignettes were adjusted to showcase usage. #14
  * lubridate dependency was removed, this package now uses base R POSIXct to handle dates. #13
  * stringr was replaced by stringi to respect license terms of this and stringr packages. #21

### BUG FIXES

  * non defined function parameter on mbox has been fixed. (#25)
  * incorrect parameter removal of .git has been fixed. (#22)

### DOCUMENTATION FIXES

  * README.md now provides example vignettes according to data of interest. (#24)
