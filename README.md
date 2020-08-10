# Kaiāulu <a href="https://github.com/sailuh/kaiaulu"><img src="man/figures/logo.png" align="right" height="140" /></a>

> kai.ā.ulu
> n.
> 1. Community, neighborhood, village. ʻOia nō kekahi o nā kānaka waiwai nui a kūʻonoʻono ma iā mau kaiāulu, he was one of the wealthiest and most prosperous persons of these communities.

## Overview

A common API to analyze various data sources common to software development (gitlog, mailing list, files, etc.), facilitate data interoperability through author and file linkage, filters, and popular code metrics. 

## Installation 

Dependencies in this package are modular. Depending on what you seek, you may only need to setup a sub-set of this section. Each module is divided by subsection below. Minimally you need to:

 1. Clone this repo 
 2. Open `kaiaulu.Rproj` using RStudio
 
### Gitlog analysis 

 1. Create a py virtualenv (optional)
 2. `pip3 install perceval`
 3. `which perceval` and note its path in `tools.yml`.
 4. Open vignettes/gitlog_showcase.Rmd or vignettes/churn_metrics.Rmd for examples and viz on gitlog analysis. 
 
### Mailing List Analysis

 1. Create a py virtualenv (optional)
 2. `pip3 install perceval`
 3. `which perceval` and note its path in `tools.yml`.
 4. Open vignettes/mailinglist_showcase.Rmd for examples and viz on mailing list analysis. 
 
### Dependency Analysis 

 1. Download [Depends release](https://github.com/multilang-depends/depends/releases/) (last tested on 0.96a)
 2. Open the Depends folder, and note the `depends.jar` path in `tools.yml`. 
 3. See vignettes/depends_showcase.Rmd for examples and viz on file dependency analysis.  
 
### Lines of Code, Comments, etc

 1. Download [scc](https://github.com/boyter/scc/releases) (last tested on 2.12.0)
 2. Unzip to obtain a `scc` executable. Note its path in `tools.yml`
 3. See vignettes/line_metrics_showcase.Rmd for examples.
 
### Line types 

 1. Download [Universal ctags](https://github.com/universal-ctags/ctags/blob/master/docs/osx.rst) using brew. 
 2. Emacs ctags may conflict with brew ctags. If so, using `ctags` may call emacs ctags instead of universal ctags from brew. Locate the binary of brew ctags and note its path on `tools.yml`
 3. See vignettes/line_type_showcase.Rmd for usage.
 
### Refactoring Code Analysis (Java only)
 1. Download or clone most recent version of [Refactoring Miner](https://github.com/tsantalis/RefactoringMiner#running-refactoringminer-from-the-command-line)
 2. `cd` to Refactoring Miner folder and type `./gradlew distZip`
 3. Extract the file under `build/distribution/RefactoringMiner.zip` in the desired location
 4. In the extracted folder, locate `/bin/RefactoringMiner`, and note its path on Kaiaulu `tools.yml`.
 5. See vignettes/refactoringminer_showcase.Rmd for details.
 
 ### OSLOM Community Detection 
 1. Download [OSLOM code (last tested on beta version 2.4)](http://oslom.org/)
 2. Use `./compile_all.sh`, as the manual suggests, to obtain `./oslom_undir` and `/oslom_dir`, and note its path on Kaiaulu `tools.yml`. See vignettes/community_detection_showcase.Rmd for details.

## Usage 

 * `parse_*()` provide a simple interface to load data from common data sources of interest for SE research:

```r
# get each file changes per commit
perceval_path <- "bin/perceval"
git_repo_path <- "APR/.git"
project_git <- parse_gitlog(perceval_path,git_repo_path)

# get each message sent per email thread
mbox_path <- "apr-dev.mbox"
project_mbox <- parse_mbox(perceval_path,mbox_path)

# identify authors with different name and emails
name_emails <- c(unique(project_git$data.Author),unique(project_mbox$data.From))
name_mapping <- data.table(raw_name=name_emails,
                           id=assign_exact_identity(name_emails))
```

 * `parse_*_network()` can then create edgelists from the parsed logs for libraries such as `igraph` and `Gephi`:

```r
perceval_path <- "bin/perceval"
git_repo_path <- "APR/.git"
project_git <- parse_gitlog(perceval_path,git_repo_path)

network <- parse_gitlog_network(project_git,mode="author")

# creates igraph object
network_igraph <- igraph::graph_from_data_frame(
  d=project_contribution_network[["edgelist"]],
  directed = TRUE, 
  vertices = project_contribution_network[["nodes"]])

# plot a interactive viz of the network                      
visIgraph(project_contribution_network,randomSeed = 1)
```

 * `parse_dependencies()` and `parse_dependencies_network()` can also be used to generate a network of static dependency between files. 

```r
# get each file function call to another file by type
depends_jar_path <- "depends-0.9.6/depends.jar"
git_repo_path <- "APR/.git"
language <- "cpp" # accepts cpp, java, ruby, python, pom

dependencies <- parse_dependencies(depends_jar_path,git_repo_path,language=language)   
network <- parse_dependencies_network(dependencies)

network_igraph <- igraph::graph_from_data_frame(
  d=network[["edgelist"]], 
  directed = TRUE, 
  vertices = network[["nodes"]])
visIgraph(network_igraph,randomSeed = 1)
```

 * `metric_churn()` and `metric_commit_interval_churn() ` can calculate churn for files and churn for commit intervals:

```r
# calculate code churn for a commit interval

perceval_path <- "bin/perceval"
git_repo_path <- "APR/.git"
project_git <- parse_gitlog(perceval_path,git_repo_path)

interval_churn <- metric_commit_interval_churn(project_git,"9eae9e9","f1d2d56")
```
