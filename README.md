# Kaiāulu <a href="https://github.com/sailuh/kaiaulu"><img src="man/figures/logo.png" align="right" height="140" /></a>

> kai.ā.ulu
> n.
> 1. Community, neighborhood, village. ʻOia nō kekahi o nā kānaka waiwai nui a kūʻonoʻono ma iā mau kaiāulu, he was one of the wealthiest and most prosperous persons of these communities.

## Overview

An API to analyze various data sources common to software development (gitlog, mailing list, files, etc.) and facilitate data interoperability through author and file linkage, filters, and popular code metrics. 

Kaiaulu is an R package, and as such adheres closely to how R packages are structured: Instead of an all-in-all-out interface, various functions are provided so you can tailor it to your needs. The R notebooks (vignettes) provide reusable all-in-all-out runnable pipelines built with these functions. If you don't see one that fits your needs, please [ask on Discussions](https://github.com/sailuh/kaiaulu/discussions). 

Most functions will return standardized tables which makes it easier to inspect intermediate steps and combine to other tables. This is done to encourage assessment and understanding of the built-in pipelines with minimal effort.

If you are not familiar with R, you can find command line interface scripts in the `exec/` folder. Please create an issue if you would like a feature you do not see there.

## Features 

 * [Filepath Filters](http://itm0.shidler.hawaii.edu/kaiaulu/reference/index.html#section-filters)
 * Standardized table interface for [built-in and wrapper to parsers](http://itm0.shidler.hawaii.edu/kaiaulu/reference/index.html#section-parsers): 
    * GitLog (File, function, class and language-specific entities) + Bug Count + Churn
    * Mailing List .mbox + Apache, Mailman and Google Groups Mailing List Crawlers
    * JIRA Issues and Issue Comments 
    * Static Dependencies (File and function)
    * Line Metrics (e.g. LOC, Comment Lines)
    * Refactorings (Java only)
    * Issue ID parsers from commit messages 
    * Name and E-mail Identity Matching (For identifying same users inter and intra GitLog, Mailing List, etc.)
 * [Parser to Network Transformations](http://itm0.shidler.hawaii.edu/kaiaulu/reference/index.html#section-networks) + Interactive visualizations (See Getting Started below)
    * Graph representation uses node + edge format, making easier to extend analysis in popular graph tools such as Gephi and igraph library. 
 * Social Smells (Organizational Silos, Missing Links, Radio Silence)
 * Quality Framework Metrics (N. Timezones, Authors on GitLog, Authors on Mailing List, N. Files changed, etc).
 * And more! 

## Installation 

Dependencies in this package are modular. Depending on what you seek, you may only need to setup a sub-set of this section. Each module is divided by subsection below. Minimally you need to:

 1. Clone this repo 
 2. Open `kaiaulu.Rproj` using RStudio

The following is optional depending on the functionality you seek:

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

## Getting started

 * Parsing and Visualizing
    * [Git Logs](http://itm0.shidler.hawaii.edu/kaiaulu/articles/gitlog_showcase.html)
    * [Mailing Lists and JIRA Issues](http://itm0.shidler.hawaii.edu/kaiaulu/articles/reply_communication_showcase.html)
    * [File Dependencies](http://itm0.shidler.hawaii.edu/kaiaulu/articles/depends_showcase.html)
    * [Software Vulnerabilities](http://itm0.shidler.hawaii.edu/kaiaulu/articles/gitlog_vulnerabilities_showcase.html)
    * [GitHub API Interface](https://github.com/sailuh/kaiaulu/blob/86-add-github-api/vignettes/github_api_showcase.Rmd) (See 86-add-github-api branch)
    * [Network Community Detection](http://itm0.shidler.hawaii.edu/kaiaulu/articles/community_detection_showcase.html)
 * [Kaiaulu File and Function Architecture](http://itm0.shidler.hawaii.edu/kaiaulu/articles/kaiaulu_architecture.html)
 * Metrics
    * [Bug Count](http://itm0.shidler.hawaii.edu/kaiaulu/articles/bug_count.html)
    * [Social Smells](http://itm0.shidler.hawaii.edu/kaiaulu/articles/social_smells_showcase.html)
    * [Line Metrics](http://itm0.shidler.hawaii.edu/kaiaulu/articles/line_metrics_showcase.html)    
 * Downloaders
    * [Apache's Mod Mbox](http://itm0.shidler.hawaii.edu/kaiaulu/articles/reply_communication_showcase.html)


## Stay up-to-date

 * Read the [NEWS file](https://github.com/sailuh/kaiaulu/blob/master/NEWS.md).

## Contributing

 * See the [CONTRIBUTING file](https://github.com/sailuh/kaiaulu/blob/master/CONTRIBUTING.md) for contributing questions, issues and pull requests.

## How to cite Kaiaulu 

If you are using Kaiaulu in your research, please cite the following work: 

```
@phdthesis{Paradis:2021,
  author  = "Carlos Paradis",
  title   = "PERCEIVE: Proactive Exploration of Risky Concept Emergence for Identifying Vulnerabilities \& Exposures",
  school  = "University of Hawaii at Manoa",
  year    = "2021",
  month = "May"
}
```
