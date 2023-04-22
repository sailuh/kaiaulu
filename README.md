# Kaiāulu <a href="https://github.com/sailuh/kaiaulu"><img src="man/figures/logo.png" align="right" height="140" /></a>

> kai.ā.ulu
> n.
> 1. Community, neighborhood, village. ʻOia nō kekahi o nā kānaka waiwai nui a kūʻonoʻono ma iā mau kaiāulu, he was one of the wealthiest and most prosperous persons of these communities.

## Overview

Kaiaulu is an R package and common interface that helps with understanding evolving software development communities, and the artifacts (gitlog, mailing list, files, etc.) which developers collaborate and communicate about. 

Kaiaulu provides:

 * **Parsers** to tabulate and rename software artifacts (`R/parser.R`)
 * **Filters** to control the scope of analysis (`R/filter.R`)
 * **Downloaders** for both Issue Trackers, Pull Requests, and Mailing Lists (`R/download.R`, `R/github.R`)
 * **Networks** to represent software communities as graphs (`R/network.R`, `R/graph.R`)
 * **Identity Matching** to connect artifacts (`R/identity.R`)
 * **Metrics** commonly used in software research (`R/metrics.R`)
 * **Extended Interface** to third party tools for further analysis (`R/dv8.R`)
 * **Reusable Analysis Notebooks** which comprehensively present above features, and warn for threats and pitfalls known on research literature (`vignettes/`)
 * **CLI Interface** for server-side analysis on multiple projects (`exec/`)
 * **Project Configuration Files** that are readable, shareable, and enable reproducibility between both Notebooks and CLI (`conf/`) 
 * **A common and simple data model** using tables and sane nomenclature to various resources which can be combined performing table joins.

Please [ask questions on Discussions](https://github.com/sailuh/kaiaulu/discussions) or open an issue on the [issue tracker](https://github.com/sailuh/kaiaulu/issues) if you found a bug, or your answer can't be found in the documentation. A more comprehensive and ever growing list of features is available on the [Project Wiki](https://github.com/sailuh/kaiaulu/wiki). 


## Installation 

Kaiaulu has been tested on OS X and Ubuntu. For Windows and other OS users, try [Virtualbox](https://www.virtualbox.org/),
[VMware](https://www.vmware.com/), or any other software to run virtual machines for Ubuntu. 


 1. Clone this repo 
 2. Open `kaiaulu.Rproj` using RStudio
 3. Run the unit tests `devtools::test()`. If any fail, and you are not clear why, feel free to [ask in Discussions](https://github.com/sailuh/kaiaulu/discussions)
 4. Build the documentation `devtools::document(roclets = c('rd', 'collate', 'namespace'))`.
 5. Build Kaiaulu (Top right pane in RStudio -> Build tab -> Install and Restart)
 6. Run `vignettes/kaiaulu_architecture.Rmd` 
 7. See the Wiki's [Third Party Tools Setup](https://github.com/sailuh/kaiaulu/wiki/Third-Party-Tools-Setup) if you are using a Notebook that relies on them. These require very minimal overhead by downloading a binary file, and specifying their path on `tools.yml` (see example on the repository). 
 
## Getting started

To get started, browse through [the docs](http://itm0.shidler.hawaii.edu/kaiaulu).

## Stay up-to-date

 * Read the [NEWS file](https://github.com/sailuh/kaiaulu/blob/master/NEWS.md).

## Contributing

 * See the [CONTRIBUTING file](https://github.com/sailuh/kaiaulu/blob/master/CONTRIBUTING.md) for contributing questions, issues and pull requests.

## How to cite Kaiaulu 

If you are using Kaiaulu in your research, please cite the following work: 

```
@InProceedings{Paradis:2022,
author="Paradis, Carlos and Kazman, Rick",
editor="Scandurra, Patrizia and Galster, Matthias and Mirandola, Raffaela and Weyns, Danny",
title="Building the MSR Tool Kaiaulu: Design Principles and Experiences",
booktitle="Software Architecture",
year="2022",
publisher="Springer International Publishing",
address="Cham",
pages="107--129",
isbn="978-3-031-15116-3",
doi = "10.1007/978-3-031-15116-3_6"
}
```

For a list of previous work that used Kaiaulu, see `vignettes/kaiaulu_case_studies.Rmd`. If that's you, feel free to send a pull request modifying the Notebook with your work, and if possible, include a Bibtex record. 
