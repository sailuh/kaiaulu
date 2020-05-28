# Kaiāulu

> kai.ā.ulu
> n.
> 1. Community, neighborhood, village. ʻOia nō kekahi o nā kānaka waiwai nui a kūʻonoʻono ma iā mau kaiāulu, he was one of the wealthiest and most prosperous persons of these communities.

## 1. Setup 

Dependencies in this package are modular. Depending on what you seek, you may only need to setup a sub-set of this section. Each module is divided by subsection below. Minimally you need to:

 1. Clone this repo 
 2. Open `kaiaulu.Rproj` using RStudio
 
## 1.1 Gitlog analysis 

 1. Create a py virtualenv (optional)
 2. `pip3 install perceval`
 3. `which perceval` (take note of the path for the vignette)
 4. Open vignettes/gitlog_showcase.Rmd or vignettes/churn_metrics.Rmd for examples and viz on gitlog analysis. 
 
## 1.2 Mailing List Analysis

 1. Create a py virtualenv (optional)
 2. `pip3 install perceval`
 3. `which perceval` (take note of the path for the vignette)
 4. Open vignettes/mailinglist_showcase.Rmd for examples and viz on mailing list analysis. 
 
## 1.3 Static Code Analysis 

 1. Create a py virtualenv (optional)
 2. Download [Depends release](https://github.com/multilang-depends/depends/releases/) (last tested on 0.96a)
 3. Open the depends folder, and take note of `depends.jar` path. 
 4. See vignettes/depends_showcase.Rmd for examples and viz on file dependency analysis.  
