# Kaiāulu

> kai.ā.ulu
> n.
> 1. Community, neighborhood, village. ʻOia nō kekahi o nā kānaka waiwai nui a kūʻonoʻono ma iā mau kaiāulu, he was one of the wealthiest and most prosperous persons of these communities.

## 1. Setup 

 1. Clone this repo 
 2. Create a py virtualenv (optional)
 3. `pip3 install perceval`
 4. `which perceval` (take note of the full path of the binary for the example below)   
 5. Open `kaiaulu.Rproj` using RStudio.  

## 2.1 Example - Collaboration Network Parser 

 1. Git clone the repo of interest for analysis, e.g. [APR](https://github.com/apache/apr) 
 2. With .Rproj loaded, open the vignette `collaboration_network_parser.Rmd`.  
 3. Specify the path of `perceval` on `perceval_path` 
 4. Specify the downloaded `git_repo_path`, e.g. `/Users/cvp/Desktop/apr/.git` (the inclusion of the suffix .git is required). 
 5. Run the vignette and make some coffee while it processes the entire log into a .csv (in the future, this will be an awesome network).

## 2.2 Example - Communication Network Parser

TO-DO. 