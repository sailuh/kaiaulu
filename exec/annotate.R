#!/usr/local/bin/Rscript

# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

require(yaml,quietly=TRUE)
require(cli,quietly=TRUE)
require(docopt,quietly=TRUE)
require(kaiaulu,quietly=TRUE)
require(data.table,quietly=TRUE)
require(stringi,quietly=TRUE)
require(XML,quietly=TRUE)
require(gt,quietly=TRUE)


doc <- "
USAGE:
  annotate.R annotate help
  annotate.R annotate <tools.yml> <project_conf.yml>
  annotate.R (-h | --help)
  annotate.R --version

DESCRIPTION:
  Provides a function to interact with Kaiaulu's Syntax Extractor,
  to generate an annotated XML file. Please see
  Kaiaulu's README.md for instructions on how to create <tool.yml>
  and <project_conf.yml>.


OPTIONS:
  -h --help     Show this screen.
  --version     Show version.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9700')
if(arguments[["annotate"]] & arguments[["help"]]){
  cli_alert_info("Annotates source code using srcML.")
}else if(arguments[["annotate"]]){

  tools_path <- arguments[["<tools.yml>"]]
  conf_path <- arguments[["<project_conf.yml>"]]

  tool_conf <- parse_config(tools_path)
  project_conf <- parse_config(conf_path)

  srcml_path <- get_tool_project("srcml", tool_conf)
  src_folder <- get_src_folder(project_conf)
  srcml_filepath <- get_srcml_filepath(project_conf)

  srcml_path <- path.expand(srcml_path)
  src_folder <- path.expand(src_folder)
  srcml_filepath <- path.expand(srcml_filepath)

  annotated_file <- annotate_src_text(
    srcml_path = srcml_path,
    src_folder = src_folder,
    srcml_filepath = srcml_filepath
  )

  cli_alert_success(paste0("Annotated xml was saved at: ",srcml_filepath))
}
