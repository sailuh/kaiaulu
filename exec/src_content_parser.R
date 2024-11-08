#!/usr/local/bin/Rscript

require(yaml, quietly = TRUE)
require(cli, quietly = TRUE)
require(docopt, quietly = TRUE)
require(kaiaulu, quietly = TRUE)
require(data.table, quietly = TRUE)
require(stringi, quietly = TRUE)
require(XML, quietly = TRUE)

doc <- "
USAGE:
  query.R query <tools.yml> <project_conf.yml> <srcml_filepath> [--namespace | --file-docs | --class-docs | --variables | --packages | --functions | --imports | --class-names]

DESCRIPTION:
  Runs a specified query on the annotated XML file and saves the output to the appropriate path.

OPTIONS:
  --namespace      Run the namespace query.
  --file-docs      Run the file-level documentation query.
  --class-docs     Run the class-level documentation query.
  --variables      Run the variables query.
  --packages       Run the packages query.
  --functions      Run the functions query.
  --imports        Run the imports query.
  --class-names    Run the class names query.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')

tools_path <- arguments[["<tools.yml>"]]
conf_path <- arguments[["<project_conf.yml>"]]
srcml_filepath <- arguments[["<srcml_filepath>"]]

tool <- yaml::read_yaml(tools_path)
conf <- yaml::read_yaml(conf_path)
srcml_path <- path.expand(tool[["srcml"]])

# Set output folder path
output_folder <- "../analysis/maven/queries"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Determine which query to run and save output
if (arguments[["--namespace"]]) {
  output_file <- file.path(output_folder, "namespace_output.csv")
  query_result <- query_src_text_namespace(srcml_path, srcml_filepath)
} else if (arguments[["--class-names"]]) {
  output_file <- file.path(output_folder, "class_names_output.csv")
  query_result <- query_src_text_class_names(srcml_path, srcml_filepath)
} else if (arguments[["--file-docs"]]) {
  output_file <- file.path(output_folder, "file_docs_output.csv")
  query_result <- query_src_text_file_docs(srcml_path, srcml_filepath)
} else if (arguments[["--class-docs"]]) {
  output_file <- file.path(output_folder, "class_docs_output.csv")
  query_result <- query_src_text_class_docs(srcml_path, srcml_filepath)
} else if (arguments[["--variables"]]) {
  output_file <- file.path(output_folder, "variables_output.csv")
  query_result <- query_src_text_variables(srcml_path, srcml_filepath)
} else if (arguments[["--packages"]]) {
  output_file <- file.path(output_folder, "packages_output.csv")
  query_result <- query_src_text_packages(srcml_path, srcml_filepath)
} else if (arguments[["--functions"]]) {
  output_file <- file.path(output_folder, "functions_output.csv")
  query_result <- query_src_text_functions(srcml_path, srcml_filepath)
} else if (arguments[["--imports"]]) {
  output_file <- file.path(output_folder, "imports_output.csv")
  query_result <- query_src_text_imports(srcml_path, srcml_filepath)
} else {
  stop("No valid query option provided.")
}

# Transform each row into a single line of text
plain_text_output <- apply(query_result, 1, function(row) paste(row, collapse = " "))

# Save to file
write.table(query_result, output_file, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
cli_alert_success(paste0("Query results saved for fastText at: ", output_file))

