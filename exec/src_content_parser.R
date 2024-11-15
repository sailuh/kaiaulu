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
  src_content_parser.R query <tools.yml> <project_conf.yml> <srcml_filepath> <output_dir> [--namespace | --file-docs | --class-docs | --variables | --packages | --functions | --imports | --class-names]

DESCRIPTION:
  Uses srcML to parse a source code folder, as specified on the project configuration file, to parse selected content and generate structured tables (e.g., classes, functions, variables) as CSV files. Each query option produces a table with a specific structure. See OPTIONS for details.

OPTIONS:
  --namespace      Returns a 2-column table (filepath, namespace) where each row represents a namespace in the source code.
  --file-docs      Returns a 2-column table (filepath, file_docs) where each row is defined by file-level documentation.
  --class-docs     Returns a 2-column table (filepath, class_docs) where each row is defined by a class docstring.
  --variables      Returns a 3-column table (filepath, variable_name, variable_type) where each row represents a variable in the source code.
  --packages       Returns a 2-column table (filepath, package) where each row represents an imported package.
  --functions      Returns a 3-column table (filepath, function_name, parameters) where each row represents a function in the source code.
  --imports        Returns a 2-column table (filepath, import) where each row represents an imported module or package.
  --class-names    Returns a 2-column table (filepath, classname) where each row is defined by a class name.
"

arguments <- docopt::docopt(doc, version = 'Kaiaulu 0.0.0.9600')

tools_path <- path.expand(arguments[["<tools.yml>"]])
conf_path <- path.expand(arguments[["<project_conf.yml>"]])
srcml_filepath <- path.expand(arguments[["<srcml_filepath>"]])
output_dir <- path.expand(arguments[["<output_dir>"]])

# Read configuration
tool <- yaml::read_yaml(tools_path)
conf <- yaml::read_yaml(conf_path)

# Get the directory of the conf file
conf_dir <- dirname(conf_path)

# Resolve src_folder relative to conf_dir
src_folder <- path.expand(file.path(conf_dir, conf[["srcml"]][["src_folder"]]))

srcml_path <- path.expand(tool[["srcml"]])

# Check if srcml_filepath exists; if not, generate it
if (!file.exists(srcml_filepath)) {
  cli_alert_info(paste0("srcml_filepath does not exist. Generating srcML XML at: ", srcml_filepath))
  annotate_src_text(srcml_path, src_folder, srcml_filepath)
}

# Set output folder path
output_folder <- output_dir
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

# Save to file
write.table(query_result, output_file, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
cli_alert_success(paste0("Query results saved at: ", output_file))
