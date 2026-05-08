tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

library(XML)
library(stringi)
library(data.table)

############## Third Party Tools ##############

test_that("If Depends is specified on tools.yml, then --help returns the help message", {
  # This test will skip if the Depends path is set to the default in tools.yml.
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  depends_path <- tool[["depends"]]

  default_path <- "/Library/Frameworks/Python.framework/Versions/3.12/bin/perceval"
  skip_if(grepl(depends_path, default_path), "Depends path is not set up in tools.yml.")

  out <- system2(
    "java",
    args = c("-jar", depends_path, "--help"),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_true(expect_true(any(grepl("Usage: depends", out))))
})

test_that("If scitools is specified on tools.yml, then und help returns the help message", {
  # This test will skip if the Depends path is set to the default in tools.yml.
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  scitools_path <- tool[["scitools"]]

  default_path <- "/Applications/Understand.app/Contents/MacOS/und"
  skip_if(grepl(scitools_path, default_path), "Scitools path is not set up in tools.yml.")

  out <- system2(
    scitools_path,
    args = "help",
    stdout = TRUE,
    stderr = TRUE
  )

  expect_true(expect_true(any(grepl("Understand", out))))
})

test_that("If RefactoringMiner is specified on tools.yml, then it returns the help message", {
  # This test will skip if the Depends path is set to the default in tools.yml.
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  refactoring_miner_path <- tool[["refactoring_miner"]]

  default_path <- "~/RefactoringMiner-1.0/bin/RefactoringMiner"
  skip_if(grepl(refactoring_miner_path, default_path), "RefactoringMiner path is not set up in tools.yml.")

  # Set working directory to RefactoringMiner Binary
  old_wd <- getwd()
  setwd(refactoring_miner_path)

  out <- system2(
    "./RefactoringMiner",
    args = "-h",
    stdout = TRUE,
    stderr = TRUE
  )

  setwd(old_wd)
  # Return working directory to Kaiaulu

  expect_true(expect_true(any(grepl("Show options", out))))
})

test_that("If scc is specified on tools.yml, then it returns the help message", {
  # This test will skip if the Depends path is set to the default in tools.yml.
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  scc_path <- tool[["scc"]]

  default_path <- "~/scc/scc"
  skip_if(grepl(scc_path, default_path), "scc path is not set up in tools.yml.")

  out <- system2(
    "scc",
    args = "--help",
    stdout = TRUE,
    stderr = TRUE
  )
  expect_true(expect_true(any(grepl("Usage:", out))))
})

############## Parsers ##############

test_that("filters can be used to delete unit tests and example files without deleting source code", {
  # Create a temporary directory for the Git repository
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_test_example_src_repo(folder_path = "/tmp",
                                                 folder_name = "test_example_and_src_repo")
  result <- parse_gitlog(perceval_path, git_repo_path)
  filtered_result <- result %>% filter_by_filepath_substring(c("example",'test'),"file_pathname")
  # expect only 1 because prefix test files and suffix example files are ignored.
  expect_equal(nrow(filtered_result), 1)
  io_delete_folder(folder_path = "/tmp",folder_name = "test_example_and_src_repo")

})

test_that("When parse_dependencies is given a depends_jar_path, a ./git folder, the language used in it, and a path to an output directory, then it returns a nodes and edges data table", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  depends_path <- tool[["depends"]]
  git_repo_path <- example_src_java_code_dependencies(folder_path = "/tmp",
                                                  folder_name = "test_example_java_code")

  result <- parse_dependencies(depends_path, git_repo_path, "java", "/tmp/")

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")

  io_delete_folder(folder_path="/tmp", "test_example_java_code")
})

test_that("When parse_dependencies is given a source code folder with a java file and a folder with another java file and the files share a dependency, then the dependency will be extracted to the edgelist data table", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  depends_path <- tool[["depends"]]
  git_repo_path <- example_src_java_nested_code_dependencies(folder_path = "/tmp",
                                                         folder_name = "test_example_java_code_dependencies")

  result <- parse_dependencies(depends_path, git_repo_path, "java", "/tmp/")

  expect_equal(result$edgelist$src_filepath, c("nested_folder/Main.java", "nested_folder/Main.java"))
  expect_equal(result$edgelist$dest_filepath, c("Helper.java", "Helper.java"))

  io_delete_folder(folder_path="/tmp", "test_example_java_code_dependencies")
})

test_that("When parse_dependencies is given a source code folder with a two java files and a python file, then the java dependencies will be extracted to a data table", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  depends_path <- tool[["depends"]]
  git_repo_path <- example_src_java_and_python_files(folder_path = "/tmp",
                                                         folder_name = "test_example_java_python_code")

  result <- parse_dependencies(depends_path, git_repo_path, "java", "/tmp/")

  expect_equal(result$edgelist$src_filepath, c("Main.java", "Main.java"))
  expect_equal(result$edgelist$dest_filepath, c("Helper.java", "Helper.java"))

  io_delete_folder(folder_path="/tmp", "test_example_java_python_code")
})

test_that("When parse_dependencies is given two java files that call each other, then the java dependencies will be extracted to a data table", {

  skip("Two java files calling each other causes an error.")
  # Error in ``[<-.data.table`(`*tmp*`, is.na(dependencies_types), value = 0)`: i is type 'list'. Must be integer, or numeric is coerced with warning. If i is a logical subset, simply wrap with which(), and take the which() outside the loop if possible for efficiency.

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  depends_path <- tool[["depends"]]
  git_repo_path <- example_src_java_circular_dependencies(folder_path = "/tmp",
                                                   folder_name = "test_example_circular_java_code")

  result <- parse_dependencies(depends_path, git_repo_path, "java", "/tmp/")

  str(result)
  expect_equal(result$edgelist$src_filepath, c("Main.java", "Main.java"))
  expect_equal(result$edgelist$dest_filepath, c("Helper.java", "Helper.java"))

  io_delete_folder(folder_path="/tmp", "test_example_circular_java_code")
})

test_that("When parse_understand_dependencies is given a Scitools formatted .xml file, then it returns a nodes and edges data table", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  scitools_path <- tool[["scitools"]]

  project_path <- example_src_xml_from_java_code(folder_path = "/tmp",
                                    folder_name = "test_example_java_to_xml_code")

  result <- parse_understand_dependencies("/tmp/test_example_java_to_xml_code/example.xml")

  expect_is(result$node_list, "data.table")
  expect_is(result$edge_list, "data.table")

  io_delete_folder(folder_path="/tmp", "test_example_java_to_xml_code")
})

test_that("When parse_r_dependencies is given a folder path containing R function code, then it returns a data table", {

  result <- example_r_code_dependencies(folder_path = "/tmp",
                                           folder_name = "test_example_r_code")

  result <- parse_r_dependencies("/tmp/test_example_r_code")

  expect_is(result, "data.table")

  io_delete_folder(folder_path="/tmp", "test_example_r_code")
})

test_that("When parse_dependencies is given a source code folder with an R file and a folder with another R file and the files share a dependency, then the dependency will be extracted to a data table", {
  result <- example_src_r_nested_code_dependencies(folder_path = "/tmp",
                                        folder_name = "test_example_r_code")

  result <- parse_r_dependencies("/tmp/test_example_r_code")

  expect_equal(result$src_functions_call_name, "helper_process")
  expect_equal(result$src_functions_caller_name, "utils_do_work")

  io_delete_folder(folder_path="/tmp", "test_example_r_code")
})

test_that("When parse_dependencies is given a source code folder with a two R files and a python file, then the java dependencies will be extracted to a data table", {
  result <- example_src_r_and_python_files(folder_path = "/tmp",
                                                   folder_name = "test_example_r_code")

  result <- parse_r_dependencies("/tmp/test_example_r_code")

  expect_equal(result$src_functions_call_name, "helper_process")
  expect_equal(result$src_functions_caller_name, "utils_do_work")

  io_delete_folder(folder_path="/tmp", "test_example_r_code")
})

test_that("When parse_dependencies is given two R files that import each other, then the R dependencies will be extracted to a data table", {
  result <- example_src_r_circular_dependencies(folder_path = "/tmp",
                                                folder_name = "test_example_r_code")

  result <- parse_r_dependencies("/tmp/test_example_r_code")

  expect_equal(result$src_functions_call_name, c("utils_do_work", "helper_process"))
  expect_equal(result$src_functions_caller_name, c("helper_process", "utils_do_work"))

  io_delete_folder(folder_path="/tmp", "test_example_r_code")
})

test_that("When parse_line_metrics is given a scc_path and a ./git folder, then it returns a data table", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  scc_path <- tool[["scc"]]
  git_repo_path <- example_src_java_code_dependencies(folder_path = "/tmp",
                                     folder_name = "test_example_java_code")

  result <- parse_line_metrics(scc_path, git_repo_path)
  expect_is(result, "data.table")

  io_delete_folder(folder_path="/tmp", "test_example_java_code")
})

############## Transforms ##############

test_that("When transform_dependencies_to_network is given nodes and edgelist data tables from parse_dependencies, then it returns a new nodes and edgelist data table", {
  dt <- list(
    nodes = data.table(
      filepath = c("Helper.java", "Main.java", "Utils.java")
    ),
    edgelist = data.table(
      src_filepath  = c("Utils.java", "Main.java", "Main.java"),
      dest_filepath = c("Helper.java", "Utils.java", "Helper.java"),
      Call = c(2, 1, 1),
      Use  = c(2, 1, 1)
    )
  )

  result <- transform_dependencies_to_network(dt)

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")
})

test_that("When transform_understand_dependencies_to_network is given a data table from parse_understand_dependencies, then it returns a nodes and edgelist data table", {
  dt <- list(
    node_list = data.table(
      node_label = c("Helper.java", "Main.java", "Utils.java"),
      id         = c("1", "15", "21"),
      long_name  = c(
        "/tmp/test_example_java_code/Helper.java",
        "/tmp/test_example_java_code/Main.java",
        "/tmp/test_example_java_code/Utils.java"
      )
    ),
    edge_list = data.table(
      label_from      = c("Main.java", "Utils.java", "Main.java"),
      label_to        = c("Helper.java", "Helper.java", "Utils.java"),
      id_from         = c("15", "21", "15"),
      id_to           = c("1", "1", "21"),
      dependency_kind = c("Call", "Call", "Call")
    )
  )

  result <- transform_understand_dependencies_to_network(dt, weight_types = "Call")

  expect_is(result$node_list, "data.table")
  expect_is(result$edge_list, "data.table")
})

test_that("When transform_r_dependencies_to_network is given a data table from parse_r_dependencies, then it returns a nodes and edgelist data table", {

  dt <- data.table(
    src_functions_call_name = c(
      "helper_process",
      "utils_do_work",
      "main",
      "helper_process",
      "helper_log"
    ),
    src_functions_call_filename = c(
      "Helper.R",
      "Utils.R",
      "Main.R",
      "Helper.R",
      "Utils.R"
    ),
    src_functions_caller_name = c(
      "main",
      "main",
      "main",
      "utils_do_work",
      "utils_do_work"
    ),
    src_functions_caller_filename = c(
      "Main.R",
      "Main.R",
      "Main.R",
      "Utils.R",
      "Utils.R"
    ),
    src_line_functions_call_start = c(5, 6, 9, 4, 5),
    src_line_functions_call_end   = c(5, 6, 9, 4, 5)
  )
  result <- transform_r_dependencies_to_network(dt)

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")
})

test_that("When transform_r_dependencies_to_network is given a valid parsed r folder and dependency_type, then the type of remaining dependencies consist only of those from the type specified", {

  dt <- data.table(
    src_functions_call_name = c(
      "helper_process",
      "utils_do_work",
      "main",
      "helper_process",
      "helper_log"
    ),
    src_functions_call_filename = c(
      "Helper.R",
      "Utils.R",
      "Main.R",
      "Helper.R",
      "Utils.R"
    ),
    src_functions_caller_name = c(
      "main",
      "main",
      "main",
      "utils_do_work",
      "utils_do_work"
    ),
    src_functions_caller_filename = c(
      "Main.R",
      "Main.R",
      "Main.R",
      "Utils.R",
      "Utils.R"
    ),
    src_line_functions_call_start = c(5, 6, 9, 4, 5),
    src_line_functions_call_end   = c(5, 6, 9, 4, 5)
  )
  functions <- transform_r_dependencies_to_network(dt, dependency_type = "function")
  files <- transform_r_dependencies_to_network(dt, dependency_type = "file")

  expect_equal(functions$nodes$name, c("helper_process", "utils_do_work", "main", "helper_log"))
  expect_equal(files$nodes$name, c("Helper.R", "Utils.R", "Main.R"))
})
