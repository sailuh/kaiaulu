tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

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
