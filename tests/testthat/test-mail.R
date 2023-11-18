tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

test_that("Calling parse_mbox with correct perceval and mbox path returns a data table with correct raw data", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  mbox_path <- example_mbox_normal <- function(folder_path = "/tmp", folder_name="sample_folder")  # Replace with the actual path to your mbox file
  result <- parse_mbox(perceval_path, mbox_path)
  #expect_is(result, "data.table")
  #expect_equal(file.exists(mbox_path), TRUE)

})

