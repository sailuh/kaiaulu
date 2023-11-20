tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

test_that("Calling parse_mbox with correct perceval and mbox path returns a data table with correct raw data", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  mbox_path <- example_mailing_list_two_threads(folder_path = "/tmp",
                                                folder_name="example_two_threads_mailing_list",
                                                file_name = "two_thread_mailing_list")
  result <- parse_mbox(perceval_path, mbox_path)

  io_delete_folder(folder_path="/tmp", folder_name="example_two_threads_mailing_list")


  expect_equal(result[reply_from == "John Doe <johndoe@example.com>"]$reply_subject, "Subject 1")
  expect_equal(result[reply_subject == "Re: Subject 1"]$reply_from, "Smithsonian Doe <smith_doe@example.com>")

})

