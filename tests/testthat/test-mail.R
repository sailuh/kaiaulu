tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

test_that("Incorrect perceval path fails parse_mbox", {

  conf <- parse_config(conf_path)
  key_1_name <- names(get_mbox_key_indexes(conf))[1]
  mbox_path <- get_mbox_path(conf,key_1_name)

  incorrect_perceval_path <- "/incorrect/path/to/perceval"
  expect_error(parse_mbox(incorrect_perceval_path, mbox_path), "Perceval execution failed.")
})
test_that("Incorrect mbox path to parse_mbox returns empty table", {

  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)
  perceval_path <- path.expand(perceval_path)
  incorrect_mbox_path <- "/incorrect/path/to/mbox"
  expect_error(parse_mbox(perceval_path, incorrect_mbox_path), "No valid JSON lines found in Perceval output. Check the mbox file or Perceval configuration.")
})

test_that("Calling parse_mbox with correct perceval and mbox path returns a data table with correct raw data", {
  tools_path <- file.path(tools_path)


  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)

  mbox_path <- example_mailing_list_two_threads(
    folder_path = "/tmp",
    folder_name = "example_two_threads_mailing_list",
    file_name = "two_thread_mailing_list"
  )

  result <- parse_mbox(perceval_path, mbox_path)

  io_delete_folder(folder_path = "/tmp", folder_name = "example_two_threads_mailing_list")

  expect_equal(result[reply_from == "John Doe <johndoe@example.com>"]$reply_subject, "Subject 1")
  expect_equal(result[reply_subject == "Re: Subject 1"]$reply_from, "Smithsonian Doe <smith_doe@example.com>")
})

