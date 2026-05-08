tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

test_that("When parse_mbox is given a correct perceval_path and path to .mbox file, it returns a data table with correct raw data", {
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

test_that("When parse_mbox is given an invalid perceval_path, it raises an error", {

  conf <- parse_config(conf_path)
  key_1_name <- names(get_mbox_key_indexes(conf))[1]
  mbox_path <- get_mbox_path(conf,key_1_name)

  incorrect_perceval_path <- "/incorrect/path/to/perceval"
  expect_error(parse_mbox(incorrect_perceval_path, mbox_path), "Perceval execution failed.")
})

test_that("When parse_mbox is given a path to a file that is not .mbox, then it raises an error", {

  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)
  perceval_path <- path.expand(perceval_path)
  incorrect_mbox_path <- "/incorrect/path/to/mbox"
  expect_error(parse_mbox(perceval_path, incorrect_mbox_path), "No valid JSON lines found in Perceval output. Check the mbox file or Perceval configuration.")
})

# I don't think there is a way to use the current fake data generator to create an .mbox file with a missing key. This could be valuable in the future
# if the fake data generator can be updated to create a missing key field.
# test_that("When parse_mbox is given an .mbox file that is missing the necessary key reply_body, then it raises an error", {
#   tool <- parse_config(tools_path)
#   perceval_path <- get_tool_project("perceval",tool)
#   perceval_path <- path.expand(perceval_path)
#
#   mbox_path <- example_mbox_missing_reply_body_key(
#     folder_path = "/tmp",
#     folder_name = "example_mbox_missing_reply_body_key",
#     file_name = "no_reply_key"
#   )
#
#   expect_error(result <- parse_mbox(perceval_path, mbox_path))
#
#   io_delete_folder(folder_path = "/tmp", folder_name = "example_mbox_missing_reply_body_key")
# })

test_that("When parse_mbox is given a reply_body that contains an escape backslash, then it returns an unescaped result in the data table", {
  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)
  perceval_path <- path.expand(perceval_path)

  mbox_path <- example_mbox_backslash(
    folder_path = "/tmp",
    folder_name = "example_mbox_backslash",
    file_name = "backslash"
  )

  result <- parse_mbox(perceval_path, mbox_path)

  expect_equal(result$reply_body, "This \n should not add a newline\n")

  io_delete_folder(folder_path = "/tmp", folder_name = "example_mbox_backslash")
})

test_that("When parse_mbox is given an empty subject and empty body, then it is able to parse the empty results in the data table", {
  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)
  perceval_path <- path.expand(perceval_path)

  mbox_path <- example_mbox_missing_subject_body(
    folder_path = "/tmp",
    folder_name = "example_mbox_missing_subject_body",
    file_name = "missing_subject"
  )

  result <- parse_mbox(perceval_path, mbox_path)

  expect_equal(result$reply_subject, NA)
  expect_equal(result$reply_body, "")

  io_delete_folder(folder_path = "/tmp", folder_name = "example_mbox_missing_subject_body")
})

test_that("When parse_mbox is given a malformed date, then it is able to parse the result in the data table", {

  skip("Malformed dates cannot be handled by parse_mbox.")
  # This test fails with "Error in `parse_mbox(perceval_path, mbox_path)`: No valid JSON lines found in Perceval output. Check the mbox file or Perceval configuration."
  # To me it seems like the parser breaks with malformed dates.

  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)
  perceval_path <- path.expand(perceval_path)

  mbox_path <- example_mbox_malformed_date(
    folder_path = "/tmp",
    folder_name = "example_mbox_malformed_date",
    file_name = "malformed_date"
  )

  result <- parse_mbox(perceval_path, mbox_path)

  expect_equal(result$reply_datetime, "20XX-150-40:30:00")

  io_delete_folder(folder_path = "/tmp", folder_name = "example_mbox_malformed_date")
})
test_that("When parse_mbox is given two recipients to receive an email in the reply_to_author and reply_to_email fields, then it is able to parse the result in the data table", {

  skip("There is no reply_to field in the resulting data table.")
  # All other rows are duplicated, but reply_to doesn't exist.

  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)
  perceval_path <- path.expand(perceval_path)

  mbox_path <- example_mbox_two_recipients(
    folder_path = "/tmp",
    folder_name = "example_mbox_two_recipients",
    file_name = "two_recipients"
  )

  result <- parse_mbox(perceval_path, mbox_path)
  str(result)

  expect_equal(result$reply_to, c("Dev 1", "Dev 2"))

  io_delete_folder(folder_path = "/tmp", folder_name = "example_mbox_two_recipients")
})

test_that("When parse_mbox is given a reply_body that contains unicode characters, then it returns the respective result in the data table", {

  skip("Unicode characters cannot be handled by parse_mbox.")
  # This is a section of what the reply_body looks like. "\xed\xb3\xa4\xed\xb2\xbb\xed\xb2\x8a\xed\xb3\xa6\xed\xb2\x97\"
  # It could be an issue with how Perceval handles unicode characters, or with the parser itself.

  tool <- parse_config(tools_path)
  perceval_path <- get_tool_project("perceval",tool)
  perceval_path <- path.expand(perceval_path)

  mbox_path <- example_mbox_unicode_characters(
    folder_path = "/tmp",
    folder_name = "example_mbox_unicode_characters",
    file_name = "unicode_characters"
  )

  result <- parse_mbox(perceval_path, mbox_path)
  str(result)
  expect_equal(result$reply_subject, NA)
  expect_equal(result$reply_body, "")

  io_delete_folder(folder_path = "/tmp", folder_name = "example_mbox_unicode_characters")
})


