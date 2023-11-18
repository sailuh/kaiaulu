tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

test_that("Calling parse_mbox with correct perceval and mbox path returns a data table with correct raw data", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  mbox_path <- example_mbox_normal <- function(folder_path = "/tmp", folder_name="sample_folder")  # Replace with the actual path to your mbox file
  #expect_is(result, "data.table")
  #expect_equal(file.exists(mbox_path), TRUE)


  expected_mlist <- "test-list"
  expected_sender <- "johndoe@example.com"
  expected_recipient <- "janedoe@example.com"
  expected_cc <- "smith_doe@example.com"
  expected_datetime <- "2023-01-15T08:30:00"
  expected_timezone <- "EST"
  expected_reply_subject <- "Test Email Subject"
  expected_reply_body <- "This is the body of the test email."



  expect_equal(result[["From"]], expected_sender)
  expect_equal(result[["To"]], expected_recipient)
  expect_equal(result[["Date"]], expected_datetime)
  expect_equal(result[["Subject"]], expected_subject)
  expect_equal(result[["body"]], expected_body)

})

