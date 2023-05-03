test_that("download_bugzilla_perceval_traditional_issue_comments() returns correct error with incorrect perceval path", {
  incorrect_perceval_path <- "incorrect/path/to/perceval"
  datetime <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  bugzilla_site <- "https://bugzilla.samba.org/"
  expect_error(download_bugzilla_perceval_traditional_issue_comments(incorrect_perceval_path, bugzilla_site, datetime), "error in running command")
})
test_that("download_bugzilla_perceval_traditional_issue_comments() returns correct error with incorrect bugzilla site", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  datetime <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  incorrect_bugzilla_site <- "www.fakebugzilla123123.com"
  suppressWarnings({
    output_json <- download_bugzilla_perceval_traditional_issue_comments(perceval_path, incorrect_bugzilla_site, datetime, max_bugs=1)
  })
  expect_equal(length(output_json), 0)
})
test_that("download_bugzilla_perceval_traditional_issue_comments() returns nonempty with correct parameters", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  datetime <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  bugzilla_site <- "https://bugzilla.samba.org/"
  suppressWarnings({
    output_json <- download_bugzilla_perceval_traditional_issue_comments(perceval_path, bugzilla_site, datetime, max_bugs=1)
  })
  expect_equal(length(output_json) != 0, TRUE)
})
test_that("download_bugzilla_perceval_rest_issue_comments() returns correct error with incorrect perceval path", {
  incorrect_perceval_path <- "incorrect/path/to/perceval"
  datetime <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  bugzilla_site <- "https://bugzilla.redhat.com/"
  expect_error(download_bugzilla_perceval_rest_issue_comments(incorrect_perceval_path, bugzilla_site, datetime), "error in running command")
})
test_that("download_bugzilla_perceval_rest_issue_comments() returns correct error with incorrect bugzilla site", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  datetime <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  incorrect_bugzilla_site <- "www.fakebugzilla123123.com"
  suppressWarnings({
    output_json <- download_bugzilla_perceval_rest_issue_comments(perceval_path, incorrect_bugzilla_site, datetime, max_bugs=1)
  })
  expect_equal(length(output_json), 0)
})
test_that("download_bugzilla_perceval_rest_issue_comments() returns nonempty with correct parameters", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  datetime <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  bugzilla_site <- "https://bugzilla.redhat.com/"
  suppressWarnings({
    output_json <- download_bugzilla_perceval_rest_issue_comments(perceval_path, bugzilla_site, datetime, max_bugs=1)
  })
  expect_equal(length(output_json) != 0, TRUE)
})

