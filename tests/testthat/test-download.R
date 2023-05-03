tools_path <- test_path("testdata", "tools.yml")

bugzilla_issues_comments_folder_path <- file.path('/tmp', 'bugzilla_issues_comments')
samba_issues_comments_json_path <- file.path(bugzilla_issues_comments_folder_path, 'samba.json')
redhat_issues_comments_json_path <- file.path(bugzilla_issues_comments_folder_path, 'redhat.json')

suppressWarnings({
  error <- system2('mkdir',
                   args = c(bugzilla_issues_comments_folder_path),
                   stdout = TRUE,
                   stderr = FALSE)
})

tool <- yaml::read_yaml(tools_path)
perceval_path <- tool[["perceval"]]
perceval_path <- path.expand(perceval_path)
datetime <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%S%z")

samba_bugzilla_site <- "https://bugzilla.samba.org/"
redhat_bugzilla_site <- "https://bugzilla.redhat.com/"

test_that("download_bugzilla_perceval_traditional_issue_comments() returns correct error with incorrect perceval path", {
  incorrect_perceval_path <- "incorrect/path/to/perceval"
  expect_error(download_bugzilla_perceval_traditional_issue_comments(incorrect_perceval_path, samba_bugzilla_site, datetime), "error in running command")
})
test_that("download_bugzilla_perceval_traditional_issue_comments() returns correct error with incorrect bugzilla site", {
  incorrect_bugzilla_site <- "www.fakebugzilla123123.com"
  suppressWarnings({
    download_bugzilla_perceval_traditional_issue_comments(perceval_path, incorrect_bugzilla_site, datetime, samba_issues_comments_json_path, max_bugs=1)
  })
  file_string <- paste(readLines(samba_issues_comments_json_path))
  expect_equal(nchar(file_string), 0)
})
test_that("download_bugzilla_perceval_traditional_issue_comments() returns nonempty with correct parameters", {
  suppressWarnings({
    download_bugzilla_perceval_traditional_issue_comments(perceval_path, samba_bugzilla_site, datetime, samba_issues_comments_json_path, max_bugs=1)
  })
  file_string <- paste(readLines(samba_issues_comments_json_path))
  expect_equal(nchar(file_string) != 0, TRUE)
})
test_that("download_bugzilla_perceval_rest_issue_comments() returns correct error with incorrect perceval path", {
  incorrect_perceval_path <- "incorrect/path/to/perceval"
  expect_error(download_bugzilla_perceval_rest_issue_comments(incorrect_perceval_path, redhat_bugzilla_site, datetime, redhat_issues_comments_json_path), "error in running command")
})
test_that("download_bugzilla_perceval_rest_issue_comments() returns correct error with incorrect bugzilla site", {
  incorrect_bugzilla_site <- "www.fakebugzilla123123.com"
  suppressWarnings({
    expect_error(download_bugzilla_perceval_rest_issue_comments(perceval_path, incorrect_bugzilla_site, datetime, redhat_issues_comments_json_path, max_bugs=1))
  })
})
test_that("download_bugzilla_perceval_rest_issue_comments() returns nonempty with correct parameters", {
  suppressWarnings({
    download_bugzilla_perceval_rest_issue_comments(perceval_path, redhat_bugzilla_site, datetime, redhat_issues_comments_json_path, max_bugs=1)
  })
  file_string <- paste(readLines(samba_issues_comments_json_path))
  expect_equal(nchar(file_string) != 0, TRUE)
})

