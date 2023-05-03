tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

bugzilla_site = "https://bugzilla.redhat.com"
start_timestamp <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
issues_folder_path <- path.expand("/tmp/bugzilla_rest_issues/")
comments_folder_path <- path.expand("/tmp/bugzilla_rest_comments/")
issues_comments_path <- path.expand("/tmp/bugzilla_rest_issues/")

suppressWarnings({
  error <- system2('mkdir',
                   args = c(issues_folder_path, comments_folder_path, issues_comments_path),
                   stdout = TRUE,
                   stderr = FALSE)
})


test_that("download_bugzilla_rest_issues() returns correct error with incorrect bugzilla site", {
  incorrect_bugzilla_site = "www.incorrect_bugzilla123123.com"
  expect_error(download_bugzilla_rest_issues(incorrect_bugzilla_site, start_timestamp, issues_folder_path))
})
test_that("download_bugzilla_rest_issues() successfully outputs issues in save_folder_path", {
  download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  check_files <- list.files(issues_folder_path)
  expect_equal(length(check_files) != 0, TRUE)
})
test_that("download_bugzilla_rest_issues() returns bug ids", {
  bug_ids <- download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  expect(length(bug_ids) != 0, TRUE)
})
test_that("download_bugzilla_rest_comments() returns correct error with incorrect bugzilla site", {
  incorrect_bugzilla_site <- "https://incorrect_bugzilla_123123.com"
  bug_ids <- download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  expect_error(download_bugzilla_rest_comments(incorrect_bugzilla_site, bug_ids[1], comments_folder_path))
})
test_that("download_bugzilla_rest_comments() successfully outputs issues in save_folder_path", {
  bug_ids <- download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  download_bugzilla_rest_comments(bugzilla_site, bug_ids[1], comments_folder_path)
  check_files <- list.files(comments_folder_path)
  expect_equal(length(check_files) != 0, TRUE)
})
test_that("download_bugzilla_rest_issues_comments() returns correct error with incorrect bugzilla site", {
  incorrect_bugzilla_site = "https://incorrect_bugzilla_123123.com"
  expect_error(download_bugzilla_rest_issues_comments(incorrect_bugzilla_site, start_timestamp, issues_comments_folder_path))
})
test_that("download_bugzilla_rest_issues_comments() successfully outputs issues and comments in save_file_path", {
  download_bugzilla_rest_issues_comments(bugzilla_site, start_timestamp, issues_comments_folder_path)
  check_files <- list.files(issues_comments_folder_path)
  expect_equal(length(check_files) != 0, TRUE)
})

suppressWarnings({
  error <- system2('rm',
                   args = c('-r',
                            issues_folder_path, comments_folder_path, issues_comments_path),
                   stdout = TRUE,
                   stderr = FALSE)
})
