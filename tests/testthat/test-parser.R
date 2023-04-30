test_that("parse_bugzilla_rest_issues() returns correct error with incorrect issues_folder_path", {
  incorrect_issues_folder_path <- "nonexistent/path/to/issues/folder"
  expect_error(parse_bugzilla_rest_issues(incorrect_issues_folder_path))
})
test_that("parse_bugzilla_rest_issues() returns nonempty data.table object", {
  bugzilla_site = "https://bugzilla.redhat.com"
  start_timestamp <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  issues_folder_path <- path.expand("/tmp/bugzilla_rest_issues/")
  suppressWarnings({
    error <- system2('mkdir',
                     args = c(issues_folder_path),
                     stdout = TRUE,
                     stderr = FALSE)
  })
  download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  output_table <- parse_bugzilla_rest_issues(issues_folder_path)
  expect_equal(nrow(output_table) != 0, TRUE)
  error <- system2('rm',
                   args = c('-r',
                            issues_folder_path),
                   stdout = TRUE,
                   stderr = FALSE)
})
test_that("parse_bugzilla_rest_comments() returns correct error with incorrect issues_folder_path", {
  incorrect_issues_folder_path <- "nonexistent/path/to/issues/folder"
  expect_error(parse_bugzilla_rest_comments(incorrect_issues_folder_path))
})
test_that("parse_bugzilla_rest_comments() returns nonempty data.table object", {
  bugzilla_site = "https://bugzilla.redhat.com"
  start_timestamp <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  issues_folder_path <- path.expand("/tmp/bugzilla_rest_issues/")
  comments_folder_path <- path.expand("/tmp/bugzilla_rest_comments/")
  suppressWarnings({
    error <- system2('mkdir',
                     args = c(issues_folder_path, comments_folder_path),
                     stdout = TRUE,
                     stderr = FALSE)
  })
  bug_ids <- download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  download_bugzilla_rest_comments(bugzilla_site, bug_ids[1], comments_folder_path)
  output_table <- parse_bugzilla_rest_comments(comments_folder_path)
  expect_equal(nrow(output_table) != 0, TRUE)
  error <- system2('rm',
                   args = c('-r',
                            issues_folder_path, comments_folder_path),
                   stdout = TRUE,
                   stderr = FALSE)
})
test_that("parse_bugzilla_rest_issues_comments() returns correct error with incorrect issues_folder_path", {
  incorrect_issues_folder_path <- "nonexistent/path/to/issues/folder"
  expect_error(parse_bugzilla_rest_issues_comments(incorrect_issues_folder_path))
})
test_that("parse_bugzilla_rest_issues_comments() returns nonempty data.table object", {
  bugzilla_site = "https://bugzilla.redhat.com"
  start_timestamp <- strftime(as.POSIXlt(Sys.time() - 86400, "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  issues_comments_path <- path.expand("/tmp/bugzilla_rest_issues/")
  suppressWarnings({
    error <- system2('mkdir',
                     args = c(issues_comments_path),
                     stdout = TRUE,
                     stderr = FALSE)
  })
  download_bugzilla_rest_issues_comments(bugzilla_site, start_timestamp, issues_comments_path)
  output_table <- parse_bugzilla_rest_issues_comments(issues_comments_path)
  expect_equal(nrow(output_table) != 0, TRUE)
  error <- system2('rm',
                   args = c('-r',
                            issues_comments_path),
                   stdout = TRUE,
                   stderr = FALSE)
})
