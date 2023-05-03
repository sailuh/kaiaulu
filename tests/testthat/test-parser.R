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

test_that("Perceval version 0.12.24 is being used", {
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  perceval_version <- system2(perceval_path, args="--version", stdout=TRUE, stderr=FALSE)
  expect_equal(perceval_version, "perceval 0.12.24")
})
test_that("Perceval path in tools.yml is specified correctly", {
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  expect_equal(file.exists(perceval_path), TRUE)
})
test_that("Configuration files are placed on recommended path", {
  expect_equal(file.exists(conf_path), TRUE)
})
test_that("Incorrect perceval path fails parse_mbox", {
  conf <- yaml::read_yaml(conf_path)
  mbox_path <- conf[["mailing_list"]][["mbox"]]
  incorrect_perceval_path <- "/incorrect/path/to/perceval"
  expect_error(parse_mbox(incorrect_perceval_path, mbox_path), "error in running command")
})
test_that("Incorrect mbox path to parse_mbox returns empty table", {
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  incorrect_mbox_path <- "/incorrect/path/to/mbox"
  output <- parse_mbox(perceval_path, incorrect_mbox_path)
  expect_equal(nrow(output), 0)
})
test_that("Incorrect jira issue comments path fails parse_jira", {
  incorrect_jira_issue_comments_path <- "/incorrect/path/to/jira_issue_comments"
  suppressWarnings({
    expect_error(parse_jira(incorrect_jira_issue_comments_path), "cannot open the connection")
  })
})

test_that("Correct git repo path", {
  git_repo_path <- suppressWarnings(git_create_sample_log())
  expect_equal(file.exists(git_repo_path), TRUE)
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_gitlog with correct perceval and correct git log path returns a data table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log())
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_is(result, "data.table")
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_gitlog with incorrect perceval path returns correct error", {
  git_repo_path <- suppressWarnings(git_create_sample_log())
  incorrect_perceval_path <- "incorrect/path/to/perceval"
  expect_error(parse_gitlog(perceval_path, git_repo_path))
})

test_that("Calling parse_gitlog with incorrect git repo path returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  incorrect_repo_path <- "incorrect/path/to/git_repo.git"
  suppressWarnings({
    expect_error(parse_gitlog(perceval_path, incorrect_repo_path))
  })
})









test_that("parse_bugzilla_rest_issues() returns correct error with incorrect issues_folder_path", {
  incorrect_issues_folder_path <- "nonexistent/path/to/issues/folder"
  expect_error(parse_bugzilla_rest_issues(incorrect_issues_folder_path))
})
test_that("parse_bugzilla_rest_issues() returns nonempty data.table object", {
  download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  output_table <- parse_bugzilla_rest_issues(issues_folder_path)
  expect_equal(nrow(output_table) != 0, TRUE)
})
test_that("parse_bugzilla_rest_comments() returns correct error with incorrect issues_folder_path", {
  incorrect_issues_folder_path <- "nonexistent/path/to/issues/folder"
  expect_error(parse_bugzilla_rest_comments(incorrect_issues_folder_path))
})
test_that("parse_bugzilla_rest_comments() returns nonempty data.table object", {
  bug_ids <- download_bugzilla_rest_issues(bugzilla_site, start_timestamp, issues_folder_path)
  download_bugzilla_rest_comments(bugzilla_site, bug_ids[1], comments_folder_path)
  output_table <- parse_bugzilla_rest_comments(comments_folder_path)
  expect_equal(nrow(output_table) != 0, TRUE)
})
test_that("parse_bugzilla_rest_issues_comments() returns correct error with incorrect issues_folder_path", {
  incorrect_issues_folder_path <- "nonexistent/path/to/issues/folder"
  expect_error(parse_bugzilla_rest_issues_comments(incorrect_issues_folder_path))
})
test_that("parse_bugzilla_rest_issues_comments() returns nonempty data.table object", {
  download_bugzilla_rest_issues_comments(bugzilla_site, start_timestamp, issues_comments_path)
  output_table <- parse_bugzilla_rest_issues_comments(issues_comments_path)
  expect_equal(nrow(output_table) != 0, TRUE)
})

suppressWarnings({
  error <- system2('rm',
                   args = c('-r',
                            issues_folder_path, comments_folder_path, issues_comments_path),
                   stdout = TRUE,
                   stderr = FALSE)
})

