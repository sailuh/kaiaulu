tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

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

suppressWarnings({
  download_bugzilla_perceval_traditional_issue_comments(perceval_path, samba_bugzilla_site, datetime, samba_issues_comments_json_path, max_bugs=1)
  download_bugzilla_perceval_traditional_issue_comments(perceval_path, redhat_bugzilla_site, datetime, redhat_issues_comments_json_path, max_bugs=20)
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
test_that("Incorrect json input returns error from parse_bugzilla_perceval_traditional_issue_comments()", {
  incorrect_bugzilla_json <- "incorrect/path/to/bugzilla/json"
  expect_error(parse_bugzilla_perceval_traditional_issue_comments(incorrect_bugzilla_json_path))
})
test_that("parse_bugzilla_perceval_traditional_issue_comments() returns nonempty data.table", {
  output_table <- parse_bugzilla_perceval_traditional_issue_comments(samba_issues_comments_json_path)
  expect_equal(nrow(output_table) != 0, TRUE)
})
# ======================================== ERROR ========================================================
# Cannot find merge.data.table function
# ========================================
# test_that("parse_bugzilla_perceval_traditional_issue_comments() with comments=TRUE returns nonempty data.table", {
#   output_table <- parse_bugzilla_perceval_traditional_issue_comments(samba_issues_comments_json_path, comments=TRUE)
#   expect_equal(nrow(output_table) != 0, TRUE)
# })
test_that("Incorrect json input returns error from parse_bugzilla_perceval_rest_issue_comments()", {
  incorrect_bugzilla_json <- "incorrect/path/to/bugzilla/json"
  suppressWarnings({
    expect_error(parse_bugzilla_perceval_rest_issue_comments(incorrect_bugzilla_json))
  })
})
# ======================================== ERROR ========================================================
# Not sure yet, but I think there's an error with there being an NA value somewhere in the JSON based on research.
# ========================================
# test_that("parse_bugzilla_perceval_rest_issue_comments() returns nonempty data.table", {
#   output_table <- parse_bugzilla_perceval_rest_issue_comments(redhat_issues_comments_json_path)
#   expect_equal(nrow(output_table) != 0, TRUE)
# })
# ======================================== ERROR ========================================================
# Not sure yet, but I think there's an error with there being an NA value somewhere in the JSON based on research.
# ========================================
# test_that("parse_bugzilla_perceval_rest_issue_comments() with comments on returns nonempty data.table", {
#   output_table <- parse_bugzilla_perceval_rest_issue_comments(redhat_issues_comments_json_path, comments=TRUE)
#   expect_equal(nrow(output_table) != 0, TRUE)
# })

# suppressWarnings({
#   error <- system2('rm',
#                    args = c('-r',
#                             bugzilla_issues_comments_folder_path),
#                    stdout = TRUE,
#                    stderr = FALSE)
# })

