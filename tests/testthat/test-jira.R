test_that("parsing one issue with two components returns a single row table", {

  jira_json_path <- example_jira_issue_tracker_components(folder_path = "/tmp")

  issues_comments_list <- parse_jira(json_path = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  jira_delete_sample_log(folder_path = "/tmp")

  expect_equal(nrow(issues),1)
})

test_that("calling parse_jira on issue tracker with two issues with no comments parses issues correctly", {
  jira_json_path <- example_jira_issue_tracker(folder_path = "/tmp")
  issues_comments_list <- parse_jira(json_path = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  jira_delete_sample_log(folder_path = "/tmp")

  expect_equal(nrow(issues),2)
})

test_that("calling parse_jira on issue_tracker with one issue with comments parses issues correctly", {
  jira_json_path <- example_jira_issue_tracker_comments(folder_path = "/tmp")
  issues_comments_list <- parse_jira(json_path = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  jira_delete_sample_log(folder_path = "/tmp")

  expect_is(issues, "data.table")
})
