test_that("parsing one issue with two components returns a single row table", {

  jira_json_path <- example_jira_issue_tracker_components(folder_path = "/tmp")

  issues_comments_list <- parse_jira(json_path = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  jira_delete_sample_log(folder_path = "/tmp")

  expect_equal(nrow(issues),1)
})

test_that("calling parse_jira on issue tracker issues with two issues returns a table with 2 rows", {
  jira_json_path <- example_jira_issue_tracker(folder_path = "/tmp")
  issues_comments_list <- parse_jira(json_path = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  jira_delete_sample_log(folder_path = "/tmp")

  expect_equal(nrow(issues),2)
})

test_that("calling parse_jira on issue_tracker comments with one issue with 2 comments returns a table with 2 rows", {
  jira_json_path <- example_jira_issue_tracker_comments(folder_path = "/tmp")
  issues_comments_list <- parse_jira(json_path = jira_json_path)
  comments <- issues_comments_list[["comments"]]

  jira_delete_sample_log(folder_path = "/tmp")

  expect_equal(nrow(comments),2)
})
