test_that("parsing one issue with two components returns a single row table", {

  jira_json_path <- jira_create_sample_log(folder_path = "/tmp")

  issues_comments_list <- parse_jira(json_path = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  jira_delete_sample_log(folder_path = "/tmp")

  expect_equal(nrow(issues),1)
})
