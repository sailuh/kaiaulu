test_that("parse_jira parses one issue with two components as one row", {

  jira_json_path <- example_jira_issue_components(folder_path = "~/Documents",
                                                          folder_name = "single_issue")

  # issues_comments_list <- parse_jira(json_path = jira_json_path)
  # issues <- issues_comments_list[["issues"]]

  # expect_equal(nrow(issues),1)
})

# test_that("parse_jira parses two issues as two rows", {
  # jira_json_path <- example_jira_two_issues(folder_path = "~/Documents",
  #                                             folder_name = "two_issues")
  # issues_comments_list <- parse_jira(json_path = jira_json_path)
  # issues <- issues_comments_list[["issues"]]

  # expect_equal(nrow(issues),2)
# })

# test_that("parse_jira parses one issue with two comments as two rows", {
#   jira_json_path <- example_jira_issue_comments(folder_path = "/tmp",
#                                                         folder_name = "one_issue_two_comments")
#   issues_comments_list <- parse_jira(json_path = jira_json_path)
#   comments <- issues_comments_list[["comments"]]
#
#   io_delete_folder(folder_path="/tmp", folder_name="one_issue_two_comments")
#
#   expect_equal(nrow(comments),2)
# })
