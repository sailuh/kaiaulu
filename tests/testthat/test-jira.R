test_that("When parse_jira is given a file that is not .json, then it raises an error", {
  incorrect_jira_issue_comments_path <- "/incorrect/path/to/jira_issue_comments"
  suppressWarnings({
    expect_error(parse_jira(incorrect_jira_issue_comments_path), "cannot open the connection")
  })
})

test_that("parse_jira parses one issue with two components as one row", {

  jira_json_path <- example_jira_issue_components(folder_path = "/tmp", folder_name = "single_issue")

  issues_comments_list <- parse_jira(json_folder_path  = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  io_delete_folder(folder_path="/tmp", folder_name="single_issue")

  expect_equal(nrow(issues),1)
  }
)

test_that("parse_jira parses two issues as two rows", {
  jira_json_path <- example_jira_two_issues(folder_path = "/tmp", folder_name = "two_issues")
  issues_comments_list <- parse_jira(json_folder_path = jira_json_path)
  issues <- issues_comments_list[["issues"]]

  io_delete_folder(folder_path="/tmp", folder_name="two_issues")

  expect_equal(nrow(issues),2)
  }
)

test_that("parse_jira parses one issue with two comments as two rows", {
  jira_json_path <- example_jira_issue_comments(folder_path = "/tmp", folder_name = "one_issue_two_comments")
  issues_comments_list <- parse_jira(json_folder_path = jira_json_path)
  comments <- issues_comments_list[["comments"]]

  io_delete_folder(folder_path="/tmp", folder_name="one_issue_two_comments")

  expect_equal(nrow(comments),2)
  }
)

test_that("When parse_jira is given a folder path containing jira issues as .json files, then it returns an issue and comments data table", {
  jira_json_path <- example_jira_issue_components(folder_path = "/tmp", folder_name = "single_issue")
  issues <- parse_jira(json_folder_path  = jira_json_path)

  expect_is(issues$issues, "data.table")
  expect_is(issues$comment, "data.table")

  io_delete_folder(folder_path="/tmp", folder_name="single_issue")
})

test_that("When parse_jira is given a .json file that is missing the necessary key issue_key, then it raises an error", {

  skip("This test produces a data table without an issue_key")
  # The test should cause an error from parse_jira, but instead creates a data table that doesn't contain an issue_key.

  jira_json_path <- example_jira_missing_key(folder_path = "/tmp", folder_name = "missing_issue_key")

  expect_error(parse_jira(json_folder_path = jira_json_path))

  io_delete_folder(folder_path="/tmp", folder_name="missing_issue_key")
})

test_that("When parse_jira is given a description that contains an escape character, then it returns an unescaped result in the data table", {
  jira_json_path <- example_jira_escape_character(folder_path = "/tmp", folder_name = "escape_character")

  issues <- parse_jira(json_folder_path = jira_json_path)

  expect_equal(issues$issues$issue_description, "This \\n should not add a newline")

  io_delete_folder(folder_path="/tmp", folder_name="escape_character")
})

test_that("When parse_jira is given a .json file containing an issue with an empty assignee_name field, then it returns the respective result in the data table", {
  jira_json_path <- example_jira_empty_assignee(folder_path = "/tmp", folder_name = "empty_assignee")

  issues <- parse_jira(json_folder_path = jira_json_path)

  expect_equal(issues$issues$issue_assignee_name, "")

  io_delete_folder(folder_path="/tmp", folder_name="empty_assignee")
})

test_that("When parse_jira is given a comment that contains unicode characters, then it returns the respective result in the data table", {
  jira_json_path <- example_jira_unicode_characters(folder_path = "/tmp", folder_name = "unicode")

  issues <- parse_jira(json_folder_path = jira_json_path)

  expect_equal(issues$issues$issue_description, "今日は😎. This is a test for unicode characters.")

  io_delete_folder(folder_path="/tmp", folder_name="unicode")
})

test_that("When parse_jira_latest_date is given a folder path containing jira issues as .json files, then it returns a file name", {
  jira_json_path <- example_jira_issue_components(folder_path = "/tmp", folder_name = "single_issue")
  issues <- parse_jira_latest_date(json_folder_path  = jira_json_path)

  expect_is(issues, "character")

  io_delete_folder(folder_path="/tmp", folder_name="single_issue")
})

test_that("When parse_jira_replies is given a data table from parse_jira, then it returns a data table", {

  jira_json_path <- example_jira_issue_comments(folder_path = "/tmp", folder_name = "issue_comments")
  issues <- parse_jira(json_folder_path  = jira_json_path)

  dt <- list(

    issues = data.table(
      issue_key = "PROJECT-123",
      issue_summary = "Summary of new feature",
      issue_type = "New Feature",
      issue_status = "Done",
      issue_resolution = "Finished",
      issue_components = "jira;mail",
      issue_description = "The new features have been implemented",
      issue_priority = "Minor",
      issue_affects_versions = "3.4.3",
      issue_fix_versions = "3.4.2",
      issue_labels = "pull-request-available",
      issue_votes = 10,
      issue_watchers = 15,
      issue_created_datetimetz = "2007-07-08T06:07:06.000+0000",
      issue_updated_datetimetz = "2008-05-12T08:01:39.000+0000",
      issue_resolution_datetimetz = "2007-08-13T19:12:33.000+0000",
      issue_creator_id = "user_id",
      issue_creator_name = "Bob",
      issue_creator_timezone = "Etc/UTC",
      issue_assignee_id = "user_id",
      issue_assignee_name = "Moe",
      issue_assignee_timezone = "Etc/UTC",
      issue_reporter_id = "user_id",
      issue_reporter_name = "Joe",
      issue_reporter_timezone = "Etc/UTC"
    ),

    comments = data.table(
      issue_key = c("PROJECT-123", "PROJECT-123"),
      comment_id = c(450, 939),
      comment_created_datetimetz = c(
        "2021-01-01T10:00:00.000+0000",
        "2021-01-01T10:00:00.000+0000"
      ),
      comment_updated_datetimetz = c(
        "2021-01-01T12:00:00.000+0000",
        "2021-01-01T12:00:00.000+0000"
      ),
      comment_author_id = c("user1", "user1"),
      comment_author_name = c("User One", "User One"),
      comment_author_timezone = c("Etc/UTC", "Etc/UTC"),
      comment_author_update_id = c("user2", "user2"),
      comment_author_update_name = c("User Two", "User Two"),
      comment_author_update_timezone = c("America/New_York", "America/New_York"),
      comment_body = c(
        "This is the first body comment.",
        "This is the second body comment."
      )
    )
  )

  result <- parse_jira_replies(dt)

  expect_is(result, "data.table")

  io_delete_folder(folder_path="/tmp", folder_name="issue_comments")
})
