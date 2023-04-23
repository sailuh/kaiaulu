test_that("Using Perceval version other than 0.12.24 will fail for parse_mbox", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_version <- system2(perceval_path, args="--version", stdout=TRUE, stderr=FALSE)
  expect_equal(perceval_version, "perceval 0.12.24")
})
test_that("Correct perceval path", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  expect_equal(file.exists(perceval_path), TRUE)
})
test_that("Correct mbox path", {
  conf_path <-"../../conf/thrift.yml"
  conf <- yaml::read_yaml(conf_path)
  mbox_path <- conf[["mailing_list"]][["mbox"]]
  expect_equal(file.exists(mbox_path), TRUE)
})
test_that("Correct jira issues comments path", {
  conf_path <-"../../conf/thrift.yml"
  conf <- yaml::read_yaml(conf_path)
  jira_issue_comments_path <- conf[["issue_tracker"]][["jira"]][["issue_comments"]]
  expect_equal(file.exists(jira_issue_comments_path), TRUE)
})
test_that("Incorrect perceval path fails parse_mbox", {
  conf_path <-"../../conf/thrift.yml"
  conf <- yaml::read_yaml(conf_path)
  mbox_path <- conf[["mailing_list"]][["mbox"]]
  incorrect_perceval_path <- "/incorrect/path/to/perceval"
  expect_error(parse_mbox(incorrect_perceval_path, mbox_path), "error in running command")
})
test_that("Incorrect mbox path fails parse_mbox", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  incorrect_mbox_path <- "/incorrect/path/to/mbox"
  expect_error(parse_mbox(perceval_path, incorrect_mbox_path), "Items of 'old' not found in column names: [data.body]. Consider skip_absent=TRUE.")
})
test_that("Incorrect jira issue comments path fails parse_jira", {
  incorrect_jira_issue_comments_path <- "/incorrect/path/to/jira_issue_comments"
  expect_error(parse_jira(incorrect_jira_issue_comments_path), "cannot open the connection")
})
test_that("Parsers produce tables with correct columns and expected size", {
  tools_path <- "../../tools.yml"
  conf_path <-"../../conf/thrift.yml"
  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  perceval_path <- tool[["perceval"]]
  mbox_path <- conf[["mailing_list"]][["mbox"]]
  jira_issue_comments_path <- conf[["issue_tracker"]][["jira"]][["issue_comments"]]
  mbox_table <- parse_mbox(perceval_path, mbox_path)
  jira_table <- parse_jira(jira_issue_comments_path)
  jira_issues <- jira_table[["issues"]]
  jira_comments <- jira_table[["comments"]]
  expected_mbox_columns <- c("reply_id",
                       "in_reply_to_id",
                       "reply_datetimetz",
                       "reply_from",
                       "reply_to",
                       "reply_cc",
                       "reply_subject",
                       "reply_body")
  expected_jira_issue_columns <- c("issue_key",
                     "issue_summary",
                     "issue_type",
                     "issue_status",
                     "issue_resolution",
                     "issue_components",
                     "issue_description",
                     "issue_created_datetimetz",
                     "issue_updated_datetimetz",
                     "issue_resolution_datetimetz",
                     "issue_creator_id",
                     "issue_creator_name",
                     "issue_creator_timezone",
                     "issue_assignee_id",
                     "issue_assignee_name",
                     "issue_assignee_timezone",
                     "issue_reporter_id",
                     "issue_reporter_name",
                     "issue_reporter_timezone")
  expected_jira_comments_columns <- c(
    "issue_key",
    "comment_id",
    "comment_created_datetimetz",
    "comment_updated_datetimetz",
    "comment_author_id",
    "comment_author_name",
    "comment_author_timezone",
    "comment_author_update_id",
    "comment_author_update_name",
    "comment_author_update_timezone",
    "comment_body"
  )
  # Check that the parsers output tables
  expect_s3_class(mbox_table, "data.table")
  expect_s3_class(jira_issues, "data.table")
  expect_s3_class(jira_comments, "data.table")
  # Check that the tables are the correct size
  expect_gte(nrow(mbox_table), 1)
  expect_gte(nrow(jira_issues), 1)
  expect_gte(nrow(jira_comments), 0)
  # Check the table columns match what is used in the functions in parser.r
  expect_equal(ncol(mbox_table), 8)
  expect_equal(ncol(jira_issues), 19)
  expect_equal(colnames(mbox_table), expected_mbox_columns)
  expect_equal(sort(colnames(jira_issues)), sort(expected_jira_issue_columns))
  # Check the jira comments only if there were any
  if (nrow(jira_comments > 0)){
    expect_equal(ncol(jira_comments), 11)
    expect_equal(sort(colnames(jira_comments)), sort(expected_jira_comments_columns))
  }
})
