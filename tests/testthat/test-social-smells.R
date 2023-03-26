test_that("Perceval version is 0.12.24", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <-tool[["perceval"]]
  perceval_version <- system2(perceval_path, args="--version", stdout=TRUE, stderr=FALSE)
  expect_equal(perceval_version, "perceval 0.12.24")
})

test_that("Perceval path works for parse mbox", {
  tools_path <- "../../tools.yml"
  conf_path <- "../../conf/thrift.yml"
  tool <- yaml::read_yaml(tools_path)
  conf <- yaml::read_yaml(conf_path)
  perceval_path <-tool[["perceval"]]
  mbox_path <- paste("../", conf[["mailing_list"]][["mbox"]], sep="")
  expect_no_error(parse_mbox(perceval_path, mbox_path))
})

# test_that("Correct config file parameters and Perceval version 0.12.24 work", {
#   perceval_path <- "../../../tools/bin/perceval"
#   mbox_path <- "../../../../rawdata/mbox/project.mbox"
#   jira_issue_comments_path <- "../../../../rawdata/issue_tracker/project_issue_comments.json"
#   expect_no_error(parse_mbox(perceval_path, mbox_path))
#   expect_no_error(parse_jira(jira_issue_comments_path))
# })
# test_that("Incorrect paths to files passed to parse functions fails", {
#   perceval_path <- "../../../tools/bin/perceval"
#   perceval_path_bad <- "/bad/perceval/path"
#   mbox_path <- "../../../../rawdata/mbox/project.mbox"
#   mbox_path_bad <- "/bad/mbox/path"
#   jira_issue_comments_path_bad <- "/bad/jira/path"
#   expect_error(parse_mbox(perceval_path_bad, mbox_path))
#   expect_error(parse_mbox(perceval_path, mbox_path_bad))
#   expect_warning(expect_error(parse_jira(jira_issue_comments_path_bad)))
# })
# test_that("Empty file path passed to parse functions fails", {
#   perceval_path <- "../../../tools/bin/perceval"
#   mbox_path <- "../../../../rawdata/mbox/project.mbox"
#   # If a user forgot to set a config parameter, they would have something like
#   # the variable below
#   empty_path <-
#   # What happens if config parameter(s) left blank
#   expect_error(parse_mbox(perceval_path, empty_path))
#   expect_error(parse_mbox(empty_path, perceval_path))
#   expect_error(parse_mbox(empty_path, empty_path))
#   expect_error(parse_jira(empty_path))
# })
# test_that("Absolute filepaths passed to parsers work", {
#   perceval_path_rel <- "../../../tools/bin/perceval"
#   mbox_path_rel <- "../../../../rawdata/mbox/project.mbox"
#   perceval_path_abs <- "/Users/absolute-path/tools/bin/perceval"
#   mbox_path_abs <- "/Users/absolute-path/rawdata/mbox/project.mbox"
#   jira_path_abs <- "/Users/absolute-path/rawdata/issue_tracker/project_issue_comments.json"
#   expect_no_error(parse_mbox(perceval_path_rel, mbox_path_abs))
#   expect_no_error(parse_mbox(perceval_path_abs, mbox_path_rel))
#   expect_no_error(parse_mbox(perceval_path_abs, mbox_path_abs))
#   expect_no_error(parse_jira(jira_path_abs))
# })
# test_that("Parsers produce tables with correct columns and expected size", {
#   perceval_path <- "../../../tools/bin/perceval"
#   mbox_path <- "../../../../rawdata/mbox/project.mbox"
#   jira_issue_comments_path <- "../../../../rawdata/issue_tracker/project.json"
#   mbox_table <- parse_mbox(perceval_path, mbox_path)
#   jira_table <- parse_jira(jira_issue_comments_path)
#   jira_issues <- jira_table[["issues"]]
#   jira_comments <- jira_table[["comments"]]
#   expected_mbox_columns <- c("reply_id",
#                        "in_reply_to_id",
#                        "reply_datetimetz",
#                        "reply_from",
#                        "reply_to",
#                        "reply_cc",
#                        "reply_subject",
#                        "reply_body")
#   expected_jira_issue_columns <- c("issue_key",
#                      "issue_summary",
#                      "issue_type",
#                      "issue_status",
#                      "issue_resolution",
#                      "issue_components",
#                      "issue_description",
#                      "issue_created_datetimetz",
#                      "issue_updated_datetimetz",
#                      "issue_resolution_datetimetz",
#                      "issue_creator_id",
#                      "issue_creator_name",
#                      "issue_creator_timezone",
#                      "issue_assignee_id",
#                      "issue_assignee_name",
#                      "issue_assignee_timezone",
#                      "issue_reporter_id",
#                      "issue_reporter_name",
#                      "issue_reporter_timezone")
#   expected_jira_comments_columns <- c(
#     "issue_key",
#     "comment_id",
#     "comment_created_datetimetz",
#     "comment_updated_datetimetz",
#     "comment_author_id",
#     "comment_author_name",
#     "comment_author_timezone",
#     "comment_author_update_id",
#     "comment_author_update_name",
#     "comment_author_update_timezone",
#     "comment_body"
#   )
#   # Check that the parsers output tables
#   expect_s3_class(mbox_table, "data.table")
#   expect_s3_class(jira_issues, "data.table")
#   expect_s3_class(jira_comments, "data.table")
#   # Check that the tables are the correct size
#   expect_gte(nrow(mbox_table), 1)
#   expect_gte(nrow(jira_issues), 1)
#   expect_gte(nrow(jira_comments), 0)
#   # Check the table columns match what is used in the functions in parser.r
#   expect_equal(ncol(mbox_table), 8)
#   expect_equal(ncol(jira_issues), 19)
#   expect_equal(colnames(mbox_table), expected_mbox_columns)
#   expect_equal(sort(colnames(jira_issues)), sort(expected_jira_issue_columns))
#   # Check the jira comments only if there were any
#   if (nrow(jira_comments > 0)){
#     expect_equal(ncol(jira_comments), 11)
#     expect_equal(sort(colnames(jira_comments)), sort(expected_jira_comments_columns))
#   }
# })
