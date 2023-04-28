test_that("Using Perceval version 0.12.24", {
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
test_that("Incorrect mbox path to parse_mbox returns empty table", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  incorrect_mbox_path <- "/incorrect/path/to/mbox"
  output <- parse_mbox(perceval_path, incorrect_mbox_path)
  expect_equal(nrow(output), 0)
})
test_that("Incorrect jira issue comments path fails parse_jira", {
  incorrect_jira_issue_comments_path <- "/incorrect/path/to/jira_issue_comments"
  expect_error(parse_jira(incorrect_jira_issue_comments_path), "cannot open the connection")
})
