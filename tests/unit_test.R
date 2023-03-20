library(kaiaulu)
library(testthat)

tools_path <- "../tools.yml"
conf_path <- "../conf/openssl.yml"
tool <- yaml::read_yaml(tools_path)
conf <- yaml::read_yaml(conf_path)

test_that("test that calling parse_ghitlog with correct perceval and correct git log path returns a data table", {
  perceval_path <- tool[["perceval"]]
  git_repo_path <- conf[["version_control"]][["log"]]
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_is(result, "data.table")
})

test_that("test that calling parse_gitlog with incorrect perceval path and non-exist local github repository path returns error", {
  git_repo_path <- "~/Documents/some_random_project_path"
  perceval_path <- "/homeuser/"
  expect_error(expect_warning(parse_gitlog(perceval_path, git_repo_path)))
})

test_that("test that calling git_checkout with correct branch name and an exist local path of github project returns list of string", {
  git_repo_path <- conf[["version_control"]][["log"]]
  branch_name <- conf[["version_control"]][["branch"]][1]
  expected_result1 <- "Your branch is up to date"
  expected_result2 <- "Your branch is behind"
  result <- git_checkout(branch_name, git_repo_path)
  expect_true(any(grepl(expected_result1, result[1]), grepl(expected_result2, result[1])))
})

test_that("test that calling git_checkout with incorrect branch name and an exist local path of github project returns warning", {
  git_repo_path <- conf[["version_control"]][["log"]]
  branch_name <- "mas"
  expect_warning(git_checkout(branch_name, git_repo_path))
})

test_that("test that calling git_checkout with correct branch name and non-exist local path of github project returns warning", {
  branch_name <- conf[["version_control"]][["branch"]][1]
  git_repo_path <- "~/Documents/some_random_path"
  expect_warning(git_checkout(branch_name, git_repo_path))
})

