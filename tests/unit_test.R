library(kaiaulu)
library(testthat)

test_that("calling parse_gitlog with correct perceval path and an exist local path of github project", {
  git_repo_path <- "~/Documents/GitHub/thrift/.git"
  perceval_path <- "/home/mliu0141/.local/bin/perceval"

  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_is(result, "data.frame")
})

test_that("calling parse_gitlog with incorrect perceval path and non-exist local github repository path", {
  git_repo_path <- "~/Documents/thrift/.git"
  perceval_path <- "/home/mliu0141/.local/bin/perceval"

  expect_error(expect_warning(parse_gitlog(perceval_path, git_repo_path)))
})

test_that("calling git_checkout with correct branch name and an exist local path of github project", {
  branch_name <- "master"
  git_repo_path <- "~/Documents/GitHub/thrift/.git"
  result1 <- "Your branch is up to date with 'origin/master'."
  result2 <- "Your branch is behind 'origin/master"
  expect_match(git_checkout(branch_name, git_repo_path), paste(result1, result2, sep = "|"))
})

test_that("calling git_checkout with incorrect branch name and an exist local path of github project", {
  branch_name <- "mas"
  git_repo_path <- "~/Documents/GitHub/thrift/.git"
  expect_warning(git_checkout(branch_name, git_repo_path))
})

test_that("calling git_checkout with incorrect branch name and non-exist local path of github project", {
  branch_name <- "master"
  git_repo_path <- "~/Documents/thrift/.git"
  expect_warning(git_checkout(branch_name, git_repo_path))
})

