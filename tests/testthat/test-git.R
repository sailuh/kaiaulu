test_that("Calling git_checkout with correct branch name and an exist local path of github project returns list of string", {
  git_repo_path <- suppressWarnings(git_create_sample_log())
  branch_name <- ''
  result <- git_checkout(branch_name, git_repo_path)
  expect_no_error(result)
  expect_no_warning(result)
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling git_checkout with incorrect branch name and an exist local path of github project returns warning", {
  git_repo_path <- suppressWarnings(git_create_sample_log())
  branch_name <- "mas"
  expect_warning(git_checkout(branch_name, git_repo_path))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling git_checkout with correct branch name and non-exist local path of github project returns warning", {
  branch_name <- 'master'
  git_repo_path <- "~/Documents/some_random_path"
  expect_warning(git_checkout(branch_name, git_repo_path))
})

