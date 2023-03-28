test_that("test that calling parse_gitlog with correct perceval and correct git log path returns a data table", {
  tools_path <- "../../tools.yml"
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- expect_warning(git_create_sample_log())
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_is(result, "data.table")
  expect_warning(git_delete_sample_log(git_repo_path))
})

test_that("test that calling parse_gitlog with incorrect perceval path and non-exist local github repository path returns error", {
  git_repo_path <- "~/Documents/some_random_project_path"
  perceval_path <- "/homeuser/"
  expect_error(expect_warning(parse_gitlog(perceval_path, git_repo_path)))
})
