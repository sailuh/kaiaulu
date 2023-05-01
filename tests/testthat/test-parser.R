test_that("Correct git repo path", {
  git_repo_path <- suppressWarnings(git_create_sample_log())
  expect_equal(file.exists(git_repo_path), TRUE)
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Check perceval exist", {
  tryCatch({
    result <- system('perceval --version', intern = TRUE)
    expect_true(grepl("perceval", result))
  },
  error = function(e) {
    an.error.occured <<- TRUE
    expect_true(an.error.occurred)
  })
})

test_that("Calling parse_gitlog with correct perceval and correct git log path returns a data table", {
  tryCatch({
    tools_path <- "../../tools.yml"
    tools_path <- file.path(tools_path)
    tool <- yaml::read_yaml(tools_path)
    perceval_path <- tool[["perceval"]]
    git_repo_path <- suppressWarnings(git_create_sample_log())
    result <- parse_gitlog(perceval_path, git_repo_path)
    expect_is(result, "data.table")
    suppressWarnings(git_delete_sample_log(git_repo_path))
  },
  error = function(e) {
    an.error.occurred <<- TRUE
    expect_true(an.error.occurred)
  })
})

test_that("Calling parse_gitlog with incorrect perceval path returns correct error", {
  tryCatch({
    git_repo_path <- suppressWarnings(git_create_sample_log())
    incorrect_perceval_path <- "incorrect/path/to/perceval"
    expect_error(parse_gitlog(perceval_path, git_repo_path))
  },
  error = function(e) {
    an.error.occured <<- TRUE
    expect_true(an.error.occurred)
  })
})

test_that("Calling parse_gitlog with incorrect git repo path returns correct error", {
  tryCatch({
    tools_path <- "../../tools.yml"
    tools_path <- file.path(tools_path)
    tool <- yaml::read_yaml(tools_path)
    perceval_path <- tool[["perceval"]]
    incorrect_repo_path <- "incorrect/path/to/git_repo.git"
    suppressWarnings({
      expect_error(parse_gitlog(perceval_path, incorrect_repo_path))
    })
  },
  error = function(e) {
    an.error.occured <<- TRUE
    expect_true(an.error.occurred)
  })
})
