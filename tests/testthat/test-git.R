tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

# Parser

test_that("Perceval version 0.12.24 is being used", {
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  perceval_version <- system2(perceval_path, args="--version", stdout=TRUE, stderr=FALSE)
  expect_equal(perceval_version, "perceval 0.12.24")
})
test_that("Perceval path in tools.yml is specified correctly", {
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  perceval_path <- path.expand(perceval_path)
  expect_equal(file.exists(perceval_path), TRUE)
})
test_that("Configuration files are placed on recommended path", {
  expect_equal(file.exists(conf_path), TRUE)
})

test_that("Correct git repo path", {
  git_repo_path <- suppressWarnings(git_create_sample_log())
  expect_equal(file.exists(git_repo_path), TRUE)
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_gitlog with correct perceval and correct git log path returns a data table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log())
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_is(result, "data.table")
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_gitlog with incorrect perceval path returns correct error", {
  git_repo_path <- suppressWarnings(git_create_sample_log())
  incorrect_perceval_path <- "incorrect/path/to/perceval"
  expect_error(parse_gitlog(perceval_path, git_repo_path))
})

test_that("Calling parse_gitlog with incorrect git repo path returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  incorrect_repo_path <- "incorrect/path/to/git_repo.git"
  suppressWarnings({
    expect_error(parse_gitlog(perceval_path, incorrect_repo_path))
  })
})

test_that("renamed file is reported on parsed git log", {
  # Create a temporary directory for the Git repository
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_renamed_file(folder_path = "/tmp",
                                        folder_name = "renamed_file_repo")

  result <- parse_gitlog(perceval_path, git_repo_path)
  renamed_filepath <- result[!is.na(file_pathname_renamed)]$file_pathname_renamed[1]
  # expect 3 commits
  expect_equal(renamed_filepath, "hi.R")
  io_delete_folder(folder_path = "/tmp",folder_name = "renamed_file_repo")

})

test_that("Calling parse_gitlog on a repo with no commits throws an error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_empty_repo(folder_path = "/tmp",folder_name = "empty_repo")

  # The real issue lies not in parse_gitlog per se, but `git_log`, or more specifically the
  # actual git command, which will throw an error if the path of the repository is unknown
  # or if the number of commits is zero. This is the git command error the system call generates
  # for zero commits:
  # fatal: your current branch 'master' does not have any commits yet
  # TODO: For some reason this error is not propagated via the system call. Should inspect why
  # in the future.
  result <- tryCatch(
    {
      result <- parse_gitlog(perceval_path, git_repo_path)
    },
    error=function(cond){
      return(NULL)
    },
    warning=function(cond){
      return(NULL)
    }
  )
  io_delete_folder(folder_path="/tmp", "empty_repo")
  expect_equal(result, NULL)
})

test_that("Calling parse_gitlog on two branches with one commit each extracts all commits", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_different_branches(folder_path = "/tmp",
                                              folder_name = "different_branches_repo")
  result <- parse_gitlog(perceval_path, git_repo_path)
  io_delete_folder(folder_path="/tmp", "different_branches_repo")
  expect_equal(nrow(result), 2)

})

test_that("Filtering parse_gitlog by commit size removes large sized commits", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_large_sized_commits(folder_path = "/tmp",
                                               folder_name = "example_large_sized_commits")

  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- result %>% filter_by_commit_size(commit_size = 3)
  io_delete_folder(folder_path="/tmp", "example_large_sized_commits")
  expect_equal(nrow(result), 1)

})


test_that("Parsing git log function entities on notebook files return an empty table", {

  skip("Newer version of ctags are capable of parsing R Notebook function declarations.
       This test will therefore fail on newer versions. Skip it or now.")

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_notebook_function_in_code_blocks(folder_path = "/tmp",
                                                            folder_name = "example_notebook_function_in_code_blocks")

  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)

  io_delete_folder(folder_path="/tmp", "example_notebook_function_in_code_blocks")
  expect_equal(nrow(result), 0)

})

test_that("Parsing git log function entities on R files return a table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")

  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)


  io_delete_folder(folder_path="/tmp", "example_function_in_files")
  expect_equal(nrow(result), 2)

})
# Git Cmd


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

