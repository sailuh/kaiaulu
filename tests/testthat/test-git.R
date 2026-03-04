tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

tool <- yaml::read_yaml(tools_path)
tmp_folderpath <- tool[["tmp"]]



####################### Third Party Tools Tests ######################

test_that("Perceval is installed", {
  expect_true(nzchar(Sys.which("perceval")))
})

test_that("Git is installed", {
 expect_true(nzchar(Sys.which("git")))
})

####################### Parsers ######################
# The parsers are generally the functions called first by users and so are the
# most prone to erroneous user input. They are each responsible for parsing
# data from Git.

##### parse_gitlog() #####

test_that("Calling parse_gitlog with correct perceval and correct git log path returns a data table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_is(result, "data.table")

  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_gitlog with incorrect perceval path returns correct error", {
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
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

test_that("Calling parse_gitlog with perl_regex works correctly", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_different_branches(folder_path = "/tmp",
                                               folder_name = "example_different_branches")

  result <- parse_gitlog(perceval_path, git_repo_path, perl_regex="first")
  expect_equal(result$file_pathname, "file1.R")
  io_delete_folder(folder_path="/tmp", "example_different_branches")
})

##### parse_gitlog_entity() #####

test_that("Calling parse_gitlog_entity with correct fields returns a data table", {
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
  expect_is(result, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling parse_gitlog_entity with incorrect git repo path returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")
  incorrect_repo_path <- "incorrect/path/to/git_repo.git"
  project_git <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(parse_gitlog_entity(incorrect_repo_path=git_repo_path,
                                   utags_path = utags_path,
                                   project_git_log = project_git,
                                   kinds=list( r=c('f')),
                                   progress_bar = FALSE))
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling parse_gitlog_entity with incorrect utags path returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")
  incorrect_utags_path <- "incorrect/path/to/utags"
  project_git <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(parse_gitlog_entity(incorrect_repo_path=git_repo_path,
                                   incorrect_utags_path = utags_path,
                                   project_git_log = project_git,
                                   kinds=list( r=c('f')),
                                   progress_bar = FALSE))
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling parse_gitlog_entity with incorrect project git log returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")
  project_git <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(parse_gitlog_entity(incorrect_repo_path=git_repo_path,
                                   utags_path = utags_path,
                                   project_git_log = NULL,
                                   kinds=list( r=c('f')),
                                   progress_bar = FALSE))
  io_delete_folder("/tmp", "example_function_in_files")
})


test_that("Calling parse_gitlog_entity with incorrect kinds returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")
  project_git <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(parse_gitlog_entity(incorrect_repo_path=git_repo_path,
                                   utags_path = utags_path,
                                   project_git_log = project_git,
                                   kinds=NULL,
                                   progress_bar = FALSE))
  io_delete_folder("/tmp", "example_function_in_files")
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

test_that("Calling parse_gitlog_entity returns correct results", {
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
                                progress_bar = TRUE)
  expect_equal(result$entity[1], "car")
  expect_equal(result$entity[2], "car")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling parse_gitlog_entity with correct fields and a progress bar returns a data table", {
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
                                progress_bar = TRUE)
  expect_is(result, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling parse_gitlog_entity with kinds filter returns correct result", {
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
                                kinds=list( r=c('c')),
                                progress_bar = FALSE)
  result <- result$entity[1]
  expect_true(is.na(result))
  io_delete_folder("/tmp", "example_function_in_files")
})

# Always fails due to previous git log issue, refer to git log tests
# test_that("Calling parse_gitlog_entity with empty table returns a data table", {
#   tools_path <- file.path(tools_path)
#   tool <- yaml::read_yaml(tools_path)
#   perceval_path <- tool[["perceval"]]
#   utags_path <- tool[["utags"]]
#   git_repo_path <- example_empty_repo(folder_path = "/tmp",
#                                              folder_name = "example_empty_repo")
#   project_git <- parse_gitlog(perceval_path, git_repo_path)
#   result <- parse_gitlog_entity(git_repo_path=git_repo_path,
#                                 utags_path = utags_path,
#                                 project_git_log = project_git,
#                                 kinds=list( r=c('f')),
#                                 progress_bar = FALSE)
#   expect_is(result, "data.table")
#   io_delete_folder("/tmp", "example_empty_repo")
# })

test_that("Calling parse_gitlog_entity with correct fields returns a data table", {
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
                                kinds=list( r=c()),
                                progress_bar = FALSE)
  expect_is(result, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

##### parse_git_blame() #####

test_that("Calling parse_git_blame with correct fields returns a data table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  commit_hash <- git_head(git_repo_path)

  result <- parse_git_blame(git_repo_path, commit_hash, "hello.R")
  expect_is(result, "data.table")

  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_git_blame with NULL incorrect git repo path returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  commit_hash <- git_head(git_repo_path)
  expect_null(parse_git_blame(NULL, commit_hash, "hello.R"))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_git_blame with NULL commit_hash returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  expect_null(parse_git_blame(git_repo_path, NULL, "hello.R"))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_git_blame with correct fields returns correct data", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")

  commit_hash <- git_head(git_repo_path)

  result <- parse_git_blame(git_repo_path, commit_hash, "file1.R")

  expect_equal(result$line_n_original_file[1], "1")
  expect_equal(result$line_n_final_file[1], "1")

  io_delete_folder("/tmp", "example_function_in_files")
})


##### parse_commit_message_id() #####

test_that("Calling parse_commit_message_id() with correct fields returns a data table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_commit_message_id(result, ".*")

  expect_is(result, "data.table")
  suppressWarnings(git_delete_sample_log(git_repo_path))
})


test_that("Calling parse_commit_message_id() with incorrect git_repo_path returns correct error", {
  incorrect_parse_gitlog <- "incorrect"
  expect_error(result <- parse_commit_message_id( incorrect_parse_gitlog, ".*"))
})

test_that("Calling parse_commit_message_id() with no regex returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(result <- parse_commit_message_id(result))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling parse_commit_message_id() adds a column commit_message_id with correct data", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_commit_message_id(result, ".*")

  expect_equal(result$commit_message_id, "hello world commit")
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

####################### Git Cmd ######################
# Most of these functions are Git command wrappers. I've decided to test
# many of them based on if they can execute given proper arguments, as
# Git itself should be testing these in-depth, not Kaiaulu.

##### git_add() #####

test_that("Calling git_add() wrapper is functional", {

  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_repo_path <- git_init(folder_path)
  file_path <- io_make_file(file.path(folder_path, "test_file.R"), "print('hello world')")
  expect_error(git_add(git_repo_path, folder_path, file_path), NA)
  io_delete_folder(tmp_folderpath, "git_test")

})

test_that("Calling git_add() gives warning given invalid git_repo_path", {
  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_repo_path <- git_init(folder_path)
  file_path <- io_make_file(file.path(folder_path, "test_file.R"), "print('hello world')")
  expect_warning(git_add("invalid/path", folder_path, file_path))
  io_delete_folder(tmp_folderpath, "git_test")
})

test_that("Calling git_add() gives warning given invalid folder_path", {
  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_repo_path <- git_init(folder_path)
  file_path <- io_make_file(file.path(folder_path, "test_file.R"), "print('hello world')")
  expect_warning(git_add(git_repo_path, "incorrect/folder/path", file_path))
  io_delete_folder(tmp_folderpath, "git_test")
})

test_that("Calling git_add() gives warning given invalid file_path", {
  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_repo_path <- git_init(folder_path)
  file_path <- io_make_file(file.path(folder_path, "test_file.R"), "print('hello world')")
  expect_warning(git_add(git_repo_path, folder_path, "invalid/file/path"))
  io_delete_folder(tmp_folderpath, "git_test")
})

##### git_commit() #####

test_that("Calling git_commit() wrapper is functional", {

  folder_path <- io_make_folder("/tmp","test_folder")
  git_init(folder_path)
  git_repo_path <- file.path(folder_path, '.git')
  test_path <- file.path(folder_path, "test-hello.R")

  io_make_file(test_path, "print('tester')")
  git_add(git_repo_path, folder_path, test_path)
  expect_error(git_commit(git_repo_path, folder_path, "Commit test-example.R", "John Doe", "JohnDoe@test.com"), NA)
  io_delete_folder(tmp_folderpath, "test_folder")

})

test_that("Calling git_commit() gives warning given invalid git_repo", {
  folder_path <- io_make_folder("/tmp","test_folder")
  git_init(folder_path)
  git_repo_path <- file.path(folder_path, '.git')
  test_path <- file.path(folder_path, "test-hello.R")
  io_make_file(test_path, "print('tester')")
  git_add(git_repo_path, folder_path, test_path)
  expect_warning(git_commit("invalid/git/repo/path", folder_path, "Commit test-example.R", "John Doe", "JohnDoe@test.com"))
  io_delete_folder(tmp_folderpath, "test_folder")
})

test_that("Calling git_commit() gives warning given invalid folder_path", {
  folder_path <- io_make_folder("/tmp","test_folder")
  git_init(folder_path)
  git_repo_path <- file.path(folder_path, '.git')
  test_path <- file.path(folder_path, "test-hello.R")
  io_make_file(test_path, "print('tester')")
  git_add(git_repo_path, folder_path, test_path)
  expect_warning(git_commit(git_repo_path, "invalid/path", "Commit test-example.R", "John Doe", "JohnDoe@test.com"))
  io_delete_folder(tmp_folderpath, "test_folder")
})

##### git_init() #####

test_that("Calling git_init() with a path creates a .git folder", {

  tmp_dir <- io_make_folder(tmp_folderpath, "git_init_test")
  git_init(tmp_dir)
  expect_true(dir.exists(file.path(tmp_dir, ".git")))
  io_delete_folder(tmp_folderpath, "git_init_test")

})

# This does not produce warnings.
# test_that("Calling git_init() with incorrect folder path returns a warning", {
#   expect_warning(git_init("invalid/folder/path"))
# })

##### git_mv() #####

test_that("Calling git_mv() wrapper is functional", {

  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')
  hello_path <- file.path(folder_path, "hello.R")
  io_make_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")

  expect_error(git_mv(git_repo, folder_path, old_name = "hello.R", new_name = "hi.R"), NA)

  io_delete_folder(tmp_folderpath, "git_test")

})

test_that("Calling git_mv() with invalid git_repo returns a warning", {
  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')
  hello_path <- file.path(folder_path, "hello.R")
  io_make_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")
  expect_warning(git_mv("invalid/git/repo", folder_path, old_name = "hello.R", new_name = "hi.R"))
  io_delete_folder(tmp_folderpath, "git_test")
})

test_that("Calling git_mv() with invalid folder_path returns a warning", {
  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')
  hello_path <- file.path(folder_path, "hello.R")
  io_make_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")
  expect_warning(git_mv(git_repo, "invalid/folder/path", old_name = "hello.R", new_name = "hi.R"))
  io_delete_folder(tmp_folderpath, "git_test")
})

test_that("Calling git_mv() with invalid old_name returns a warning", {
  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')
  hello_path <- file.path(folder_path, "hello.R")
  io_make_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")
  expect_warning(git_mv(git_repo, folder_path, old_name = "hi.R", new_name = "hi.R"))
  io_delete_folder(tmp_folderpath, "git_test")
})

test_that("Calling git_mv() with invalid new_name returns a warning", {
  folder_path <- io_make_folder(tmp_folderpath, "git_test")
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')
  hello_path <- file.path(folder_path, "hello.R")
  io_make_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")
  expect_warning(git_mv(git_repo, folder_path, old_name = "hello.R", new_name = NULL))
  io_delete_folder(tmp_folderpath, "git_test")
})

##### git_checkout() #####

test_that("Calling git_checkout with correct branch name and an exist local path of github project returns list of string", {
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  branch_name <- ''
  result <- git_checkout(branch_name, git_repo_path)
  expect_no_error(result)
  expect_no_warning(result)
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling git_checkout with incorrect branch name and an exist local path of github project returns warning", {
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  branch_name <- "mas"
  expect_warning(git_checkout(branch_name, git_repo_path))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling git_checkout with correct branch name and non-exist local path of github project returns warning", {
  branch_name <- 'master'
  git_repo_path <- "~/Documents/some_random_path"
  expect_warning(git_checkout(branch_name, git_repo_path))
})

##### git_head() #####

test_that("Calling git_head() wrapper is functional", {
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  expect_type(git_head(git_repo_path), "character")
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling git_head() with incorrect git_repo_path returns a warning", {
  expect_warning(git_head("invalid/git/repo/path"))
})

##### git_log() #####

test_that("Calling git_log() wrapper is functional", {

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  save_path <- file.path(tmp_folderpath, "git_log_test.txt")
  io_make_file(save_path, "")

  git_log(git_repo_path, flags = c("--oneline"), save_path)

  expect_true(file.exists(save_path))
  io_delete_folder(tmp_folderpath, "git_log_test.txt")
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

# Does not produce a warning
# test_that("Calling git_log() with invalid git_repo_path returns a warning", {
#   save_path <- file.path(tmp_folderpath, "git_log_test.txt")
#   io_make_file(save_path, "")
#   expect_warning(git_log("invalid/git/repo/path", flags = c("--oneline"), save_path))
#   io_delete_folder(tmp_folderpath, "git_log_test.txt")
# })

# Not sure why the flags are not producing a warning
# test_that("Calling git_log() wrapper is functional", {
#   git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
#   save_path <- file.path(tmp_folderpath, "git_log_test.txt")
#   io_make_file(save_path, "")
#   expect_warning(git_log(git_repo_path, flags = --errorflags, save_path))
#   io_delete_folder(tmp_folderpath, "git_log_test.txt")
#   suppressWarnings(git_delete_sample_log(git_repo_path))
# })

# Not sure how to catch the directory error
# test_that("Calling git_log() with invalid save_path returns an error", {
#
#   git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
#   save_path <- file.path(tmp_folderpath, "git_log_test.txt")
#   io_make_file(save_path, "")
#
#   expect_error(git_log(git_repo_path, flags = c("--oneline"), "invalid/save/path"))
#
#   io_delete_folder(tmp_folderpath, "git_log_test.txt")
#   suppressWarnings(git_delete_sample_log(git_repo_path))
# })



##### git_blame() #####

test_that("Calling git_blame() wrapper is functional", {

  git_repo_path <- suppressWarnings(git_create_sample_log(folder_path = tmp_folderpath))
  commit_hash <- git_head(git_repo_path)
  expect_error(git_blame(git_repo_path, flags = c("-p"), commit_hash, "hello.R"), NA)
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

# This does not produce a warning as well
# test_that("Calling git_blame() wrapper is functional", {
#
#   git_repo_path <- suppressWarnings(git_create_sample_log(folder_path = tmp_folderpath))
#   commit_hash <- git_head(git_repo_path)
#   expect_warning(git_blame("invalid/git/repo/path", flags = c("-p"), commit_hash, "hello.R"))
#   suppressWarnings(git_delete_sample_log(git_repo_path))
# })

##### git_create_sample_log() and git_delete_sample_log() #####

test_that("Correct git repo path", {
  git_repo_path <- suppressWarnings(git_create_sample_log(folder_path = tmp_folderpath))
  expect_equal(file.exists(git_repo_path), TRUE)
  suppressWarnings(git_delete_sample_log(git_repo_path))
  expect_false(file.exists(file.path(git_repo_path, ".git")))
})

####################### Example Tests ######################
# These test for all the example functions used in the tests.

##### Example Function Unit Tests #####

test_that("Correct git repo path for example_renamed_file()", {
  git_repo_path <- suppressWarnings(example_renamed_file(folder_path = tmp_folderpath, "renamed_file_repo"))
  expect_equal(file.exists(git_repo_path), TRUE)
  io_delete_folder(tmp_folderpath, "renamed_file_repo")
})

test_that("Correct git repo path for example_empty_repo()", {
  git_repo_path <- suppressWarnings(example_empty_repo(folder_path = tmp_folderpath, "empty_repo_repo"))
  expect_equal(file.exists(git_repo_path), TRUE)
  io_delete_folder(tmp_folderpath, "empty_repo_repo")
})

test_that("Correct git repo path for example_different_branches()", {
  git_repo_path <- suppressWarnings(example_different_branches(folder_path = tmp_folderpath, "different_branches_repo"))
  expect_equal(file.exists(git_repo_path), TRUE)
  io_delete_folder(tmp_folderpath, "different_branches_repo")
})

test_that("Correct git repo path for example_large_sized_commits()", {
  git_repo_path <- suppressWarnings(example_large_sized_commits(folder_path = tmp_folderpath, "large_sized_commits_repo"))
  expect_equal(file.exists(git_repo_path), TRUE)
  io_delete_folder(tmp_folderpath, "large_sized_commits_repo")
})

test_that("Correct git repo path for example_function_in_files()", {
  git_repo_path <- suppressWarnings(example_function_in_files(folder_path = tmp_folderpath, "function_in_files_repo"))
  expect_equal(file.exists(git_repo_path), TRUE)
  io_delete_folder(tmp_folderpath, "function_in_files_repo")
})

test_that("Correct git repo path for example_notebook_alternating_function_in_files()", {
  git_repo_path <- suppressWarnings(example_notebook_alternating_function_in_files(folder_path = tmp_folderpath, "notebook_alternating_function_in_files_repo"))
  expect_equal(file.exists(git_repo_path), TRUE)
  io_delete_folder(tmp_folderpath, "notebook_alternating_function_in_files_repo")
})



####################### Network Transform ######################
# These are responsible for building the networks seen in vignettes.

##### transform_gitlog_to_bipartite_network() #####

test_that("Calling transform_gitlog_to_bipartite_network() with parsed git log and mode returns an edgelist", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- transform_gitlog_to_bipartite_network(result, mode="author-file")

  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")

  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling transform_gitlog_to_bipartite_network() without parsed git log returns correct error", {
  expect_error(transform_gitlog_to_bipartite_network(NULL, mode="author-file"))
})

test_that("Calling transform_gitlog_to_bipartite_network() with incorrect mode returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(transform_gitlog_to_bipartite_network(result, mode="writer-file"))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling transform_gitlog_to_bipartite_network() creates correct directed graph components given example_notebook_alternating_function_in_files", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- example_notebook_alternating_function_in_files(folder_path = "/tmp",
                                        folder_name = "example_function_in_files")

  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- transform_gitlog_to_bipartite_network(result, mode="author-file")

  nodes <- result$nodes
  edges <- result$edgelist

  expect_true(any(nodes$name == "dev 1 <>"))
  expect_true(any(nodes$name == "dev 2 <>"))
  expect_true(any(nodes$name == "file1.R"))

  expect_true(any(edges$from == "dev 1 <>" & edges$to == "file1.R" & edges$weight == 2))
  expect_true(any(edges$from == "dev 2 <>" & edges$to == "file1.R" & edges$weight == 2))

  io_delete_folder(folder_path = "/tmp",folder_name = "example_function_in_files")
})


##### transform_gitlog_to_entity_bipartite_network() #####

test_that("Calling transform_gitlog_to_entity_bipartite_network() with parsed git log and author-entity mode returns an edgelist", {
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
  result <- transform_gitlog_to_entity_bipartite_network(result, mode="author-entity")
  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_bipartite_network() with parsed git log and committer-entity mode returns an edgelist", {
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
  result <- transform_gitlog_to_entity_bipartite_network(result, mode="committer-entity")
  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_bipartite_network() with parsed git log and commit-entity mode returns an edgelist", {
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
  result <- transform_gitlog_to_entity_bipartite_network(result, mode="commit-entity")
  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_bipartite_network() with parsed git log and author-committer mode returns an edgelist", {
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
  result <- transform_gitlog_to_entity_bipartite_network(result, mode="author-committer")
  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_bipartite_network() without parsed git repo returns correct error", {
  expect_error(transform_gitlog_to_entity_bipartite_network(NULL, mode="author-entity"))
})

test_that("Calling transform_gitlog_to_entity_bipartite_network() with parsed git repo and incorrect mode returns correct error", {
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
  expect_error(transform_gitlog_to_entity_bipartite_network(NULL, mode="writer-entity"))
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_bipartite_network() creates correct directed graph components given example_notebook_alternating_function_in_files", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_notebook_alternating_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")
  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)
  result <- transform_gitlog_to_entity_bipartite_network(result, mode="author-entity")

  nodes <- result$nodes
  edges <- result$edgelist

  expect_true(any(nodes$name == "dev 1 <>"))
  expect_true(any(nodes$name == "dev 2 <>"))
  expect_true(any(nodes$name == "car"))

  expect_true(any(edges$from == "dev 1 <>" & edges$to == "car" & edges$weight == 2))
  expect_true(any(edges$from == "dev 2 <>" & edges$to == "car" & edges$weight == 2))

  io_delete_folder("/tmp", "example_function_in_files")
})

##### transform_gitlog_to_temporal_network() #####

test_that("Calling transform_gitlog_to_temporal_network() with correct fields in author mode returns an edgelist", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- transform_gitlog_to_temporal_network(result, mode="author", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges)

  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")

  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling transform_gitlog_to_temporal_network() with correct fields in committer mode returns an edgelist", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- transform_gitlog_to_temporal_network(result, mode="committer", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges)

  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")

  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling transform_gitlog_to_temporal_network() without parsed git log returns correct error", {
  expect_error(transform_gitlog_to_temporal_network(NULL, mode="author", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges))
})

test_that("Calling transform_gitlog_to_temporal_network() with incorrect mode returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(transform_gitlog_to_temporal_network(result, mode="writer", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling transform_gitlog_to_temporal_network() with incorrect lag returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path)
  expect_error(transform_gitlog_to_temporal_network(result, mode="author", lag="no_lag", weight_scheme_function=weight_scheme_sum_edges))
  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling transform_gitlog_to_temporal_network() creates correct directed graph components given example_notebook_alternating_function_in_files", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path <- example_notebook_alternating_function_in_files(folder_path = "/tmp",
                                                                  folder_name = "example_function_in_files")

  result <- parse_gitlog(perceval_path, git_repo_path)
  result <- transform_gitlog_to_temporal_network(result, mode="author", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges)

  nodes <- result$nodes
  edges <- result$edgelist

  expect_true(any(nodes$name == "dev 1 <>"))
  expect_true(any(nodes$name == "dev 2 <>"))

  expect_true(any(edges$from == "dev 1 <>" & edges$to == "dev 2 <>" & edges$weight == 2))
  expect_true(any(edges$from == "dev 2 <>" & edges$to == "dev 1 <>" & edges$weight == 4))

  io_delete_folder("/tmp", "example_function_in_files")
})

##### transform_gitlog_to_entity_temporal_network() #####

test_that("Calling transform_gitlog_to_entity_temporal_network() with correct fields in author mode returns an edgelist", {
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
  result <- transform_gitlog_to_entity_temporal_network(result, mode="author", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges)
  edgelist <- result$edgelist
  expect_is(edgelist, "data.table")

  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_temporal_network() in committer mode returns an edgelist", {
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
                                kinds=list(r=c('f')),
                                progress_bar = FALSE)

  committer_network <- transform_gitlog_to_entity_temporal_network(
    project_git_entity = result,
    mode = "committer",
    lag = "one_lag",
    weight_scheme_function = weight_scheme_sum_edges
  )

  expect_is(committer_network$edgelist, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_temporal_network() without parsed git entity returns correct error", {
  expect_error(transform_gitlog_to_entity_temporal_network(NULL, mode="author", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges))
})

test_that("Calling transform_gitlog_to_entity_bipartite_network() with parsed git repo and incorrect mode returns correct error", {
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
  expect_error(transform_gitlog_to_entity_temporal_network(result, mode="author", lag="no_lag", weight_scheme_function=weight_scheme_sum_edges))
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_gitlog_to_entity_temporal_network() creates correct directed graph components given example_notebook_alternating_function_in_files", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_notebook_alternating_function_in_files(folder_path = "/tmp",
                                                                  folder_name = "example_function_in_files")

  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)
  result <- transform_gitlog_to_entity_temporal_network(result, mode="author", lag="one_lag", weight_scheme_function=weight_scheme_sum_edges)

  nodes <- result$nodes
  edges <- result$edgelist

  expect_true(any(nodes$name == "dev 1 <>"))
  expect_true(any(nodes$name == "dev 2 <>"))

  expect_true(any(edges$from == "dev 1 <>" & edges$to == "dev 2 <>" & edges$weight == 8))
  expect_true(any(edges$from == "dev 2 <>" & edges$to == "dev 1 <>" & edges$weight == 16))

  io_delete_folder("/tmp", "example_function_in_files")

})

##### transform_commit_message_id_to_network() #####

test_that("Calling transform_commit_message_id_to_network() with correct arguments returns an edgelist", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")

  result <- parse_gitlog(perceval_path, git_repo_path)

  result <- transform_commit_message_id_to_network(result, ".*")

  expect_is(result$edgelist, "data.table")
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_commit_message_id_to_network() with incorrect git log returns correct error", {
  expect_error(transform_commit_message_id_to_network(NULL, ".*"))
})

test_that("Calling transform_commit_message_id_to_network() with incorrect hash returns correct error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")

  result <- parse_gitlog(perceval_path, git_repo_path)
  hash <- git_head(git_repo_path)

  expect_error(transform_commit_message_id_to_network(result, NULL))
  io_delete_folder("/tmp", "example_function_in_files")
})

test_that("Calling transform_commit_message_id_to_network() creates correct directed graph components given example_notebook_alternating_function_in_files", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = "/tmp",
                                             folder_name = "example_function_in_files")

  result <- parse_gitlog(perceval_path, git_repo_path)

  result <- transform_commit_message_id_to_network(result, "first file")

  nodes <- result$nodes
  edges <- result$edgelist

  expect_true(any(nodes$name == "first file"))
  expect_true(any(nodes$name == "file1.R"))

  expect_true(any(edges$from == "first file" & edges$to == "file1.R" & edges$weight == 2))

  io_delete_folder("/tmp", "example_function_in_files")
})

####################### Get Functions ######################
# These are functions from config.R used in gitlog_showcase
# and gitlog_entity_showcase.

##### get_git_repo_path() #####

test_that("Calling get_git_repo_path returns the git repo path", {
  config_path <- "testdata/thrift.yml"
  result <- parse_config(config_path)
  result <- get_git_repo_path(result)
  expect_equal(result, "../../rawdata/thrift/git_repo/.git")
})


test_that("Calling get_git_repo_path with incorrect pathing returns correct error", {
  expect_error(get_git_repo_path("/incorrect/path"))
})

##### get_file_extensions() #####

test_that("Calling get_file_extensions returns the correct file extensions", {
  config_path <- "testdata/thrift.yml"
  result <- parse_config(config_path)
  result <- get_file_extensions(result)
  expected <- c("cpp", "c", "h", "java", "js", "py", "cc")
  expect_equal(result, expected)
})

test_that("Calling get_file_extensions with incorrect pathing returns correct error", {
  expect_error(get_file_extensions("/incorrect/path"))
})

##### get_substring_filepath() #####

test_that("Calling get_substring_filepath returns the correct file extesnions", {
  config_path <- "testdata/thrift.yml"
  result <- parse_config(config_path)
  result <- get_substring_filepath(result)
  expect_equal(result, "test")
})

test_that("Calling get_substring_filepath with incorrect pathing returns correct error", {
  expect_error(get_substring_filepath("/incorrect/path"))
})

##### get_uctags_line_types() #####

test_that("Calling get_uctags_line_types returns a list", {
  config_path <- "testdata/thrift.yml"
  result <- parse_config(config_path)
  result <- get_uctags_line_types(result)
  expect_is(result, "list")
})

test_that("Calling get_uctags_line_types with incorrect pathing returns correct error", {
  expect_error(get_uctags_line_types("/incorrect/path"))
})

####################### Identity Match ######################
# This is a function used in both Notebooks that masks personal
# user data.

##### identity_match() #####
test_that("Calling identity_match() with correct arguments will not error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path0 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  git_repo_path1 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path0)
  result1 <- parse_gitlog(perceval_path, git_repo_path1)
  git_logs <- list(result, result1)
  expect_error(result <- identity_match(
    project_log = git_logs,
    name_column = c("author_name_email", "author_name_email"),
    assign_identity_function = assign_exact_identity,
    use_name_only = FALSE,
    label = "identity_id"
  ), NA)
  suppressWarnings(git_delete_sample_log(git_repo_path0))
  suppressWarnings(git_delete_sample_log(git_repo_path1))
})

test_that("Calling identity_match() with incorrect project_log will error", {
  expect_error(result <- identity_match(
    project_log = "incorrect_project_log",
    name_column = c("author_name_email", "author_name_email"),
    assign_identity_function = assign_exact_identity,
    use_name_only = FALSE,
    label = "identity_id"
  ))
})

test_that("Calling identity_match() with invalid name_column will error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path0 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  git_repo_path1 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path0)
  result1 <- parse_gitlog(perceval_path, git_repo_path1)
  git_logs <- list(result, result1)
  expect_error(result <- identity_match(
    project_log = git_logs,
    name_column = c("invalid_author_name_email", "invalid_author_name_email"),
    assign_identity_function = assign_exact_identity,
    use_name_only = FALSE,
    label = "identity_id"
  ))
  suppressWarnings(git_delete_sample_log(git_repo_path0))
  suppressWarnings(git_delete_sample_log(git_repo_path1))
})

test_that("Calling identity_match() with invalid assign_identity_function will error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path0 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  git_repo_path1 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path0)
  result1 <- parse_gitlog(perceval_path, git_repo_path1)
  git_logs <- list(result, result1)
  expect_error(result <- identity_match(
    project_log = git_logs,
    name_column = c("author_name_email", "author_name_email"),
    assign_identity_function = invalid_function,
    use_name_only = FALSE,
    label = "identity_id"
  ))
  suppressWarnings(git_delete_sample_log(git_repo_path0))
  suppressWarnings(git_delete_sample_log(git_repo_path1))
})

test_that("Calling identity_match() with invalid label will error", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path0 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  git_repo_path1 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  result <- parse_gitlog(perceval_path, git_repo_path0)
  result1 <- parse_gitlog(perceval_path, git_repo_path1)
  git_logs <- list(result, result1)
  expect_error(result <- identity_match(
    project_log = git_logs,
    name_column = c("author_name_email", "author_name_email"),
    assign_identity_function = assign_identity_function,
    use_name_only = FALSE,
    label = "invalid_label"
  ))
  suppressWarnings(git_delete_sample_log(git_repo_path0))
  suppressWarnings(git_delete_sample_log(git_repo_path1))
})

test_that("Calling identity_match returns correct result", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path0 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  git_repo_path1 <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  result <- parse_gitlog(perceval_path, git_repo_path0)
  result1 <- parse_gitlog(perceval_path, git_repo_path1)

  git_logs <- list(result, result1)

  result <- identity_match(
    project_log = git_logs,
    name_column = c("author_name_email", "author_name_email"),
    assign_identity_function = assign_exact_identity,
    use_name_only = FALSE,
    label = "identity_id"
  )

  expect_type(result, "list")
  expect_length(result, 2)

  suppressWarnings(git_delete_sample_log(git_repo_path0))
  suppressWarnings(git_delete_sample_log(git_repo_path1))

})

test_that("Calling identity_match with example_notebook_alternating_function_in_files returns same IDs for same authors and different IDS for different ones", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path0 <- example_notebook_alternating_function_in_files(folder_path = "/tmp",
                                              folder_name = "example0")
  git_repo_path1 <- example_notebook_alternating_function_in_files(folder_path = "/tmp",
                                               folder_name = "example1")

  result <- parse_gitlog(perceval_path, git_repo_path0)
  result1 <- parse_gitlog(perceval_path, git_repo_path1)
  git_logs <- list(result, result1)

  result <- identity_match(
    project_log = git_logs,
    name_column = c("author_name_email", "author_name_email"),
    assign_identity_function = assign_exact_identity,
    use_name_only = FALSE,
    label = "identity_id"
  )

  id1 <- result[[1]]$identity_id[1]
  id2 <- result[[2]]$identity_id[1]
  expect_equal(id1, id2)

  id3 <- result[[1]]$identity_id[3]
  id4 <- result[[2]]$identity_id[3]
  expect_equal(id1, id2)

  expect_false(isTRUE(all.equal(id1, id3)))

  io_delete_folder(folder_path="/tmp", "example0")
  io_delete_folder(folder_path="/tmp", "example1")
})

test_that("Calling identity_match with one repo correctly errors", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  result <- parse_gitlog(perceval_path, git_repo_path)

  expect_error(result <- identity_match(
    project_log = git_logs,
    name_column = c("author_name_email"),
    assign_identity_function = assign_exact_identity,
    use_name_only = FALSE,
    label = "identity_id"
  ))

  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("Calling identity_match with different name_column parameters works correctly", {

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  git_repo_path0 <- suppressWarnings(git_create_sample_log(tmp_folderpath))
  git_repo_path1 <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  result <- parse_gitlog(perceval_path, git_repo_path0)
  result1 <- parse_gitlog(perceval_path, git_repo_path1)

  git_logs <- list(result, result1)

  result <- identity_match(
    project_log = git_logs,
    name_column = c("author_name_email", "commit_message"),
    assign_identity_function = assign_exact_identity,
    use_name_only = FALSE,
    label = "identity_id"
  )

  id1 <- result[[1]]$identity_id[1]
  id2 <- result[[2]]$identity_id[1]
  expect_false(id1 == id2)

  suppressWarnings(git_delete_sample_log(git_repo_path0))
  suppressWarnings(git_delete_sample_log(git_repo_path1))

})

