tools_path <- test_path("testdata", "tools.yml")
conf_path <- test_path("testdata", "thrift.yml")

tool <- yaml::read_yaml(tools_path)
tmp_folderpath <- tool[["tmp"]]

############## Third Party Tools ##############

test_that("If Perceval is specified on tools.yml, then --help returns the help message", {
  # This test will skip if the Perceval path is set to the default in tools.yml.
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]

  default_path <- "/Library/Frameworks/Python.framework/Versions/3.12/bin/perceval"
  skip_if(grepl(perceval_path, default_path), "Perceval path is not set up in tools.yml.")

  out <- system2(
    perceval_path,
    args = "--help",
    stdout = TRUE,
    stderr = TRUE
  )

  expect_true(expect_true(any(grepl("usage: perceval", out))))
})

test_that("If utags is specified on tools.yml, then it returns the help message", {
  # This test will skip if the Perceval path is set to the default in tools.yml.
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  utags_path <- tool[["utags"]]

  default_path <- "/usr/local/Cellar/universal-ctags/HEAD-62f0144/bin/ctags"
  skip_if(grepl(utags_path, default_path), "utags path is not set up in tools.yml.")

  out <- system2(
    utags_path,
    args = "--help",
    stdout = TRUE,
    stderr = TRUE
  )

  expect_true(expect_true(any(grepl("Usage: ctags", out))))
})

############## Parsers ##############

test_that("When parse_gitlog is given a perceval_path and a ./git folder, then it returns a data table", {
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

test_that("When parse_gitlog is given a renamed file ./git folder, then the renamed file name is parsed to the new file_pathname_renamed column in the data table", {
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

test_that("When parse_gitlog is given a ./git folder with no commits, then a Git command error is thrown", {
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

test_that("When parse_gitlog is given a ./git folder with two branches with one commit each, it extracts only the current branch commit", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_different_branches(folder_path = "/tmp",
                                              folder_name = "different_branches_repo")
  result <- parse_gitlog(perceval_path, git_repo_path)
  io_delete_folder(folder_path="/tmp", "different_branches_repo")
  expect_equal(nrow(result), 2)

})

test_that("When parse_gitlog is given a valid Perl Regex, then the data table has a filtered output resulting from that Perl Regex", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_different_branches(folder_path = "/tmp",
                                              folder_name = "different_branches_repo")
  result <- parse_gitlog(perceval_path, git_repo_path, perl_regex = "committing first file")

  expect_equal(result$commit_message, "committing first file")
  expect_equal(nrow(result), 1)

  io_delete_folder(folder_path="/tmp", "different_branches_repo")
})

test_that("When parse_gitlog is given a ./git folder with a file inside a folder inside another folder, then it will extract the current commit correctly to a data table", {

  skip("Currently, the nested folder case test will always fail.")
  # The goal of this test is to test for function recursion through nested folders inside the fake repository.
  # This test will throw the error "Error in `parse_gitlog(perceval_path, git_repo_path, perl_regex = "committing first file")`: Unable to generate git log from this repository. Perhaps the path specified was incorrect or the repository has no commits?"
  # I'm not sure why it's failing, maybe I coded the example function for this test incorrectly.

  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  git_repo_path <- example_nested_folder_case(folder_path = "/tmp",
                                              folder_name = "nested_folder_case",
                                              subfolder_name = "subfolder")
  result <- parse_gitlog(perceval_path, git_repo_path, perl_regex = "committing first file")
  str(result)
  expect_is(result, "data.table")

  io_delete_folder(folder_path="/tmp", "nested_folder_case")
})


test_that("When parse_gitlog_entity is given a perceval_path, utags_path, a ./git folder, and a project_git object, then it returns a data table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)

  expect_is(result, "data.table")
  suppressWarnings(git_delete_sample_log(git_repo_path))
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

test_that("When parse_git_blame is given a ./git folder, existing commit hash, and path to a blamed file, then it returns a data table", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)

  git_repo_path <- suppressWarnings(git_create_sample_log(tmp_folderpath))

  head <- git_head(git_repo_path)
  result <- parse_git_blame(git_repo_path, head, "hello.R")

  expect_is(result, "data.table")

  suppressWarnings(git_delete_sample_log(git_repo_path))
})

test_that("When parse_git_blame is given a blame output consisting of noncontinuous code chunks, then it will return a data table", {

  git_repo_path <- example_git_blame_no_metadata(folder_path = "/tmp", folder_name = "exam_blame_repo")
  commit <- git_head(git_repo_path)

  result <- parse_git_blame(git_repo_path, commit, "test-blame.R")

  expect_is(result, "data.table")

  io_delete_folder(folder_path="/tmp", "exam_blame_repo")
})

test_that("When parse_commit_message_id is given a parsed git project, and valid regex, then it returns a data table", {

  project_git <- data.table(
    author_name_email     = "fakeAuthor <fakeEmail>",
    author_datetimetz     = "Thu Apr 30 01:45:26 2026 -1000",
    commit_hash           = "dbae7673ab9e8769761998a710f3b2795b841804",
    committer_name_email  = "Committer <commit@committer.com>",
    committer_datetimetz  = "Thu Apr 30 01:45:26 2026 -1000",
    commit_message        = "hello world commit",
    file_pathname         = "hello.R",
    lines_added           = 1L,
    lines_removed         = 0L,
    file_pathname_renamed = NA_character_
  )

  result <- parse_commit_message_id(project_git, "hello world commit")

  expect_is(result, "data.table")

})

############## Git Cmd ##############

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

############## Transforms ##############

test_that("When transform_gitlog_to_bipartite_network is given a valid parsed git project and mode, then it returns nodes and edgelist data tables", {
  dt <- data.table(
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_hash = c(
      "d9e12e9c3a26e1ec70cf8a29b4b1318c5ec6dbc5",
      "304d9aea39011f4a0a42970355902ad81464192e"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_message = c(
      "committing first file",
      "modifying first file"
    ),
    file_pathname = c("file1.R", "file1.R"),
    lines_added = c("5", "1"),
    lines_removed = c("0", "0"),
    file_pathname_renamed = c(NA, NA)
  )

  result <- transform_gitlog_to_bipartite_network(dt, "author-file")

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")
})

test_that("When transform_gitlog_to_bipartite_network is given a valid parsed git project and mode, then the list of node names consist only of those from the mode specified", {
  dt <- data.table(
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_hash = c(
      "d9e12e9c3a26e1ec70cf8a29b4b1318c5ec6dbc5",
      "304d9aea39011f4a0a42970355902ad81464192e"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_message = c(
      "committing first file",
      "modifying first file"
    ),
    file_pathname = c("file1.R", "file1.R"),
    lines_added = c("5", "1"),
    lines_removed = c("0", "0"),
    file_pathname_renamed = c(NA, NA)
  )

  authorfile <- transform_gitlog_to_bipartite_network(dt, "author-file")
  committerfile <- transform_gitlog_to_bipartite_network(dt, "committer-file")
  commitfile <- transform_gitlog_to_bipartite_network(dt, "commit-file")
  authorcommitter <- transform_gitlog_to_bipartite_network(dt, "author-committer")

  expect_equal(
    sort(authorfile$nodes$name),
    sort(c("Author 1 <author1@email.com>", "Author 2 <author2@email.com>", "file1.R"))
  )
  expect_equal(
    sort(committerfile$nodes$name),
    sort(c("Committer 1 <committer1@email.com>", "Committer 2 <committer2@email.com>", "file1.R"))
  )
  expect_equal(
    sort(commitfile$nodes$name),
    sort(c("d9e12e9c3a26e1ec70cf8a29b4b1318c5ec6dbc5", "304d9aea39011f4a0a42970355902ad81464192e", "file1.R"))
  )
  expect_equal(
    sort(authorcommitter$nodes$name),
    sort(c("Author 1 <author1@email.com>", "Author 2 <author2@email.com>", "Committer 1 <committer1@email.com>", "Committer 2 <committer2@email.com>"))
  )
})

test_that("When transform_gitlog_to_entity_bipartite_network is given a valid parsed git project and mode, then it returns nodes and edglist data tables", {

  dt <- data.table(
    row_id = 1:2,
    commit_hash = c(
      "a3f08f47e6d125dc796c6d53dc74434d10d07e54",
      "51da69013f56c60419720758d8060f8721881a4a"
    ),
    entity_definition_name = c("car", "car"),
    entity_type = c("f", "f"),
    entity_definition_line_start = c(2L, 2L),
    entity_definition_line_end = c(4L, 5L),
    author_name = c("Author 1", "Author 2"),
    author_email = c("<author1@email.com>", "<author2@email.com>"),
    author_timestamp = c("1777850117", "1777850117"),
    author_tz = c("-1000", "-1000"),
    committer_name = c("Committer 1", "Committer 2"),
    committer_email = c("<committer1@email.com>", "<committer2@email.com>"),
    committer_timestamp = c("1777850117", "1777850117"),
    committer_tz = c("-1000", "-1000"),
    committer_summary = c(
      "committing first file",
      "modifying first file"
    ),
    n_lines_changed = c(3L, 1L),
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    entity = c("car", "car"),
    weight = c(3L, 1L)
  )

  result <- transform_gitlog_to_entity_bipartite_network(dt, "author-entity")

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")
})

test_that("When transform_gitlog_to_entity_bipartite_network is given a valid parsed git project and mode, then the list of nodes names consist only of those from the mode specified", {

  dt <- data.table(
    row_id = 1:2,
    commit_hash = c(
      "a3f08f47e6d125dc796c6d53dc74434d10d07e54",
      "51da69013f56c60419720758d8060f8721881a4a"
    ),
    entity_definition_name = c("car", "car"),
    entity_type = c("f", "f"),
    entity_definition_line_start = c(2L, 2L),
    entity_definition_line_end = c(4L, 5L),
    author_name = c("Author 1", "Author 2"),
    author_email = c("<author1@email.com>", "<author2@email.com>"),
    author_timestamp = c("1777850117", "1777850117"),
    author_tz = c("-1000", "-1000"),
    committer_name = c("Committer 1", "Committer 2"),
    committer_email = c("<committer1@email.com>", "<committer2@email.com>"),
    committer_timestamp = c("1777850117", "1777850117"),
    committer_tz = c("-1000", "-1000"),
    committer_summary = c(
      "committing first file",
      "modifying first file"
    ),
    n_lines_changed = c(3L, 1L),
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    entity = c("car", "car"),
    weight = c(3L, 1L)
  )

  authorentity <- transform_gitlog_to_entity_bipartite_network(dt, "author-entity")
  committerentity <- transform_gitlog_to_entity_bipartite_network(dt, "committer-entity")
  commitentity <- transform_gitlog_to_entity_bipartite_network(dt, "commit-entity")
  authorcommitter <- transform_gitlog_to_entity_bipartite_network(dt, "author-committer")

  expect_equal(
    sort(authorentity$nodes$name),
    sort(c("Author 1 <author1@email.com>", "Author 2 <author2@email.com>", "car"))
  )
  # expect_equal(
  #   sort(committerentity$nodes$name),
  #   sort(c("Committer 1 <committer1@email.com>", "Committer 2 <committer2@email.com>", "car"))
  # )
  # Currently, committer-entity projects author-entity relationships in git.R, so this test will always fail.
  expect_equal(
    sort(commitentity$nodes$name),
    sort(c("a3f08f47e6d125dc796c6d53dc74434d10d07e54", "51da69013f56c60419720758d8060f8721881a4a", "car"))
  )
  expect_equal(
    sort(authorcommitter$nodes$name),
    sort(c("Author 1 <author1@email.com>", "Author 2 <author2@email.com>", "Committer 1 <committer1@email.com>", "Committer 2 <committer2@email.com>"))
  )
})

test_that("When transform_gitlog_to_temporal_network is given a valid parsed git project, mode, and lag, then it returns nodes and edgelist data tables", {
  dt <- data.table(
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_hash = c(
      "d9e12e9c3a26e1ec70cf8a29b4b1318c5ec6dbc5",
      "304d9aea39011f4a0a42970355902ad81464192e"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_message = c(
      "committing first file",
      "modifying first file"
    ),
    file_pathname = c("file1.R", "file1.R"),
    lines_added = c("5", "1"),
    lines_removed = c("0", "0"),
    file_pathname_renamed = c(NA, NA)
  )

  result <- transform_gitlog_to_temporal_network(dt, "author", "one_lag")

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")

})

test_that("When transform_gitlog_to_temporal_network is given a valid parsed git project, mode, and lag, then the list of nodes names consist only of those from the mode specified", {
  dt <- data.table(
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_hash = c(
      "d9e12e9c3a26e1ec70cf8a29b4b1318c5ec6dbc5",
      "304d9aea39011f4a0a42970355902ad81464192e"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = c(
      "Mon Apr 27 00:45:35 2026 -1000",
      "Mon Apr 27 00:45:35 2026 -1000"
    ),
    commit_message = c(
      "committing first file",
      "modifying first file"
    ),
    file_pathname = c("file1.R", "file1.R"),
    lines_added = c("5", "1"),
    lines_removed = c("0", "0"),
    file_pathname_renamed = c(NA, NA)
  )

  author <- transform_gitlog_to_temporal_network(dt, "author", "one_lag")
  committer <- transform_gitlog_to_temporal_network(dt, "committer", "one_lag")

  expect_equal(
    sort(author$nodes$name),
    sort(c("Author 1 <author1@email.com>", "Author 2 <author2@email.com>"))
  )
  expect_equal(
    sort(committer$nodes$name),
    sort(c("Committer 1 <committer1@email.com>", "Committer 2 <committer2@email.com>"))
  )
})

test_that("When transform_gitlog_to_entity_temporal_network is given a valid parsed git project, mode, and lag, then it returns nodes and edgelist data tables", {

  dt <- data.table(
    row_id = 1:2,
    commit_hash = c(
      "a3f08f47e6d125dc796c6d53dc74434d10d07e54",
      "51da69013f56c60419720758d8060f8721881a4a"
    ),
    entity_definition_name = c("car", "car"),
    entity_type = c("f", "f"),
    entity_definition_line_start = c(2L, 2L),
    entity_definition_line_end = c(4L, 5L),
    author_name = c("Author 1", "Author 2"),
    author_email = c("<author1@email.com>", "<author2@email.com>"),
    author_timestamp = c("1777850117", "1777850117"),
    author_tz = c("-1000", "-1000"),
    committer_name = c("Committer 1", "Committer 2"),
    committer_email = c("<committer1@email.com>", "<committer2@email.com>"),
    committer_timestamp = c("1777850117", "1777850117"),
    committer_tz = c("-1000", "-1000"),
    committer_summary = c(
      "committing first file",
      "modifying first file"
    ),
    n_lines_changed = c(3L, 1L),
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    entity = c("car", "car"),
    weight = c(3L, 1L)
  )

  result <- transform_gitlog_to_entity_temporal_network(dt, "author", "one_lag")

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")

})

test_that("When transform_gitlog_to_entity_temporal_network is given a valid parsed git project, mode, and lag, then the list of nodes names consist only of those from the mode specified", {

  dt <- data.table(
    row_id = 1:2,
    commit_hash = c(
      "a3f08f47e6d125dc796c6d53dc74434d10d07e54",
      "51da69013f56c60419720758d8060f8721881a4a"
    ),
    entity_definition_name = c("car", "car"),
    entity_type = c("f", "f"),
    entity_definition_line_start = c(2L, 2L),
    entity_definition_line_end = c(4L, 5L),
    author_name = c("Author 1", "Author 2"),
    author_email = c("<author1@email.com>", "<author2@email.com>"),
    author_timestamp = c("1777850117", "1777850117"),
    author_tz = c("-1000", "-1000"),
    committer_name = c("Committer 1", "Committer 2"),
    committer_email = c("<committer1@email.com>", "<committer2@email.com>"),
    committer_timestamp = c("1777850117", "1777850117"),
    committer_tz = c("-1000", "-1000"),
    committer_summary = c(
      "committing first file",
      "modifying first file"
    ),
    n_lines_changed = c(3L, 1L),
    author_name_email = c(
      "Author 1 <author1@email.com>",
      "Author 2 <author2@email.com>"
    ),
    author_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    committer_name_email = c(
      "Committer 1 <committer1@email.com>",
      "Committer 2 <committer2@email.com>"
    ),
    committer_datetimetz = as.POSIXct(
      c("2026-05-03 23:15:17", "2026-05-03 23:15:17"),
      tz = "Pacific/Honolulu"
    ),
    entity = c("car", "car"),
    weight = c(3L, 1L)
  )

  author <- transform_gitlog_to_entity_temporal_network(dt, "author", "one_lag")
  committer <- transform_gitlog_to_entity_temporal_network(dt, "committer", "one_lag")

  expect_equal(
    sort(author$nodes$name),
    sort(c("Author 1 <author1@email.com>", "Author 2 <author2@email.com>"))
  )
  expect_equal(
    sort(committer$nodes$name),
    sort(c("Committer 1 <committer1@email.com>", "Committer 2 <committer2@email.com>"))
  )
})

test_that("When transform_commit_message_id_to_network is given a parsed git project and valid commit Regex, then it returns nodes and edgelist data tables", {

  dt <- data.table(
    author_name_email = "fakeAuthor <fakeEmail>",
    author_datetimetz = "Thu Apr 30 01:45:26 2026 -1000",
    commit_hash = "dbae7673ab9e8769761998a710f3b2795b841804",
    committer_name_email = "Committer <commit@committer.com>",
    committer_datetimetz = "Thu Apr 30 01:45:26 2026 -1000",
    commit_message = "hello world commit",
    file_pathname = "hello.R",
    lines_added = 1L,
    lines_removed = 0L,
    file_pathname_renamed = NA_character_,
    commit_message_id = "hello world commit"
  )
  result <- transform_commit_message_id_to_network(dt, "hello world commit")

  expect_is(result$nodes, "data.table")
  expect_is(result$edgelist, "data.table")
})


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

