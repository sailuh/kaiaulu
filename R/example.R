# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.
#
# This file is meant to generate fake git data


#'  A repo with 3 commits. The first adds hello.R.
#' The second renames the file to hi.R. The third adds a second file bye.R
#'
#' @return git_repo of newly created empty repo
#' @export
example_repo_three_commits <- function() {

# Create folder & repo
folder_path <- io_create_folder("example_repo")
git_init(folder_path)
git_repo <- file.path(folder_path, '.git')

# Add hello.R file and commit it
hello_path <- file.path(folder_path, "hello.R")
io_make_sample_file(hello_path, "print('hello!')")
git_add(git_repo, folder_path, hello_path)
git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")


# rename the file from hello.R to hi.R, then add and commit
git_rename("hello.R", "hi.R")
hi_path <- file.path(folder_path, "hi.R")
git_add(git_repo, folder_path, hi_path)
git_commit(git_repo, folder_path, "Renamed file name to hi.R", "John Doe", "JohnDoe@test.com")

# Add bye.R file and commit it
bye_path <- file.path(folder_path, "bye.R")
io_make_sample_file(bye_path, "print('bye!')")
git_add(git_repo, folder_path, bye_path)
git_commit(git_repo, folder_path, "Commit bye.R file to repo", "John Doe", "JohnDoe@test.com")

}


# A repo with 3 commits, where 1 file has as prefix _test.R, 1 file has the suffix example_*.R,
# and 1 file hello.R.
example_repo_suffix_prefix <- function() {

  # Create folder & repo
  folder_path <- io_create_folder("example_repo")
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Add example_test.R file and commit it
  test_path <- file.path(folder_path, "test-example.R")
  io_make_sample_file(test_path, "print('tester')")
  git_add(git_repo, folder_path, test_path)
  git_commit(git_repo, folder_path, "Commit test-example.R file to repo", "John Doe", "JohnDoe@test.com")


  # Add hello.R file and commit it
  example_path <- file.path(folder_path, "fake-example.R")
  io_make_sample_file(example_path, "print('example!')")
  git_add(git_repo, folder_path, example_path)
  git_commit(git_repo, folder_path, "Commit fake-example.R file", "John Doe", "JohnDoe@test.com")

  # Add hello.R file and commit it
  hello_path <- file.path(folder_path, "hello.R")
  io_make_sample_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")

}



