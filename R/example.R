# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.
#
# This file is meant to generate fake git data


#' Example Renamed File Repo
#'
#' A repo with 3 commits. The first adds hello.R, , the second
#' renames the file to hi.R. and the third adds a second file bye.R.
#'
#' This example can be used to test how parsers trace file renaming.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo of newly created empty repo
#' @export
example_renamed_file <- function(folder_path="/tmp",folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Add hello.R file and commit it
  hello_path <- file.path(folder_path, "hello.R")
  io_make_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")


  # rename the file from hello.R to hi.R, then add and commit
  git_mv(git_repo, folder_path, old_name = "hello.R", new_name = "hi.R")
  hi_path <- file.path(folder_path, "hi.R")
  git_add(git_repo, folder_path, hi_path)
  git_commit(git_repo, folder_path, "Renamed file name to hi.R", "John Doe", "JohnDoe@test.com")

  # Add bye.R file and commit it
  bye_path <- file.path(folder_path, "bye.R")
  io_make_file(bye_path, "print('bye!')")
  git_add(git_repo, folder_path, bye_path)
  git_commit(git_repo, folder_path, "Commit bye.R file to repo", "John Doe", "JohnDoe@test.com")

}


#' Example Unit Test and Examples Repository
#'
#' A repository which contains test, example and
#' source files. Can be useful to test filter functions.
#'
#' The repo contains 3 commits, where 1 file has as prefix
#'  \_test.R, 1 file has the suffix example\_*.R, and 1 file
#'  hello.R. The second renames the file to hi.R.
#'  The third adds a second file bye.R
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo of newly created empty repo
#' @export
#' @export
example_test_example_src_repo <- function(folder_path="/tmp",folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path,folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Add example_test.R file and commit it
  test_path <- file.path(folder_path, "test-hello.R")
  io_make_file(test_path, "print('tester')")
  git_add(git_repo, folder_path, test_path)
  git_commit(git_repo, folder_path, "Commit test-example.R file to repo", "John Doe", "JohnDoe@test.com")


  # Add hello.R file and commit it
  example_path <- file.path(folder_path, "example-hi.R")
  io_make_file(example_path, "print('example!')")
  git_add(git_repo, folder_path, example_path)
  git_commit(git_repo, folder_path, "Commit fake-example.R file", "John Doe", "JohnDoe@test.com")

  # Add hello.R file and commit it
  hello_path <- file.path(folder_path, "hello.R")
  io_make_file(hello_path, "print('hello!')")
  git_add(git_repo, folder_path, hello_path)
  git_commit(git_repo, folder_path, "Commit hello.R file to empty repo", "John Doe", "JohnDoe@test.com")

}

#' Example Empty Repo
#'
#' Creates an empty git repo named "empty_repo".
#'
#' Useful to test the behavior of git_log exporter and parse_gitlog
#' on repositories with no commits.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
example_empty_repo <- function(folder_path="/tmp",folder_name) {

  # Create empty folder named "empty_repo"
  folder_path <- io_make_folder(folder_path=folder_path, folder_name)
  git_init(folder_path)
  git_repo_path <- file.path(folder_path,'.git')

  return(git_repo_path)
}

#' Example Commit Different Branches
#'
#' One commit in two different with branches with 1 file each.
#'
#' Useful to check parser includes commits from different branches.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
example_different_branches <- function(folder_path="/tmp", folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path=folder_path, folder_name = folder_name)
  git_init(folder_path)

  # first branch (master)
  git_repo_path <- file.path(folder_path, '.git')
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, "print('hello world!')")
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "committing first file", "fakeAuthor", "fakeEmail@email.com")

  # second new branch
  git_checkout(commit_hash="123", git_repo_path, new_branch = TRUE)
  file_path <- file.path(folder_path, "file2.R")
  io_make_file(file_path, "print('hello world!')")
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "committing second file", "realAuthor", "realEmail@email.com")

  return(git_repo_path)
}

#' Example Different Files Commit
#'
#' Repo with 2 commits. The first commit contains 5 files modified, and
#' second commit contains only one file modified.
#'
#' Useful to test unbalanced sized commits and filters.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
example_large_sized_commits <- function(folder_path="/tmp", folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path=folder_path, folder_name = folder_name)
  git_init(folder_path)
  git_repo_path <- file.path(folder_path, '.git')

  # Making 5 new files
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, "print('hello world 1!')")
  git_add(git_repo_path, folder_path, file_path)

  file_path <- file.path(folder_path, "file2.R")
  io_make_file(file_path, "print('hello world 2!')")
  git_add(git_repo_path, folder_path, file_path)

  file_path <- file.path(folder_path, "file3.R")
  io_make_file(file_path, "print('hello world 3!')")
  git_add(git_repo_path, folder_path, file_path)

  file_path <- file.path(folder_path, "file4.R")
  io_make_file(file_path, "print('hello world 4!')")
  git_add(git_repo_path, folder_path, file_path)

  file_path <- file.path(folder_path, "file5.R")
  io_make_file(file_path, "print('hello world 5!')")
  git_add(git_repo_path, folder_path, file_path)

  git_commit(git_repo_path, folder_path, "committing 5 files", "testAuthor", "fakeEmail@email.com")

  # Making one file
  file_path <- file.path(folder_path, "file6.R")
  io_make_file(file_path, "print('hello world 6!')")
  git_add(git_repo_path, folder_path, file_path)

  git_commit(git_repo_path, folder_path, "committing one file", "testAuthor", "fakeEmail@email.com")

  return(git_repo_path)
}


#' Create sample mbox with three replies
#'
#' Creating a fake mbox with three replies. The first two fake mbox replies have the same email subject while the
#' third reply has a different subject.
#'
#' @param folder_path Default folder path set to "/tmp"
#' @param folder_name Name of folder where .mbox file will be stored
#' @return Folder path of .mbox sample file that was created
#' @export
example_mbox_three_replies <- function(folder_path = "/tmp", folder_name) {

  # Define sample data for create_fake_mbox function that can easily be altered for testing

  # Step 1: Create fake mbox replies and assign them to variables for easy editing
  reply_1 <- create_fake_mbox_replies("test-list", "John Doe", "johndoe@example.com", "Janette Doe", "janedoe@example.com",
                                      "Smithsonian Doe", "smith_doe@example.com", "2023-01-15T08:30:00", "EST",
                                      "Sample Email Subject ONE", "This is the body of the test email 1.")

  reply_2 <- create_fake_mbox_replies("test-list", "John Doe", "johndoe@example.com", "Janette Doe", "janedoe@example.com",
                                      "Smithsonian Doe", "smith_doe@example.com", "2023-01-15T09:45:00", "EST",
                                      "Sample Email Subject ONE", "This is the body of the test email 2.")

  reply_3 <- create_fake_mbox_replies("test-list", "John Doe", "johndoe@example.com", "Janette Doe", "janedoe@example.com",
                                      "Smithsonian Doe", "smith_doe@example.com", "2023-01-15T11:52:00", "EST",
                                      "Sample Email Subject TWO", "This is the body of the test email 3.")

  # Step 2: Add each reply to a list
  replies <- list(reply_1, reply_2, reply_3)

  # Create mbox file from the list of replies
  mbox_path <- create_mbox_from_replies("/tmp", "sample", replies)
  print(mbox_path)

}






