# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.
#

#' Create Empty Repo
#'
#' Checks if parse_gitlog works on an empty repo.
#'
#' @return git_repo of newly created empty repo
#' @export
example_empty_repo <- function() {

# Create empty folder named "empty_repo"
folder_path <- io_create_folder("empty_repo")
git_init(folder_path)
git_repo <- file.path(folder_path,'.git')

return(git_repo)
}

#' Commit in two different branches
#'
#' One commit in two different with branches with 1 file each
#'
#' @return git_repo of newly created empty repo
#' @export
example_different_branches <- function() {

# Create folder & repo
folder_path <- io_create_folder("different_branches_repo", folder_path="/tmp")
git_init(folder_path)

# first master branch
git_repo_path <- file.path(folder_path, '.git')
file_path <- file.path(folder_path, "file1.R")
io_make_sample_file(file_path, "print('hello world!')")
git_add(git_repo_path, folder_path, file_path)
git_commit(git_repo_path, folder_path, "committing first file", "fakeAuthor", "fakeEmail@email.com")

# second new branch
git_checkout(commit_hash="123", git_repo_path, new_branch = TRUE) # unsure of commit hash
file_path <- file.path(folder_path, "file2.R")
io_make_sample_file(file_path, "print('hello world!')")
git_add(git_repo_path, folder_path, file_path)
git_commit(git_repo_path, folder_path, "committing second file", "realAuthor", "realEmail@email.com")

return(git_repo_path)
}

#' Create repo with 2 commits, diff # of files
#'
#' Repo with 2 commits, first one contains 5 files modified, second contains only one
#'
#' @return git_repo of newly created empty repo
#' @export
example_different_files_commits <- function() {

  # Create folder & repo
  folder_path <- io_create_folder("example_diff_commits")
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Making 5 new files
  file_path <- file.path(folder_path, "file1.R")
  io_make_sample_file(file_path, "print('hello world 1!')")
  git_add(git_repo, folder_path, file_path)

  file_path <- file.path(folder_path, "file2.R")
  io_make_sample_file(file_path, "print('hello world 2!')")
  git_add(git_repo, folder_path, file_path)

  file_path <- file.path(folder_path, "file3.R")
  io_make_sample_file(file_path, "print('hello world 3!')")
  git_add(git_repo, folder_path, file_path)

  file_path <- file.path(folder_path, "file4.R")
  io_make_sample_file(file_path, "print('hello world 4!')")
  git_add(git_repo, folder_path, file_path)

  file_path <- file.path(folder_path, "file5.R")
  io_make_sample_file(file_path, "print('hello world 5!')")
  git_add(git_repo, folder_path, file_path)

  git_commit(git_repo, folder_path, "committing 5 files", "testAuthor", "fakeEmail@email.com")

  # Making one file
  file_path <- file.path(folder_path, "file6.R")
  io_make_sample_file(file_path, "print('hello world 6!')")
  git_add(git_repo, folder_path, file_path)

  git_commit(git_repo, folder_path, "committing one file", "testAuthor", "fakeEmail@email.com")

}
