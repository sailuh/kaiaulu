% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git.R
\name{git_mv}
\alias{git_mv}
\title{Git Mv}
\usage{
git_mv(git_repo, folder_path, old_name, new_name)
}
\arguments{
\item{git_repo}{The git repo path}

\item{folder_path}{The worktree path}

\item{old_name}{The name of the file/folder that you are going to change or move}

\item{new_name}{The new name of the file/folder}
}
\description{
Performs a `git mv`, which serves to
rename a folder or file. Specifically,
The `git mv` command combines `mv` and `git add`
command for the new folder/file name and the old
folder/file name in one command.
}
