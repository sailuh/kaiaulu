# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.
#
# This file is meant to generate fake git data

#' Example GitHub Issue Without Description
#'
#' This function creates a synthetic GitHub issue JSON with a missing description (`body = NULL`).
#' The JSON is written to a temporary folder for testing how parsers handle GitHub issues
#' with null description fields. 
#'
#' @param folder_path The path where the folder will be created 
#' @param folder_name The name of the folder
#' @return The JSON folder path of the newly created GitHub issue
#' @export
#' @keywords internal
example_github_issue_no_description <- function(folder_path="/tmp", folder_name) {

  # Create folder
  folder_path <- io_make_folder(folder_path=folder_path, folder_name = folder_name)

  # Synthetic GitHub issue with missing description
  issue <- list(
    id = 1001,
    number = 42,
    html_url = "https://github.com/example/repo/issues/42",
    url = "https://api.github.com/repos/example/repo/issues/42",
    created_at = "2025-11-01T00:00:00Z",
    updated_at = "2025-11-02T00:00:00Z",
    state = "open",
    user = list(login = "test-user"),
    author_association = "random-name",
    title = "Issue #X",
    body = NULL,  
    labels = list(
      list(name = "bug"),
      list(name = "help wanted")
    )
  )

  jsonlite::write_json(list(issue),
                       file.path(folder_path, "ONE_ISSUE_NO_DESCRIPTION.json"))

  return(folder_path)
}

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
#' @keywords internal
example_renamed_file <- function(folder_path,folder_name) {

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
#' @keywords internal
example_test_example_src_repo <- function(folder_path,folder_name) {

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
#' @keywords internal
example_empty_repo <- function(folder_path,folder_name) {

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
#' @keywords internal
example_different_branches <- function(folder_path, folder_name) {

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
#' @keywords internal
example_large_sized_commits <- function(folder_path, folder_name) {

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

#' Example Commit of R Notebooks
#'
#' One commit that defines the function by Dev 1, then
#' one commit that modifies the same function by Dev 2.
#'
#' Useful to check how git log entity behaves with files it does
#' not recognize.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_notebook_function_in_code_blocks <- function(folder_path, folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path=folder_path, folder_name = folder_name)
  git_init(folder_path)

  #initial file
  body1 <- "
  ```{r}
  car <- function(x){
  return(x)
  }
  ```
  "

  #changed file
  body2 <- "
  ```{r}
  car <- function(x){
  print('hi!')
  return(x)
  }
  ```
  "

  # first commit
  git_repo_path <- file.path(folder_path, '.git')
  file_path <- file.path(folder_path, "file1.Rmd")
  io_make_file(file_path, body1)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "committing first file", "Author 1", "author1@email.com")

  # second commit
  file_path <- file.path(folder_path, "file1.Rmd")
  io_make_file(file_path, body2)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "modifying first file", "Author 2", "author2@email.com")

  return(git_repo_path)
}

#' Example Commit of R Function Declarations
#'
#' One commit that defines the function by Dev 1, then
#' one commit that modifies the same function by Dev 2.
#'
#' Useful to check how git log entity behaves with files it should
#' recognize.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_function_in_files <- function(folder_path, folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path=folder_path, folder_name = folder_name)
  git_init(folder_path)

  #initial file
  body1 <- "
  car <- function(x){
  return(x)
  }
  "

  #changed file
  body2 <- "
  car <- function(x){
  print('hi!')
  return(x)
  }
  "

  # first commit
  git_repo_path <- file.path(folder_path, '.git')
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, body1)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "committing first file", "Author 1", "author1@email.com")

  # second commit
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, body2)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "modifying first file", "Author 2", "author2@email.com")

  return(git_repo_path)
}

#' Example Alternating Undecided Developers
#'
#' Developers keep alternating the lines changes.
#'
#' Useful to check how git log entity behaves with files it should
#' recognize.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_notebook_alternating_function_in_files <- function(folder_path, folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path=folder_path, folder_name = folder_name)
  git_init(folder_path)

  #initial file

  body1 <- "
  car <- function(x){return(x)}
  "

  body2 <- "
  car <- function(x){
  return(x)
  }
  "

  #changed file
  body3 <- "
  car <- function(x){
  print('hi!')
  print('one more line!')
  print('one more line again!')
  print('one more line again 2!')
  print('one more line again 3!')
  return(x)
  }
  "

  body4 <- "
  car <- function(x){
  print('hi!')
  print('one more line!')
  print('one more line again!')
  print('5th line!')
  print('6th line!')
  print('7th line!')
  print('8th line!')
  print('9th line!')
  print('10th line!')
  print('11th line!')
  return(x)
  }
  "

  # first commit
  git_repo_path <- file.path(folder_path, '.git')
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, body1)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "committing 1", "dev 1", "")

  # second commit
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, body2)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "commit 2", "dev 2", "")

  # third commit
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, body3)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "commit 3", "dev 1", "")

  # forth commit
  file_path <- file.path(folder_path, "file1.R")
  io_make_file(file_path, body4)
  git_add(git_repo_path, folder_path, file_path)
  git_commit(git_repo_path, folder_path, "commit 4", "dev 2", "")

  return(git_repo_path)
}
#' Example Src Java Code Dependencies
#'
#' This function creates a repo that has three java files. These are Helper.java, Utils.java, and Main.java.
#' This could be useful for testing that the parsers in src.R can correctly detect file dependencies.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_java_code_dependencies <- function(folder_path, folder_name) {

  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Helper.java
  helper_path <- file.path(folder_path, "Helper.java")
  io_make_file(helper_path,
               "public class Helper {
                  public static void help() {
                    System.out.println(\"Helping...\");
                    process();
                  }

                  public static void process() {
                    System.out.println(\"Processing...\");
                  }
                }"
  )
  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper class", "John Doe", "JohnDoe@test.com")

  # Utils.java adds cross-file dependencies
  utils_path <- file.path(folder_path, "Utils.java")
  io_make_file(utils_path,
               "public class Utils {
                  public static void doWork() {
                    Helper.process();
                  }
                }"
  )
  git_add(git_repo, folder_path, utils_path)
  git_commit(git_repo, folder_path, "Add Utils class calling Helper", "John Doe", "JohnDoe@test.com")

  # Main.java depends on both Helper and Utils
  main_path <- file.path(folder_path, "Main.java")
  io_make_file(main_path,
               "public class Main {
                  public static void main(String[] args) {
                    Helper.help();
                    Utils.doWork();
                  }
                }"
  )
  git_add(git_repo, folder_path, main_path)
  git_commit(git_repo, folder_path, "Add Main using Helper and Utils", "John Doe", "JohnDoe@test.com")
}
#' Example Src Nested Java Code Dependencies
#'
#' This function creates a repo that has a java file at the /.git folder level, as well as a java file in a folder
#' that is at the ./git folder level, which calls the former java file. This could be useful for testing that the
#' src parsers can recursively search for files through folders.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_java_nested_code_dependencies <- function(folder_path, folder_name) {

  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Create nested folder
  nested_folder <- file.path(folder_path, "nested_folder")
  dir.create(nested_folder, recursive = TRUE)

  # Helper.java at repo level
  helper_path <- file.path(folder_path, "Helper.java")
  io_make_file(
    helper_path,
    "public class Helper {
       public static void help() {
         System.out.println(\"Helping...\");
       }
     }"
  )

  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper file and class", "John Doe", "JohnDoe@test.com")

  # Main.java inside nested folder
  main_path <- file.path(nested_folder, "Main.java")
  io_make_file(
    main_path,
    "public class Main {
      public static void main(String[] args) {
        Helper.help();
      }
    }"
  )

  git_add(git_repo, folder_path, main_path)
  git_commit(git_repo, folder_path, "Add Main file", "John Doe", "JohnDoe@test.com")
}
#' Example Src Java And Python Files
#'
#' This function creates a repo that has two java files and a python file. These are Helper.java, Utils.java,
#' and Hello.py. This could be useful for testing that the parsers in src.R can filter out files.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_java_and_python_files <- function(folder_path, folder_name) {

  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Main.java at repo level
  main_path <- file.path(folder_path, "Main.java")
  io_make_file(
    main_path,
    "public class Main {
      public static void main(String[] args) {
        Helper.help();
      }
    }"
  )

  git_add(git_repo, folder_path, main_path)
  git_commit(git_repo, folder_path, "Add Main file", "John Doe", "JohnDoe@test.com")

  # Helper.java at repo level
  helper_path <- file.path(folder_path, "Helper.java")
  io_make_file(
    helper_path,
    "public class Helper {
       public static void help() {
         System.out.println(\"Helping...\");
       }
     }"
  )

  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper file and class", "John Doe", "JohnDoe@test.com")

  # Hello.py at repo level
  py_path <- file.path(folder_path, "Hello.py")
  io_make_file(
    py_path,
    "class myClass:
      x = 5"
  )

  git_add(git_repo, folder_path, py_path)
  git_commit(git_repo, folder_path, "Add Python file", "John Doe", "JohnDoe@test.com")
}
#' Example Src Java Circular Dependencies
#'
#' This function creates a repo that has two java files which call each other. These are Helper.java and Utils.java.
#' This could be useful for testing that the parsers in src.R can handle circular file dependencies.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_java_circular_dependencies <- function(folder_path, folder_name) {

  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  # Main.java at repo level
  utils_path <- file.path(folder_path, "Utils.java")
  io_make_file(utils_path,
               "public class Utils {
                  public static void doWork() {
                    System.out.println(\"Processing...\");
                  }
                Helper.help();
                }"
  )

  git_add(git_repo, folder_path, utils_path)
  git_commit(git_repo, folder_path, "Add Utils file", "John Doe", "JohnDoe@test.com")

  # Helper.java
  helper_path <- file.path(folder_path, "Helper.java")
  io_make_file(
    helper_path,
    "public class Helper {
       public static void help() {
         System.out.println(\"Helping...\");
       }
    Utils.doWork();
     }"
  )

  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper file", "John Doe", "JohnDoe@test.com")

}
#' Example Src Xml From Java Code
#'
#' This function creates a repo that has one .XML file. This contains information from example_src_java_code_dependencies,
#' after it has been ran by both build_understand_project and export_understand_dependencies. This is used to test for
#' parse_understand_dependencies, and would make future changes to the .XML format from Scitools more obvious.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_xml_from_java_code <- function(folder_path, folder_name) {
  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  java_path <- file.path(folder_path, "example.xml")
  io_make_file(java_path,
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <graph xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:cy=\"http://www.cytoscape.org\" xmlns=\"http://www.cs.rpi.edu/XGMML\" label=\" File Dependencies\" Directed=\"0\" Graphic=\"0\" Layout=\"Circular\">
       <att name=\"documentVersion\" value=\"1.1\"/>
       <att name=\"networkMetadata\">
               <rdf:RDF>
                       <rdf:Description rdf:about=\"http://www.cytoscape.org/\">
                               <dc:type>File Dependencies</dc:type>
                               <dc:description/>
                               <dc:identifier>-</dc:identifier>
                               <dc:date/>
                               <dc:title/>
                               <dc:source>http://localhost/?p=</dc:source>
                               <dc:format>Cytoscape-XGMML</dc:format>
                       </rdf:Description>
               </rdf:RDF>
      </att>
      <node id=\"1\" label=\"Helper.java id:1\">
           <att type=\"string\" name=\"node.shape\" value=\"rect\"/>
           <att type=\"string\" name=\"node.fontSize\" value=\"5\"/>
           <att type=\"string\" name=\"node.label\" value=\"Helper.java\"/>
           <att type=\"string\" name=\"longName\" value=\"/tmp/test_example_java_code/Helper.java\"/>
           <graphics type=\"RECTANGLE\" h=\"35\" w=\"35\" x=\"0\" y=\"0\" fill=\"#ffffff\" width=\"1\" outline=\"#000000\" cy:nodeTransparency=\"1.0\" cy:nodeLabelFont=\"Default-0-8\" cy:borderLineType=\"solid\"/>
      </node>
      <node id=\"15\" label=\"Main.java id:15\">
           <att type=\"string\" name=\"node.shape\" value=\"rect\"/>
           <att type=\"string\" name=\"node.fontSize\" value=\"5\"/>
           <att type=\"string\" name=\"node.label\" value=\"Main.java\"/>
           <att type=\"string\" name=\"longName\" value=\"/tmp/test_example_java_code/Main.java\"/>
           <graphics type=\"RECTANGLE\" h=\"35\" w=\"35\" x=\"0\" y=\"115\" fill=\"#ffffff\" width=\"1\" outline=\"#000000\" cy:nodeTransparency=\"1.0\" cy:nodeLabelFont=\"Default-0-8\" cy:borderLineType=\"solid\"/>
      </node>
      <node id=\"21\" label=\"Utils.java id:21\">
           <att type=\"string\" name=\"node.shape\" value=\"rect\"/>
           <att type=\"string\" name=\"node.fontSize\" value=\"5\"/>
           <att type=\"string\" name=\"node.label\" value=\"Utils.java\"/>
           <att type=\"string\" name=\"longName\" value=\"/tmp/test_example_java_code/Utils.java\"/>
           <graphics type=\"RECTANGLE\" h=\"35\" w=\"35\" x=\"0\" y=\"230\" fill=\"#ffffff\" width=\"1\" outline=\"#000000\" cy:nodeTransparency=\"1.0\" cy:nodeLabelFont=\"Default-0-8\" cy:borderLineType=\"solid\"/>
      </node>
      <edge source=\"15\" target=\"21\" label=\"Main.java(Depends On)Utils.java\">
             <att type=\"string\" name=\"edge.targetArrowShape\" value=\"ARROW\"/>
             <att type=\"string\" name=\"edge.color\" value=\"#0000FF\"/>
             <att type=\"string\" name=\"canonicalName\" value=\"Main.java(Depends On)Utils.java\"/>
             <att type=\"string\" name=\"interaction\" value=\"Depends On\"/>
             <att type=\"string\" name=\"dependency kind\" value=\"Call\"/>
      </edge>
      <edge source=\"15\" target=\"1\" label=\"Main.java(Depends On)Helper.java\">
             <att type=\"string\" name=\"edge.targetArrowShape\" value=\"ARROW\"/>
             <att type=\"string\" name=\"edge.color\" value=\"#0000FF\"/>
             <att type=\"string\" name=\"canonicalName\" value=\"Main.java(Depends On)Helper.java\"/>
             <att type=\"string\" name=\"interaction\" value=\"Depends On\"/>
             <att type=\"string\" name=\"dependency kind\" value=\"Call\"/>
      </edge>
      <edge source=\"21\" target=\"1\" label=\"Utils.java(Depends On)Helper.java\">
             <att type=\"string\" name=\"edge.targetArrowShape\" value=\"ARROW\"/>
             <att type=\"string\" name=\"edge.color\" value=\"#0000FF\"/>
             <att type=\"string\" name=\"canonicalName\" value=\"Utils.java(Depends On)Helper.java\"/>
             <att type=\"string\" name=\"interaction\" value=\"Depends On\"/>
             <att type=\"string\" name=\"dependency kind\" value=\"Call\"/>
      </edge>
      </graph>"
  )
  git_add(git_repo, folder_path, java_path)
  git_commit(git_repo, folder_path, "Added XML example file", "John Doe", "JohnDoe@test.com")
}
#' Example R Code Dependencies
#'
#' This function creates a repo that has three R files. These are Helper.R, Utils.R, and Main.R.
#' This could be useful for testing that the parse_r_dependencies parser in src.R can correctly
#' detect file dependencies.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_r_code_dependencies <- function(folder_path, folder_name) {
  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  helper_path <- file.path(folder_path, "Helper.R")
  io_make_file(helper_path,
             "helper_process <- function() {
                print(\"processing\")
              }"
              )
  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper.R", "John Doe", "JohnDoe@test.com")

  utils_path <- file.path(folder_path, "Utils.R")
  io_make_file(utils_path,
              "source(\"Helper.R\")

               utils_do_work <- function() {
                  helper_process()
              }"
              )
  git_add(git_repo, folder_path, utils_path)
  git_commit(git_repo, folder_path, "Add Utils.R", "John Doe", "JohnDoe@test.com")

  main_path <- file.path(folder_path, "Main.R")

  io_make_file(main_path,
             "source(\"Helper.R\")
              source(\"Utils.R\")

              main <- function() {
                  helper_process()
                  utils_do_work()
              }

              main()"
              )
  git_add(git_repo, folder_path, main_path)
  git_commit(git_repo, folder_path, "Add Main.R", "John Doe", "JohnDoe@test.com")
}
#' Example Src R Nested Code Dependencies
#'
#' This function creates a repo that has an R file at the /.git folder level, as well as an R file in a folder
#' that is at the ./git folder level, which calls the former R file. This could be useful for testing that
#' r_parse_dependencies can recursively search for files through folders.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_r_nested_code_dependencies <- function(folder_path, folder_name) {
  folder_path <- io_make_folder(folder_path, folder_name)

  git_init(folder_path)

  git_repo <- file.path(folder_path, ".git")

  # Create nested folder
  nested_folder <- file.path(folder_path, "utils")
  dir.create(nested_folder, recursive = TRUE)

  # Helper.R
  helper_path <- file.path(folder_path, "Helper.R")

  io_make_file(
    helper_path,
    "helper_process <- function() {
       print(\"processing\")
     }"
  )

  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper.R", "John Doe","JohnDoe@test.com")

  # Nested Utils.R
  utils_path <- file.path(nested_folder, "Utils.R")

  io_make_file(
    utils_path,
    "source(\"Helper.R\")

     utils_do_work <- function() {
       helper_process()
     }"
    )

  git_add(git_repo, folder_path, utils_path)
  git_commit(git_repo, folder_path, "Add Utils.R depending on Helper.R", "John Doe", "JohnDoe@test.com")
}
#' Example Src R And Python Files
#'
#' This function creates a repo that has two R files and a python file. These are Helper.R, Utils.R,
#' and Hello.py. This could be useful for testing that parse_r_dependencies in src.R can filter out non-R
#' files.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_r_and_python_files <- function(folder_path, folder_name) {
  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  helper_path <- file.path(folder_path, "Helper.R")
  io_make_file(helper_path,
               "helper_process <- function() {
                  print(\"processing\")
              }"
  )

  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper.R", "John Doe", "JohnDoe@test.com")

  utils_path <- file.path(folder_path, "Utils.R")
  io_make_file(utils_path,
              "source(\"Helper.R\")

               utils_do_work <- function() {
                 helper_process()
               }"
  )

  git_add(git_repo, folder_path, utils_path)
  git_commit(git_repo, folder_path, "Add Utils.R", "John Doe", "JohnDoe@test.com")

  py_path <- file.path(folder_path, "Hello.py")
  io_make_file(py_path,
               "class myClass:
                  x = 5"
  )

  git_add(git_repo, folder_path, py_path)
  git_commit(git_repo, folder_path, "Add Hello.py", "John Doe", "JohnDoe@test.com")

}
#' Example Src R Circular Dependencies
#'
#' This function creates a repo that has two R files which import each other. These are Helper.R and Utils.R.
#' This could be useful for testing that parse_r_dependencies in src.R can handle circular R file dependencies.
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return git_repo_path of newly created empty repo
#' @export
#' @keywords internal
example_src_r_circular_dependencies <- function(folder_path, folder_name) {
  folder_path <- io_make_folder(folder_path, folder_name)
  git_init(folder_path)
  git_repo <- file.path(folder_path, '.git')

  helper_path <- file.path(folder_path, "Helper.R")
  io_make_file(helper_path,
               "source(\"Utils.R\")

               helper_process <- function() {
                  utils_do_work()
               }"
  )

  git_add(git_repo, folder_path, helper_path)
  git_commit(git_repo, folder_path, "Add Helper.R", "John Doe", "JohnDoe@test.com")

  utils_path <- file.path(folder_path, "Utils.R")
  io_make_file(utils_path,
               "source(\"Helper.R\")

               utils_do_work <- function() {
                 helper_process()
               }"
  )

  git_add(git_repo, folder_path, utils_path)
  git_commit(git_repo, folder_path, "Add Utils.R", "John Doe", "JohnDoe@test.com")
}
#' Create one No-Comment Issue with Two Components
#'
#' This example can be used to evaluate the parser does not replicate
#' new components on new issues, which would severely bias metrics
#' associated to issues such as bugs (see #244).
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return the JSON folder path of the newly created issue issue tracker
#' @export
#' @keywords internal
example_jira_issue_components <- function(folder_path, folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path = folder_path, folder_name = folder_name)

  issue1 <- make_jira_issue(
    jira_domain_url = "https://project.org/jira",
    issue_key = "PROJECT-123",
    project_key = "PROJECT",
    summary = "Summary of new feature",
    description = "The new features have been implemented",
    issue_type = "New Feature",
    resolution = "Finished",
    priority = "Minor",
    status = "Open",
    labels = c("pull-request-available"),
    components = c("jira", "mail"),
    affects_versions = c("3.4.3"),
    fix_versions = c("3.4.2"),
    assignee_name = "Moe",
    creator_name = "Bob",
    reporter_name = "Joe"
  )

  issues <- list(issue1)

  jira_json_path <- make_jira_issue_tracker(issues,
                                            save_filepath=file.path(folder_path, "ONE_ISSUE_NO_COMMENTS_issues_1121646814_1121719175.json"))

  return(folder_path)
}

#' Example JIRA Issue Tracker No Comments
#'
#' Create fake JIRA issue tracker with 2 issues, no comments
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return the JSON folder path of the newly created issue issue tracker
#' @export
#' @keywords internal
example_jira_two_issues <- function(folder_path, folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path = folder_path, folder_name = folder_name)

  issue1 <- make_jira_issue(
    jira_domain_url = "https://project.org/jira",
    issue_key = "PROJECT-11",
    project_key = "PROJECT",
    summary = "Summary of issue 1",
    description = "Description of summary 1",
    issue_type = "New Feature",
    resolution = "Finished",
    priority = "Minor",
    status = "Closed",
    labels = c("pull-request-available"),
    components = c("jira"),
    affects_versions = c("1.1.1"),
    fix_versions = c("1.1.1"),
    assignee_name = "Moe",
    creator_name = "Bob",
    reporter_name = "Joe"
  )

  issue2 <- make_jira_issue(
    jira_domain_url = "https://project.org/jira",
    issue_key = "PROJECT-22",
    project_key = "PROJECT",
    summary = "Summary of issue 2",
    description = "Description of summary 2",
    issue_type = "New Feature",
    resolution = "Finished",
    priority = "Minor",
    status = "Open",
    labels = c("pull-request-available"),
    components = c("jira"),
    affects_versions = c("2.2.2"),
    fix_versions = c("2.2.2"),
    assignee_name = "Steven",
    creator_name = "Nathan",
    reporter_name = "Matthew"
  )

  issues <- list(issue1, issue2)

  jira_json_path <- make_jira_issue_tracker(issues,
                                            save_filepath=file.path(folder_path, "TWO_ISSUES_NO_COMMENTS_issues_1121646814_1121719175.json"))

  return(folder_path)
}

#' Example Jira Issue Tracker With Comments
#'
#' Create fake jira issue tracker with one issue with 2 comments
#'
#' @param folder_path The path where the folder will be created
#' @param folder_name The name of the folder
#' @return the JSON folder path of the newly created issue issue tracker
#' @export
#' @keywords internal
example_jira_issue_comments <- function(folder_path, folder_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path = folder_path, folder_name = folder_name)

  issue1 <- make_jira_issue(
    jira_domain_url = "https://project.org/jira",
    issue_key = "PROJECT-123",
    project_key = "PROJECT",
    summary = "Summary of new feature",
    description = "The new features have been implemented",
    issue_type = "New Feature",
    resolution = "Finished",
    priority = "Minor",
    status = "Open",
    labels = c("pull-request-available"),
    components = c("jira", "mail"),
    affects_versions = c("3.4.3"),
    fix_versions = c("3.4.2"),
    assignee_name = "Moe",
    creator_name = "Bob",
    reporter_name = "Joe",
    comments = c(
      "This is the first body comment.",
      "This is the second body comment."
    )
  )

  issues <- list(issue1)

  jira_json_path <- make_jira_issue_tracker(
    issues, save_filepath=file.path(folder_path,"ONE_ISSUE_WITH_COMMENTS_issues_1121646814_1121719175.json"))

  return(folder_path)
}

#' Two Thread and Three Replies Mailing List
#'
#' Create a mailing list of two e-mail threads, with
#' two and one reply respectively by two developers.
#'
#' @param folder_path The folder path to create the example
#' @param folder_name Name of the example folder
#' @param file_name Name of the file where .mbox will be stored
#' @return Folder path of .mbox sample file that was created
#' @export
#' @keywords internal
example_mailing_list_two_threads <- function(folder_path, folder_name, file_name) {

  # Create folder & repo
  folder_path <- io_make_folder(folder_path = folder_path, folder_name = folder_name)

  # Step 1: Create fake mbox replies and assign them to variables for easy editing
  thread_1_reply_1 <- make_mbox_reply(mailing_list="test-list",
                                      reply_from_author = "John Doe", reply_from_email = "johndoe@example.com",
                                      reply_to_author = "", reply_to_email =  "dev@test-list.com",
                                      reply_cc_author = "Smithsonian Doe", reply_cc_email = "smith_doe@example.com",
                                      reply_datetime = "2023-01-15T08:30:00", timezone = "EST",
                                      reply_subject = "Subject 1",
                                      reply_body = "This is the body of the test email 1 of thread 1.")

  thread_1_reply_2 <- make_mbox_reply(mailing_list="test-list",
                                      reply_from_author = "Smithsonian Doe", reply_from_email = "smith_doe@example.com",
                                      reply_to_author = "", reply_to_email =  "dev@test-list.com",
                                      reply_cc_author = "John Doe", reply_cc_email = "johndoe@example.com",
                                      reply_datetime = "2023-01-16T09:30:00", timezone = "EST",
                                      reply_subject = "Re: Subject 1",
                                      reply_body = "This is the body of the test email 2 of thread 1.")

  thread_2_reply_1 <- make_mbox_reply(mailing_list="test-list",
                                      reply_from_author = "Smithsonian Doe", reply_from_email = "smith_doe@example.com",
                                      reply_to_author = "", reply_to_email =  "dev@test-list.com",
                                      reply_cc_author = "John Doe", reply_cc_email = "johndoe@example.com",
                                      reply_datetime = "2023-01-16T09:30:00", timezone = "EST",
                                      reply_subject = "Subject 2",
                                      reply_body = "This is the body of the test email 1 of thread 2.")




  # Step 2: Concatenate each reply into the replies variable
  replies <- c(thread_1_reply_1, thread_1_reply_2, thread_2_reply_1)

  # Create mbox file from the list of replies
  mbox_path <- make_mbox_mailing_list(replies = replies, folder_path = folder_path, file_name = file_name)

  return(mbox_path)
}
