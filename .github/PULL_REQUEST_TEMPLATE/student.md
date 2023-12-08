If you are a student working on this project to fulfill a class requirement, please use the template below when submitting your pull request. 

-----

# Purpose 

1. Briefly describe the goal of the pull request and reference the issue ID associated with it. 
1. If you are referencing code in Kaiaulu, search the code on the repository and reference it. 
    1. You can reference code by selecting any file, left-clicking the line number, and choosing copy permalink. 
    1. You can reference multiple lines of code by holding Shift.
1. If you are providing a code snippet, please use [syntax highlighting](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-and-highlighting-code-blocks#syntax-highlighting), and specify the language as `r`.

# Task List 

 Define the list of functions you will be writing or modifying as a checklist. You should break the list of functions into sub-section steps if functions in step 2 require functions in step 1 to be completed.

 In addition, you should consider the naming convention so the code is consistent. If variables_are_written_in_this_form, do not write variableInADifferentManner.


## Step 1

Observe the form of the checklist: You must specify a) which file the function will be added, b) the function name, and c) the parameter name. 

- [ ] io.R/io_create_folder(folder_path,folder_name): This function takes as parameter a folder path and creates a folder in it.

## Step 2 

We can't create a git repository without a folder; hence, this belongs to a subsequent step. 

- [ ] git.R/git_init(folder_path) This function takes as a parameter a folder path and initializes a git repository.

-----

# Before you request Review

1. Check all the code you wrote is consistent with the specification above. If you need to deviate from the specification, asking before proceeding is a good idea. 
1. Did you build the code (Cmd/Ctrl + Shift + B) and documentation (Cmd/Ctrl + Shift + D)?
1. Is the documentation of every function clear?
    1. Is the title concise?
    1. Does the body explain the purpose of the function?
    1. Are all the parameters specified?
1. Have you executed all unit tests, and they have passed (Cmd/Ctrl + Shift + T)?
1. Have you run `Check` (Cmd/Ctrl + Shift + E)?
1. Does Kaiaulu repository GitHub Actions Checks pass?

# Asking for Review 

1. When ready, request team member(s) for a [code review](https://github.com/features/code-review). Ensure your team members know you are waiting on them for review. You should have a second issue to work in parallel while waiting on your team members. If not, reach out.  
1. If you are code reviewing, Click the "Files changed" tab on the PR. Click any line to add in-line comments. Choose `Start a review` (do not choose `Add single comment`). Add all the reviews required, then on the top right, click "Review changes", add a summary, and use the checkbox "Approve" or "Request Changes". 
1. Perform additional commits to your fork. Request changes again from your team. 
1. Continue this process until all teammates approve. Then, request a review from the repository maintainer. 

