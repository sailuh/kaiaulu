# Third Party Tools
## Perceval
* `”testthat Perceval is installed in order to run the Git Log Notebooks”`
* `”testthat Perceval is configured correctly in order to run the Git Log Notebooks”`

## Git
* `”testthat Git is installed in order to run the Git Log Notebooks”`
* `”testthat Git is configured correctly in order to run the Git Log Notebooks”`

## utags
* `”testthat utags is installed in order to run the Git Log Notebooks”`
* `”testthat utags is configured correctly in order to run the Git Log Entity Notebook”`

# Parsers
## Git Log
**Path Tests**
* `”testthat function succeeds given correct arguments”`
* `”testthat function correctly errors given incorrect git_repo_path”`
* `”testthat function correctly errors given incorrect perceval_path”`
* `”testthat function correctly errors given invalid save_path”`
* `”testthat function correctly errors given invalid perl_regex”`

**Behavioral Tests**
* `”testthat the amount of commit hashes correspond to the amount of commits”`
* `”testthat all commit hashes correspond with the correct author when there are multiple commits”`
* `”testthat all commit hashes correspond with the correct commit message when there are multiple commits”`
* `”testthat all commit hashes correspond with the correct file pathname”`
* `”testthat all commit hashes correspond with the correct commit date”`
* `”testthat calling parse_gitlog with correct perceval and correct git log path returns a data table”`
* `”testthat calling parse_gitlog does not display deleted branches”`
* `”testthat calling parse_gitlog on two branches with one commit each extracts all commits”`
* `”testthat calling parse_gitlog on a repo with no commits throws an error”`
* `”testthat calling parse_gitlog and filtering by commit size removes large sized commits”`
* `”testthat calling parse_gitlog with renamed file repository is reported on parsed git log”`
* `”testthat calling parse_gitlog with perl_regex correctly filters git log data”`

## Git Blame
**Path Tests**
* `”testthat function succeeds given correct arguments”`
* `”testthat function correctly errors given incorrect git_repo_path”`
* `”testthat function correctly errors given invalid commit_hash”`
* `”testthat function correctly errors given incorrect file_path”`

**Behavioral Tests**
* `”testthat for n_lines_content == 2, the correct commit hash and line content are parsed”`
* `”testthat for n_lines_content == 3, the correct commit hash, line content, and filename are parsed”`
* `”testthat for n_lines_content == 4, a previous commit hash exists, and the correct commit hash, line content, and filename are parsed”`
* `”testthat for n_lines_content == 13, full metadata is included with a previous commit will produce correct blame data”`
* `”testthat for n_lines_content == 12, full metadata is included with no previous commit will produce correct blame data”`
* `”testthat parse_git_blame will correctly parse empty lines committed”`
* `”testthat parse_git_blame will correctly parse blame data for a commit behind the most recent commit”`
* `”testthat parse_git_blame will correctly track moved lines and parse their blame accordingly”`

## Git Log Entity
**Path Tests**
* `”testthat function succeeds given correct arguments”`
* `”testthat function succeeds given correct arguments and progress_bar set to TRUE”`
* `”testthat function correctly errors given incorrect git_repo_path”`
* `”testthat function correctly errors given incorrect utags_path”`
* `”testthat function correctly errors given incorrect object project_git_log”`

**Behavioral Tests**
* `”testthat calling parse_gitlog_entity with correct fields returns a data table”`
* `”testthat calling parse_gitlog_entity with kinds ‘c’ correctly returns changes to all classes per commit”`
* `”testthat calling parse_gitlog_entity with kinds ‘f’ correctly returns changes to all functions per commit”`
* `”testthat calling parse_gitlog_entity with kinds ‘m’ correctly returns changes to all methods per commit”`
* `”testthat calling parse_gitlog_entity with kinds ‘c’, `f`, and `m` correctly returns changes to all classes, functions, and methods per commit”`
* `”testthat commit_hash column matches with the column from object project_git_log”`
* `”testthat n_lines_changed column correctly outputs changed lines for each author”`
* `”testthat weight column correctly outputs changed lines for each author”`
* `”testthat entity column correctly outputs the corresponding entities”`
* `”testthat the amount of commit hashes correspond to the amount of commits”`
* `”testthat all commit hashes correspond with the correct author when there are multiple commits”`

## Commit Message ID
**Path Tests**
* `”testhat function succeeds given correct arguments”`
* `”testhat function throws proper error given invalid project_git object”`
* `”testthat function throws proper error given erroneous commit_message_id_regex”`

**Behavioral Tests**
* `”testthat parse_commit_message_id creates a new column commit_message_id”`
* `”testthat parse_commit_message_id correctly inputs correct commit messages in new column”`
* `”testthat parse_commit_message_id correctly filters using commit_message_id_regex to input correct commit messages in new column”` 

# Git Cmd
**These do not need Behavioral Tests, due to being wrappers for Git's own functions. Their behavior should be tested for by Git.**

## Git Add
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given invalid git_repo”` 
* `”testthat function correctly errors given invalid folder_path”` 
* `”testthat function correctly errors given invalid file_path”` 

## Git Commit
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given invalid git_repo”` 
* `”testthat function correctly errors given invalid folder_path”` 
* `”testthat function correctly errors given invalid commit_msg”` 
* `”testthat function correctly errors given invalid email”` 
    
## Git Init
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given invalid folder_path”` 
* `”testthat function correctly errors given invalid commit_msg”` 
* `”testthat function correctly errors given invalid email”` 

## Git mv
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given invalid git_repo”` 
* `”testthat function correctly errors given invalid folder_path”` 
* `”testthat function correctly errors given invalid old_name”` 
* `”testthat function correctly errors given invalid new_name”` 

## Git head
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given invalid git_repo_path”` 

## Git log
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”Test for proper error given invalid git_repo_path”` 
* `”Test for proper error given invalid flags”` 
* `”Test for proper error given invalid save_path”` 

## Git blame
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given invalid git_repo_path”` 
* `”testthat function correctly errors given invalid commit_hash”` 
* `”testthat function correctly errors given invalid file_path”` 

## Git Create Sample Log
## Git Delete Sample Log
**Both serve a function similar to R/example.R, and so don’t need unit tests**

# Transforms
## Gitlog to bipartite
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given incorrect object project_git”`
* `”testthat function correctly errors given invalid mode”`

**Behavioral Tests**
* `"testthat the bipartite transform outputs a nodes data table"`
* `"testthat the bipartite transform outputs an edgelist data table"`
* `"testthat the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat the number of committer nodes is the same as the number of unique committers in the git log"`
* `"testthat the number of commit nodes is the same as the number of unique commit_hashes in the git log"`
* `"testthat the number of file nodes is the same as the number of unique files in the git log"`
* `"testthat the number of edges is the same as the number of author to file interactions in the author-file mode"`
* `"testthat the number of edges is the same as the number of committer to file interactions in the committer-file mode"`
* `"testthat the number of edges is the same as the number of commit_hash to file interactions in the commit-file mode"`
* `"testthat the number of edges is the same as the number of author to committer interactions in the author-committer mode"`
* `”testthat edge weights for all edges are correct for author-file”`
* `”testthat edge weights for all edges are correct for committer-file”`
* `”testthat edge weights for all edges are correct for commit-file”`
* `”testthat edge weights for all edges are correct for author-committer”`

## Gitlog to entity bipartite
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given incorrect object project_git_entity”`
* `”testthat function correctly errors given invalid mode”`

**Behavioral Tests**
* `"testthat the entity bipartite transform outputs a nodes data table"`
* `"testthat the entity bipartite transform outputs an edgelist data table"`
* `"testthat the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat the number of committer nodes is the same as the number of unique committers in the git log"`
* `"testthat the number of commit nodes is the same as the number of unique commit_hashes in the git log"`
* `"testthat the number of entity nodes is the same as the number of unique entities in the git log"`
* `"testthat the number of edges is the same as the number of author to entity interactions in the author-entity mode"`
* `"testthat the number of edges is the same as the number of committer to entity interactions in the committer-entity mode"`
* `"testthat the number of edges is the same as the number of commit_hash to entity interactions in the commit-entity mode"`
* `"testthat the number of edges is the same as the number of author to committer interactions in the author-committer mode"`
* `”testthat edge weights for all edges are correct for author-entity”`
* `”testthat edge weights for all edges are correct for committer-entity”`
* `”testthat edge weights for all edges are correct for commit-entity”`
* `”testthat edge weights for all edges are correct for author-committer”`

## Gitlog to temporal
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given incorrect object project_git”`
* `”testthat function correctly errors given invalid mode”`
* `”testthat function correctly errors given invalid lag”`
* `”testthat function correctly errors given invalid weight_scheme_function”`

**Behavioral Tests**
* `"testthat the temporal transform outputs a nodes data table"`
* `"testthat the temporal transform outputs an edgelist data table"`
* `"testthat the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat one_lag correctly outputs edges from the current and previous time steps"`
* `"testthat all_lag correctly outputs edges from the all time steps"`
* `”testthat edge weights for all edges are correct for one_lag”`
* `”testthat edge weights for all edges are correct for no_lag”`
* `”testthat edge connections for all edges are correct for one_lag”`
* `”testthat edge connections for all edges are correct for no_lag”`

## Gitlog to entity temporal
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given incorrect object project_git_entity”`
* `”testthat function correctly errors given invalid mode”`
* `”testthat function correctly errors given invalid lag”`
* `”testthat function correctly errors given invalid weight_scheme_function”`

**Behavioral Tests**
* `"testthat the temporal transform outputs a nodes data table"`
* `"testthat the temporal transform outputs an edgelist data table"`
* `"testthat the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat one_lag correctly outputs edges from the current and previous time steps"`
* `"testthat all_lag correctly outputs edges from the all time steps"`
* `”testthat edge weights for all edges are correct for one_lag”`
* `”testthat edge weights for all edges are correct for no_lag”`
* `”testthat edge connections for all edges are correct for one_lag”`
* `”testthat edge connections for all edges are correct for no_lag”`

## Commit message ID to network
**Path Tests**
* `”testthat function succeeds given correct arguments”` 
* `”testthat function correctly errors given incorrect object project_git”`
* `”Test for proper error given invalid commit_message_id_regex”`

**Behavioral Tests**
* `"testthat the commit message id transform outputs a nodes data table"`
* `"testthat the commit message id transform outputs an edgelist data table"`
* `"testthat the number of commit ID nodes is the same as the number of commit hashes in the git log"`
* `"testthat the number of file nodes is the same as the number of unique files in the git log"`
* `”testthat commit_message_id_regex correctly identifies the correct commit message ids”` 
* `”testthat all edges correctly flow from commits to their respective files are correct”`
* `”testthat edge weights for all edges are correct”`
