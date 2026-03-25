# Third Party Tools
## Perceval
* `”testthat if Perceval is specified on tools.yml, then it returns the help message”`
* `”testthat if Perceval is specified on tools.yml, the Perceval path is a directory called perceval”`

## Git
* `”testthat if Git returns the help message, check there is a .git folder in the working directory”`

## utags
* `”testthat if utags is specified on tools.yml, then it returns the help message”`
* `”testthat if utags is specified on tools.yml, the utags path is an executable file called ctags-universal”`

# Parsers
## Git Log
**Path Tests**
* `”testthat if the path to git repo has no .git folder, the function will gracefully error with message Unable to generate git log from this repository.”`
* `”testthat if the given Perceval path is invalid, the function will gracefully error with message Unable to generate git log from this repository.”`
* `”testthat if the save_path is invalid, the function will still correctly parse the git log data”`
* `”testthat given an invalid perl_regex argument, the function will gracefully error with message Unable to generate git log from this repository.”`

**Behavioral Tests**
* `”testthat calling parse_gitlog with correct perceval and correct git log path returns a data table”`
* `”testthat all commits preserve their commit order in the data table output”`
* `”testthat all necessary columns are generated in the data table output”`
* `”testthat all author_name_email fields correspond to their correct commit_message fields”`
* `”testthat all author_name_email fields correspond to their correct file_pathname fields”`
* `”testthat all author_name_email fields correspond to their correct lines_added and lines_removed fields”`
* `”testthat author_name_email, commit_message, and file_pathname columns correctly parsed the git log data”`
* `”testthat the lines_added and lines_removed columns correctly parsed the git log data”`
* `”testthat calling parse_gitlog on two branches with one commit each extracts only the current branch commits”`
* `”testthat calling parse_gitlog on a repo with no commits throws a Git command error”` - Error is not propagated via system call, temporarily caught with ```try catch``` block
* `”testthat calling parse_gitlog with a renamed file repository parses the renamed file to the file_pathname_renamed column”`
* `”testthat calling parse_gitlog with perl_regex correctly filters git log data”`

## Git Blame
**Path Tests**
* `”testthat function errors gracefully and outputs an informative error message given a git_repo_path with no .git folder”`
* `”testthat function returns NULL when given an invalid commit_hash”`
* `”testthat function errors gracefully given a file_path that does not exist and informs the user”`

**Behavioral Tests**
* `”testhat function correctly parses a minimal 2-line git blame output(commit hash + content) into a data table”`
* `”testthat function correctly parses a 3-line git blame output including filename into a data table”`
* `”testthat function correctly parses a 4-line git blame output including previous commit and previous file into a data table”`
* `”testthat function correctly parses a 12-line git blame output with full author and committer metadata into a data table”`
* `”testthat function correctly parses a 13-line git blame output with full metadata and previous commit info into a data table”`
* `”testthat function throws an informative error for unrecognized git blame line output lengths into a data table”`
* `”testthat parse_git_blame will correctly parse empty lines committed”`
* `”testthat parse_git_blame will correctly parse blame data for a commit behind the most recent commit”`
* `”testthat parse_git_blame will correctly track moved lines and parse their blame accordingly”`

## Git Log Entity
**Path Tests**
* `”testthat function errors gracefully and outputs an informative error message given a git_repo_path with no .git folder”`
* `"testthat if the given utags path is invalid, the function will gracefully error with message regarding it”`
* `”testthat if the given object project_git_log is invalid, the function will gracefully error with message regarding it”`

**Behavioral Tests**
* `”testthat calling parse_gitlog_entity with correct fields returns a data table”`
* `”testthat calling parse_gitlog_entity with correct fields and progress bar set to TRUE returns a data table”`
* `”testthat calling parse_gitlog_entity with kinds ‘c’ correctly returns correct entity_type and entity_definition_name columns in the outputted data table”`
* `”testthat calling parse_gitlog_entity with kinds ‘f’ correctly returns correct entity_type and entity_definition_name columns in the outputted data table”`
* `”testthat calling parse_gitlog_entity with kinds ‘m’ correctly returns correct entity_type and entity_definition_name columns in the outputted data table”`
* `”testthat calling parse_gitlog_entity with kinds ‘c’, `f`, and `m` correctly returns correct entity_type and entity_definition_name columns in the outputted data table”`
* `”testthat commit_hash column matches with the column from object project_git_log in the outputted data table”`
* `”testthat the amount of commit hashes correspond to the amount of commits in the outputted data table”`


## Commit Message ID
**Path Tests**
* `”testhat function throws informative error message given invalid project_git object”`
* `”testthat function throws informative error message given erroneous commit_message_id_regex”`

**Behavioral Tests**
* `”testthat parse_commit_message_id creates a new column commit_message_id in the outputted data table”`
* `”testthat parse_commit_message_id correctly inputs correct commit messages in new column in the outputted data table”`
* `”testthat parse_commit_message_id correctly filters using commit_message_id_regex to input correct commit messages in new column”` 

# Git Cmd
**These do not need Behavioral Tests, due to being wrappers for Git's own functions. Their behavior should be tested for by Git.**

## Git Add
**Path Tests**
* `”testthat git_add wrapper performs git add given correct arguments”` 
* `”testthat function returns the respective system2 warning output to the user when given an invalid git_repo”` 
* `”testthat function returns the respective system2 warning output to the user when given an invalid folder_path”` 
* `”testthat function returns the respective system2 warning output to the user when given an given invalid file_path”` 

## Git Commit
**Path Tests**
* `”testthat git_commit wrapper performs git commit given correct arguments”` 
* `”testthat function returns a system2 warning output to the user when given an invalid git_repo”` 
    
## Git Init
**Path Tests**
* `”testthat git_init wrapper performs git init given correct arguments”` 
* `”testthat function returns a system2 warning output to the user when given an invalid folder_path”` 

## Git mv
**Path Tests**
* `”testthat git_mv wrapper performs git mv given correct arguments”` 
* `”testthat function returns a system2 warning output to the user when given an invalid git_repo”` 

## Git head
**Path Tests**
* `”testthat git_head wrapper performs git head given correct arguments”` 
* `”testthat function returns a system2 warning output to the user when given an invalid git_repo_path”` 

## Git log
**Path Tests**
* `”testthat git_log wrapper performs git log given correct arguments”` 
* `”testthat function returns NULL when given an invalid git_repo_path”` 

## Git blame
**Path Tests**
* `”testthat git_blame wrapper performs git blame given correct arguments”` 
* `”testthat function returns NULL when given an invalid git_repo_path”` 
* `”testthat function returns NULL when given a file deleted by the blamed commit”` 


**The Git wrappers return the same warning regardless of what argument is incorrect, so testing for multiple different incorrect arguments seems pointless to me.**

## Git Create Sample Log
## Git Delete Sample Log
**Both serve a function similar to R/example.R, and so don’t need unit tests. They are immutable.**

# Transforms
## Gitlog to bipartite
**Path Tests**
* `”testthat if the given object project_git is invalid, the function will gracefully error with message regarding it”`
* `”testthat function throws an error given invalid mode and explains the argument mode”`

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
* `”testthat if the given object project_git_entity is invalid, the function will gracefully error with message regarding it”`
* `”testthat function throws an error given invalid mode and explains the argument mode”`

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
* `”testthat if the given object project_git is invalid, the function will gracefully error with message regarding it”`
* `”testthat function throws an error given invalid mode and explains the argument mode”`
* `”testthat function throws an error given invalid lag and explains the argument lag”`

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
* `”testthat if the given object project_git_entity is invalid, the function will gracefully error with message regarding it”`
* `”testthat function throws an error given invalid mode and explains the argument mode”`
* `”testthat function throws an error given invalid lag and explains the argument lag”`

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
* `”testthat if the given object project_git is invalid, the function will gracefully error with message regarding it”`
* `”testthat function throws informative error message given erroneous commit_message_id_regex”`

**Behavioral Tests**
* `"testthat the commit message id transform outputs a nodes data table"`
* `"testthat the commit message id transform outputs an edgelist data table"`
* `"testthat the number of commit ID nodes is the same as the number of commit hashes in the git log"`
* `"testthat the number of file nodes is the same as the number of unique files in the git log"`
* `”testthat commit_message_id_regex correctly identifies the correct commit message ids”` 
* `”testthat all edges correctly flow from commits to their respective files are correct”`
* `”testthat edge weights for all edges are correct”`
