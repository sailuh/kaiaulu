# Example Function Proposals
* Create an example for parse_gitlog(), which contains a commit for a file within a folder within a folder to test parser behavior. 
* Create examples for parse_git_blame(), which test the 2, 3, and 4 line git blame output conditions
* Create an example for parse_gitlog_entity(), which contains a function, class and method in order to test that it can correctly filter them.

# Third Party Tools
## Perceval
* `”testthat given Perceval is specified on tools.yml, when Perceval --help is called, then it returns the help message”`
* `”testthat given Perceval is specified on tools.yml, when the path to Perceval is ran, then the Perceval path is a directory called perceval”`

## Git
* `”testthat given Git returns the help message, then there is a .git folder in the working directory”`

## utags
* `”testthat given utags is specified on tools.yml, when utags --help is called, then it returns the help message”`
~~* `”testthat given utags is specified on tools.yml, then utags path is an executable file called ctags-universal”`~~

# Parsers
## Git Log
**Path Tests**
* `”testthat when parse_gitlog is given the path to git repo has no .git folder, then the function will gracefully error specifying that no .git folder exists at the respository”`
* `”testthat when parse_gitlog is given an invalid path to Perceval, then the function will gracefully error specifying that the path to Perceval is invalid”`
* `”testthat when parse_gitlog is given an invalid save_path argument, then a data table is returned”`
* `”testthat when parse_gitlog is given invalid perl_regex argument, then the function will gracefully error specifying that the perl_regex in invalid”`

**Behavioral Tests**
* `”testthat when parse_gitlog is given valid paths to perceval and a git repo, then it returns a data table”`
* `”testthat when parse_gitlog is given a repo with two branches with one commit each, it extracts only the current branch commit”`
* `”testthat when parse_gitlog is given a repo with no commits, then a Git command error is thrown”` - Error is not propagated via system call, temporarily caught with ```try catch``` block
* `”testthat when parse_gitlog is given a renamed file repository, then the renamed file name is parsed to the file_pathname_renamed column in the data table”`
* `”testthat when parse_gitlog is given a valid perl_regex argument, then the data table has a filtered output resulting from the argument”`
* `”testthat when parse_gitlog is given a repo with a file inside a folder inside another folder, then it will extract the current commit correctly”`

## Git Blame
**Path Tests**
* `”testthat when parse_git_blame is given a git_repo_path with no .git folder, then the function will gracefully error specifying that no .git folder exists at the respository”`
* `”testthat when parse_git_blame is given an invalid commit_hash, then NULL is returned”`
* `”testthat when parse_git_blame is given a file_path that does not exist, then the function will gracefully error and specify that the save_path does not exist”`

**Behavioral Tests**
* `”testthat when parse_git_blame is given a minimal 2-line git blame output(commit hash + content), then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 3-line git blame output including filename, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 4-line git blame output including previous commit and previous file, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 12-line git blame output with full author and committer metadata, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 13-line git blame output with full metadata and previous commit info, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given unrecognized git blame line output length, then an informative error is thrown explaining the error to the user”`
* `”testthat when parse_git_blame is given a commit hash with empty lines committed, then a data table with correctly parsed empty lines is returned”`
* `”testthat when parse_git_blame is given a commit behind the most recent commit, then a correctly parsed data table for that commit is returned”`
* `”testthat when parse_git_blame is given moved lines in a commit, then moved lines are correctly tracked and their blame is parsed accordingly”`

## Git Log Entity
**Path Tests**
* `”testthat when parse_gitlog_entity is given a git_repo_path with no .git folder, then the function will gracefully error specifying that no .git folder exists at the repository`'
* `"testthat when parse_gitlog_entity is given an invalid path to utags, then the function will gracefully error specifying that the utags path is invalid”`
* `”testthat when parse_gitlog_entity is given an invalid project_git_log, then the function will gracefully error specifying that the project_git_log is invalid”`

**Behavioral Tests**
* `”testthat when parse_gitlog_entity is given valid git_repo_path, utags_path, project_git_log, and kinds arguments, then a data table is returned”`
* `”testthat when parse_gitlog_entity is given valid git_repo_path, utags_path, project_git_log, and kinds arguments and the progress bar argument set to TRUE, then a data table is returned”`
* `”testthat when parse_gitlog_entity is given ‘c’ specified for kinds, then the entity_type and entity_definition_name columns contain only classes from the parsed files”`
* `”testthat when parse_gitlog_entity is given ‘f’ specified for kinds, then the entity_type and entity_definition_name columns contain only functions from the parsed files”`
* `”testthat when parse_gitlog_entity is given ‘m’ specified for kinds, then the entity_type and entity_definition_name columns contain only methods from the parsed files”`
* `”testthat when parse_gitlog_entity is given ‘c’, `f`, and `m` specified for kinds, then the entity_type and entity_definition_name columns contain classes, functions, and methods from the parsed files”`
* `”testthat when parse_gitlog_entity is given valid git_repo_path, utags_path, project_git_log, and kinds arguments, then the returned data table’s commit_hash column matches with the commit_hash column from object project_git_log”`
* `”testthat when parse_gitlog_entity is given valid git_repo_path, utags_path, project_git_log, and kinds arguments, the amount of commit hashes in project_git_log correspond to the amount of commits in the returned data table”`

## Commit Message ID
**Path Tests**
* `”testthat when parse_commit_message_id is given invalid project_git, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat when parse_commit_message_id is given invalid commit_message_id_regex, then the function will gracefully error specifying that the commit_message_id_regex is in an invalid format”`

**Behavioral Tests**
* `”testthat when parse_commit_message_id is given valid project_git and commit_message_id_regex arguments, then a new column commit_message_id is added in the returned data table”`
* `”testthat when parse_commit_message_id is given valid project_git and commit_message_id_regex arguments, then it correctly inputs correct commit messages in new column in the outputted data table”`
* `”testthat when parse_commit_message_id is given valid project_git and commit_message_id_regex arguments, then correctly filters using commit_message_id_regex to input correct commit messages in new column”` 

# Git Cmd
**These do not need Behavioral Tests, due to being wrappers for Git's own functions. Their behavior should be tested for by Git. I am planning to wrap these in try catch statements, which will output the system2 warning/error for the user, as well as stop the function and return an error message further specifying the error to the user. These wrappers currently all generally error the same way, by outputting a system2 warning, but not actually erroring and stopping the code. This occurs when they are given any invalid argument by the user, which breaks the command built in system2.**

## Git Add
**Path Tests**
* `”testthat when git_add is given valid git_repo, folder_path, and filepath, then git add is performed”` 
* `”testthat when git_add is given an invalid git_repo, then the respective system2 warning output is returned to the user”` 

## Git Commit
**Path Tests**
* `”testthat when git_commit is given valid git_repo, folder_path, commit_msg, author, and email, then git commit is performed”` 
* `”testthat when git_commit is given an invalid git_repo, then it returns an error”` 
    
## Git Init
**Path Tests**
* `”testthat when git_init is given valid folder_path, then git init is performed”` 
* `”testthat when git_init is given an invalid folder_path, then it returns an error”` 

## Git mv
**Path Tests**
* `”testthat when git_mv is given valid git_repo, folder_path, old_name, and new_name, then git mv is performed”` 
* `”testthat when git_mv is given an invalid git_repo, then it returns an error”` 

## Git head
**Path Tests**
* `”testthat when git_head is given valid git_repo_path, then git head is performed”` 
* `”testthat when git_head is given an invalid git_repo_path, then it returns an error”` 

## Git log
**Path Tests**
* `”testthat when git_log is given valid git_repo_path, flags, and save_path, then git log is performed”` 
* `”testthat when git_log is given an invalid git_repo_path, then it returns an error”` 

## Git blame
**Path Tests**
* `”testthat git_blame when given valid git_repo_path, flags, commit_hash, and file_path, then git blame is performed”` 
* `”testthat git_blame when given an invalid git_repo_path, then it returns an error”` 
* `”testthat git_blame when given a file deleted by the blamed commit, then NULL is returned”` 

**Git log and git blame currently already have try catch statements that I will look into. They seem to return NULL as opposed to a function warning/error, which gives the user less information about what actually went wrong. The commit that is here https://github.com/sailuh/kaiaulu/commit/67ff85fbc13728c635fb8b143488fa0ec4f5b444#diff-e8458e36c0755109d2e986623d2229882eab41bbfd1312f742d999b674e1d583 and it looks like the intended behavior for the try catch was commented out. I will uncomment it and see if anything breaks.**

## Git Create Sample Log
## Git Delete Sample Log
**Both serve a function similar to R/example.R, and so don’t need unit tests. They are immutable.**

# Transforms
## Gitlog to bipartite
**Path Tests**
* `”testthat when transform_gitlog_to_bipartite_network is given an invalid project_git argument, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat when transform_gitlog_to_bipartite_network is given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and mode, then a nodes data table is returned"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and mode, then an edgelist data table is returned"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and mode, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and mode, then the number of commit nodes is the same as the number of unique commit_hashes in the git log"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and mode, then the number of file nodes is the same as the number of unique files in the git log"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and author-file mode, then the number of edges is the same as the number of author to file interactions"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and committer-file mode, then the number of edges is the same as the number of committer to file interactions"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and commit-file mode, then the number of edges is the same as the number of commit_hash to file interactions"`
* `"testthat when transform_gitlog_to_bipartite_network is given valid project_git and author-committer mode, then the number of edges is the same as the number of author to committer interactions"`
* `”testthat when transform_gitlog_to_bipartite_network is given valid project_git and author-file mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_bipartite_network is given valid project_git and committer-file mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_bipartite_network is given valid project_git and commit-file mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_bipartite_network is given valid project_git and author-committer mode, then edge weights for all edges are calculated correctly”`

## Gitlog to entity bipartite
**Path Tests**
* `”testthat when transform_gitlog_to_entity_bipartite_network is given an invalid project_git_entity argument, then the function will gracefully error specifying that the project_git_entity is invalid”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and mode, then a nodes data table is returned"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and mode, then an edgelist data table is returned"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and mode, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and mode, then the number of committer nodes is the same as the number of unique committers in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and mode, then the number of commit nodes is the same as the number of unique commit_hashes in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and mode, then the number of entity nodes is the same as the number of unique entities in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and  author-entity mode, then the number of edges is the same as the number of author to entity interactions"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and  committer-entity mode, then the number of edges is the same as the number of committer to entity interactions"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and commit-entity mode, then the number of edges is the same as the number of commit_hash to entity interactions"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and author-committer mode, then the number of edges is the same as the number of author to committer interactions"`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and author-entity mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and committer-entity mode, then edge weights for all edges are are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and commit-entity mode, then edge weights for all edges are are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given valid project_git_entity and author-committer mode, then edge weights for all edges are are calculated correctly”`

## Gitlog to temporal
**Path Tests**
* `”testthat when transform_gitlog_to_temporal_network is given an invalid project_git argument, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat when transform_gitlog_to_temporal_network is given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`
* `”testthat when transform_gitlog_to_temporal_network is given invalid lag argument, then the function will gracefully error with an explanation for the argument lag”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_temporal_network is given valid project_git, mode, and lag arguments, then a nodes data table is returned"`
* `"testthat when transform_gitlog_to_temporal_network is given valid project_git, mode, and lag arguments, then an edges data table is returned"`
* `"testthat when transform_gitlog_to_temporal_network is given valid project_git, author mode, and lag arguments, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_temporal_network is given valid project_git, committer mode, and lag arguments, then the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat when transform_gitlog_to_temporal_network is given valid project_git, mode, and lag arguments, then one_lag correctly outputs edges from only the current and previous time steps"`
* `"testthat transform_gitlog_to_temporal_network is given valid project_git, mode, and lag arguments, then all_lag correctly outputs the edges from all time steps"`
* `”testthat when transform_gitlog_to_temporal_network is given valid project_git, mode, and one_lag arguments, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_temporal_network is given valid project_git, mode, and no_lag arguments, then edge weights for all edges are calculated correctly”`

## Gitlog to entity temporal
**Path Tests**
* `”testthat when transform_gitlog_to_entity_temporal_network is given an invalid project_git_entity argument, then the function will gracefully error specifying that the project_git_entity is invalid”`
* `”testthat when transform_gitlog_to_entity_temporal_network is given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`
* `”testthat when transform_gitlog_to_entity_temporal_network is given invalid lag argument, then the function will gracefully error with an explanation for the argument lag”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, mode, and lag arguments, then a nodes data table is returned"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, mode, and lag arguments, then an edgelist data table is returned"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, author mode, and lag arguments, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, committer mode, and lag arguments, then the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, mode, and lag arguments, then one_lag correctly outputs edges from the current and previous time steps"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, mode, and lag arguments, then all_lag correctly outputs edges from the all time steps"`
* `”testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, mode, and one_lag arguments, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_temporal_network is given valid project_git_entity, mode, and no_lag arguments, then edge weights for all edges are calculated correctly”`

## Commit message ID to network
**Path Tests**
* `”testthat when transform_commit_message_id is given invalid project_git argument, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat when transform_commit_message_id is given invalid commit_message_id_regex, then the function will gracefully error specifying that the commit_message_id_regex is in an invalid format”`

**Behavioral Tests**
* `"testthat when transform_commit_message_id is given valid project_git and commit_messsage_id_regex arguments, then a nodes data table is returned"`
* `"testthat when transform_commit_message_id is given valid project_git and commit_messsage_id_regex arguments, then an edgelist data table is returned"`
* `"testthat when transform_commit_message_id is given valid project_git and commit_messsage_id_regex arguments, then the number of commit ID nodes is the same as the number of commit hashes in the git log"`
* `"testthat when transform_commit_message_id is given valid project_git and commit_messsage_id_regex arguments, then the number of file nodes is the same as the number of unique files in the git log"`
* `”testthat when transform_commit_message_id is given valid project_git and commit_messsage_id_regex arguments, then commit_message_id_regex correctly identifies the correct commit message ids”` 
* `”testthat when transform_commit_message_id is given valid project_git and commit_messsage_id_regex arguments, then all edges correctly flow from commits to their respective files are correct”`
* `”testthat when transform_commit_message_id is given valid project_git and commit_messsage_id_regex arguments, then all edge weights are calculated correctly”`

