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
* `”testthat parse_gitlog: given the path to git repo has no .git folder, then the function will gracefully error specifying that no .git folder exists at the respository)”`
* `”testthat parse_gitlog: given the path to Perceval is invalid, then the function will gracefully error specifying that the path to Perceval is invalid”`
* `”testthat parse_gitlog: given the save_path argument is invalid, then a data table is returned”`
* `”testthat parse_gitlog: given the perl_regex argument is invalid, then the function will gracefully error specifying that the perl_regex in invalid”`

**Behavioral Tests**
* `”testthat parse_gitlog: given valid perceval and git log paths, then it returns a data table”`
* `”testthat parse_gitlog: given valid perceval and git log paths, then all commits preserve their commit order in the data table output”`
* `”testthat parse_gitlog: given a correct perceval and correct git log path, then all necessary columns are generated in the data table output”`
* `”testthat parse_gitlog: given a correct perceval and correct git log path, then all author_name_email fields correspond to their correct commit_message fields”`
* `”testthat parse_gitlog: given a correct perceval and correct git log path, then all author_name_email fields correspond to their correct file_pathname fields”`
* `”testthat parse_gitlog: given a correct perceval and correct git log path, then all author_name_email fields correspond to their correct lines_added and lines_removed fields”`
* `”testthat parse_gitlog: given a correct perceval and correct git log path, then the author_name_email, commit_message, and file_pathname columns are correctly parsed the git log data”`
* `”testthat parse_gitlog: given a correct perceval and correct git log path, then the lines_added and lines_removed columns correctly parsed the git log data”`
* `”testthat parse_gitlog: given a correct perceval and correct git log path, then calling parse_gitlog on two branches with one commit each extracts only the current branch commits”`
* `”testthat parse_gitlog: given a repo with no commits, then a Git command error is thrown”` - Error is not propagated via system call, temporarily caught with ```try catch``` block
* `”testthat parse_gitlog: given a renamed file repository, then renamed file is parsed to the file_pathname_renamed column”`
* `”testthat parse_gitlog: given a valid perl_regex argument, then the data table has correctly filtered output”`

## Git Blame
**Path Tests**
* `”testthat parse_git_blame: given a git_repo_path with no .git folder, then the function will gracefully error specifying that no .git folder exists at the respository”`
* `”testthat parse_git_blame: given an invalid commit_hash, then NULL is returned”`
* `”testthat parse_git_blame: given a file_path that does not exist, then the function will gracefully error and specify that the save_path does not exist”`

**Behavioral Tests**
* `”testthat parse_git_blame: given a minimal 2-line git blame output(commit hash + content), then a data table with the correctly parsed data is returned”`
* `”testthat parse_git_blame: given a 3-line git blame output including filename, then a data table with the correctly parsed data is returned”`
* `”testthat parse_git_blame: given a 4-line git blame output including previous commit and previous file, then a data table with the correctly parsed data is returned”`
* `”testthat parse_git_blame: given a 12-line git blame output with full author and committer metadata, then a data table with the correctly parsed data is returned”`
* `”testthat parse_git_blame: given a 13-line git blame output with full metadata and previous commit info, then a data table with the correctly parsed data is returned”`
* `”testthat parse_git_blame: given unrecognized git blame line output length, then an informative error is thrown explaining the error to the user”`
* `”testthat parse_git_blame: given a commit hash with empty lines committed, then a data table with correctly parsed empty lines is returned”`
* `”testthat parse_git_blame: given a commit behind the most recent commit, then a correctly parsed data table for that commit is returned”`
* `”testthat parse_git_blame: given moved lines in a commit, then moved lines are correctly tracked and their blame is parsed accordingly”`

## Git Log Entity
**Path Tests**
* `”testthat parse_gitlog_entity: given a git_repo_path with no .git folder, then the function will gracefully error specifying that no .git folder exists at the repository`'
* `"testthat parse_gitlog_entity: given an invalid path to utags, then the function will gracefully error specifying that the utags path is invalid”`
* `”testthat parse_gitlog_entity: given an invalid project_git_log, then the function will gracefully error specifying that the project_git_log is invalid”`

**Behavioral Tests**
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log, and kinds arguments, then a data table is returned”`
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log, and kinds arguments and the progress bar argument set to TRUE, then a data table is returned”`
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log arguments, and ‘c’ specified for kinds, then the correct entity_type and entity_definition_name columns are in the outputted data table”`
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log arguments, and ‘f’ specified for kinds, then the correct entity_type and entity_definition_name columns are in the outputted data table”`
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log arguments, and ‘m’ specified for kinds, then the correct entity_type and entity_definition_name columns are in the outputted data table”`
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log arguments, and ‘c’, `f`, and `m` specified for kinds, then the correct entity_type and entity_definition_name columns are in the outputted data table”`
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log, and kinds arguments, then the returned data table’s commit_hash column matches with the commit_hash column from object project_git_log”`
* `”testthat parse_gitlog_entity: given valid git_repo_path, utags_path, project_git_log, and kinds arguments, the amount of commit hashes in project_git_log correspond to the amount of commits in the returned data table”`


## Commit Message ID
**Path Tests**
* `”testthat parse_commit_message_id: given invalid project_git, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat parse_commit_message_id: given invalid commit_message_id_regex, then the function will gracefully error specifying that the commit_message_id_regex is in an invalid format”`

**Behavioral Tests**
* `”testthat parse_commit_message_id: given valid project_git and commit_message_id_regex arguments, then a new column commit_message_id is added in the returned data table”`
* `”testthat parse_commit_message_id: given valid project_git and commit_message_id_regex arguments, then in the returned data table, correctly inputs correct commit messages in new column in the outputted data table”`
* `”testthat parse_commit_message_id: given valid project_git and commit_message_id_regex arguments, then correctly filters using commit_message_id_regex to input correct commit messages in new column”` 

# Git Cmd
**These do not need Behavioral Tests, due to being wrappers for Git's own functions. Their behavior should be tested for by Git.**

## Git Add
**Path Tests**
* `”testthat git_add: given valid git_repo, folder_path, and filepath, then git add is performed”` 
* `”testthat git_add: given an invalid git_repo, then the respective system2 warning output is returned to the user”` 
* `”testthat git_add: given an invalid folder_path, then the respective system2 warning output is returned to the user”` 
* `”testthat git_add: given an invalid file_path, then the respective system2 warning output is returned to the user”` 

## Git Commit
**Path Tests**
* `”testthat git_commit: given valid git_repo, folder_path, commit_msg, author, and email, then git commit is performed”` 
* `”testthat git_commit: given an invalid git_repo, then the respective system2 warning output is returned to the user”` 
    
## Git Init
**Path Tests**
* `”testthat git_init: given valid folder_path, then git init is performed”` 
* `”testthat git_init: given an invalid folder_path, then the respective system2 warning output is returned to the user”` 

## Git mv
**Path Tests**
* `”testthat git_mv: given valid git_repo, folder_path, old_name, and new_name, then git mv is performed”` 
* `”testthat git_mv: given an invalid git_repo, then the respective system2 warning output is returned to the user”` 

## Git head
**Path Tests**
* `”testthat git_head: given valid git_repo_path, then git head is performed”` 
* `”testthat git_head: given an invalid git_repo_path, then the respective system2 warning output is returned to the user”` 

## Git log
**Path Tests**
* `”testthat git_log: given valid git_repo_path, flags, and save_path, then git log is performed”` 
* `”testthat git_log: given an invalid git_repo_path, then NULL is returned”` 

## Git blame
**Path Tests**
* `”testthat git_blame: given valid git_repo_path, flags, commit_hash, and file_path, then git blame is performed”` 
* `”testthat git_blame: given an invalid git_repo_path, then NULL is returned”` 
* `”testthat git_blame: given a file deleted by the blamed commit, then NULL is returned”` 

**The Git wrappers return the same warning regardless of what argument is incorrect, so testing for multiple different incorrect warnings seems pointless to me. The only important factor is ensuring that a warning is returned to the user.**

## Git Create Sample Log
## Git Delete Sample Log
**Both serve a function similar to R/example.R, and so don’t need unit tests. They are immutable.**

# Transforms
## Gitlog to bipartite
**Path Tests**
* `”testthat transform_gitlog_to_bipartite_network: given invalid project_git argument, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat transform_gitlog_to_bipartite_network: given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`

**Behavioral Tests**
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then a nodes data table is returned"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then an edgelist data table is returned"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of committer nodes is the same as the number of unique committers in the git log"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of commit nodes is the same as the number of unique commit_hashes in the git log"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of file nodes is the same as the number of unique files in the git log"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of edges is the same as the number of author to file interactions in the author-file mode"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of edges is the same as the number of committer to file interactions in the committer-file mode"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of edges is the same as the number of commit_hash to file interactions in the commit-file mode"`
* `"testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then the number of edges is the same as the number of author to committer interactions in the author-committer mode"`
* `”testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then edge weights for all edges are correct for author-file”`
* `”testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then edge weights for all edges are correct for committer-file”`
* `”testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then edge weights for all edges are correct for commit-file”`
* `”testthat transform_gitlog_to_bipartite_network: given valid project_git and mode, then edge weights for all edges are correct for author-committer”`

## Gitlog to entity bipartite
**Path Tests**
* `”testthat transform_gitlog_to_entity_bipartite_network: given invalid project_git_entity argument, then the function will gracefully error specifying that the project_git_entity is invalid”`
* `”testthat transform_gitlog_to_entity_bipartite_network: given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`

**Behavioral Tests**
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then a nodes data table is returned"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then an edgelist data table is returned"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of committer nodes is the same as the number of unique committers in the git log"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of commit nodes is the same as the number of unique commit_hashes in the git log"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of entity nodes is the same as the number of unique entities in the git log"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of edges is the same as the number of author to entity interactions in the author-entity mode"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of edges is the same as the number of committer to entity interactions in the committer-entity mode"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of edges is the same as the number of commit_hash to entity interactions in the commit-entity mode"`
* `"testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then the number of edges is the same as the number of author to committer interactions in the author-committer mode"`
* `”testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then edge weights for all edges are correct for author-entity”`
* `”testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then edge weights for all edges are correct for committer-entity”`
* `”testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then edge weights for all edges are correct for commit-entity”`
* `”testthat transform_gitlog_to_entity_bipartite_network: given valid project_git_entity and mode, then edge weights for all edges are correct for author-committer”`

## Gitlog to temporal
**Path Tests**
* `”testthat transform_gitlog_to_temporal_network: given an invalid project_git argument, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat transform_gitlog_to_temporal_network: given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`
* `”testthat transform_gitlog_to_temporal_network: given invalid lag argument, then the function will gracefully error with an explanation for the argument lag”`

**Behavioral Tests**
* `"testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then a nodes data table is returned"`
* `"testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then an edges data table is returned"`
* `"testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then one_lag correctly outputs edges from the current and previous time steps"`
* `"testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then all_lag correctly outputs edges from the all time steps"`
* `”testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then edge weights for all edges are correct for one_lag”`
* `”testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then edge weights for all edges are correct for no_lag”`
* `”testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then edge connections for all edges are correct for one_lag”`
* `”testthat transform_gitlog_to_temporal_network: given valid project_git, mode, and lag arguments, then edge connections for all edges are correct for no_lag”`

## Gitlog to entity temporal
**Path Tests**
* `”testthat transform_gitlog_to_entity_temporal_network: given an invalid project_git_entity argument, then the function will gracefully error specifying that the project_git_entity is invalid”`
* `”testthat transform_gitlog_to_entity_temporal_network: given invalid mode argument, then the function will gracefully error with an explanation for the argument mode”`
* `”testthat transform_gitlog_to_entity_temporal_network: given invalid lag argument, then the function will gracefully error with an explanation for the argument lag”`

**Behavioral Tests**
* `"testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then a nodes data table is returned"`
* `"testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then an edgelist data table is returned"`
* `"testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then one_lag correctly outputs edges from the current and previous time steps"`
* `"testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then all_lag correctly outputs edges from the all time steps"`
* `”testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then edge weights for all edges are correct for one_lag”`
* `”testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then edge weights for all edges are correct for no_lag”`
* `”testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then edge connections for all edges are correct for one_lag”`
* `”testthat transform_gitlog_to_entity_temporal_network: given valid project_git_entity, mode, and lag arguments, then edge connections for all edges are correct for no_lag”`

## Commit message ID to network
**Path Tests**
* `”testthat transform_commit_message_id: given invalid project_git argument, then the function will gracefully error specifying that the project_git is invalid”`
* `”testthat transform_commit_message_id: given invalid commit_message_id_regex, then the function will gracefully error specifying that the commit_message_id_regex is in an invalid format”`

**Behavioral Tests**
* `"testthat transform_commit_message_id: given valid project_git and commit_messsage_id_regex arguments, then a nodes data table is returned"`
* `"testthat transform_commit_message_id: given valid project_git and commit_messsage_id_regex arguments, then an edgelist data table is returned"`
* `"testthat transform_commit_message_id: given valid project_git and commit_messsage_id_regex arguments, then the number of commit ID nodes is the same as the number of commit hashes in the git log"`
* `"testthat transform_commit_message_id: given valid project_git and commit_messsage_id_regex arguments, then the number of file nodes is the same as the number of unique files in the git log"`
* `”testthat transform_commit_message_id: given valid project_git and commit_messsage_id_regex arguments, then commit_message_id_regex correctly identifies the correct commit message ids”` 
* `”testthat transform_commit_message_id: given valid project_git and commit_messsage_id_regex arguments, then all edges correctly flow from commits to their respective files are correct”`
* `”testthat transform_commit_message_id: given valid project_git and commit_messsage_id_regex arguments, then edge weights for all edges are correct”`
