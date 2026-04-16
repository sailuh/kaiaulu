# Example Function Proposals
# Create an example for parse_gitlog(), which contains a commit for a file within a folder within a folder to test parser behavior. 
# Create examples for parse_git_blame(), which test the 2, 3, and 4 line git blame output conditions
# Create an example for parse_git_entity(), which contains a function, class and method in order to test that it can correctly filter them.

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
* `”testthat when parse_gitlog is not given .git/, then an error will be raised”`
* `”testthat when parse_gitlog is not given a valid path to Perceval, then an error will be raised”`
* `”testthat when parse_gitlog is not given an existing file save path, then a data table is returned”`
* `”testthat when parse_gitlog is not given a valid Perl Regex, then an error will be raised”`

**Behavioral Tests**
* `”testthat when parse_gitlog is given valid paths to perceval and a ./git folder, then it returns a data table”`
* `”testthat when parse_gitlog is given a repository with two branches with one commit each, it extracts only the current branch commit”`
* `”testthat when parse_gitlog is given a repository with no commits, then a Git command error is thrown”` - Error is not propagated via system call, temporarily caught with ```try catch``` block
* `”testthat when parse_gitlog is given a renamed file repository, then the renamed file name is parsed to the data table”`
* `”testthat when parse_gitlog is given a valid Perl Regex, then the data table has a filtered output resulting from the specified Regex”`
* `”testthat when parse_gitlog is given a repository with a file inside a folder inside another folder, then it will extract the current commit correctly to a data table”`

## Git Blame
**Path Tests**
* `”testthat when parse_git_blame is not given .git/, then an error will be raised”`
* `”testthat when parse_git_blame is not given a valid commit hash, then an error will be raised”`
* `”testthat when parse_git_blame is not given an existing file save path, then an error will be raised”`

**Behavioral Tests**
* `”testthat when parse_git_blame is given a 2-line git blame output consisting of a commit hash and content lines, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 3-line git blame output consisting of a commit hash, file name, and content lines, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 4-line git blame output consisting of a commit hash, file name, content, previous commit and previous file, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 12-line git blame output including full author and committer metadata, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given a 13-line git blame output including full metadata and previous commit info, then a data table with the correctly parsed data is returned”`
* `”testthat when parse_git_blame is given unrecognized git blame line output length, then an error will be raised”`
* `”testthat when parse_git_blame is given a commit hash with empty lines committed, then a data table with correctly parsed empty lines is returned”`
* `”testthat when parse_git_blame is given a commit behind the most recent commit, then a correctly parsed data table for that commit is returned”`
* `”testthat when parse_git_blame is given moved lines in a commit, then moved lines are correctly tracked and their blame is parsed accordingly”`

## Git Log Entity
**Path Tests**
* `”testthat when parse_gitlog_entity is not given .git/, then an error will be raised`”
* `"testthat when parse_gitlog_entity is not given a valid path to utags, then an error will be raised”`
* `”testthat when parse_gitlog_entity is not given a valid parsed git project object, then an error will be raised”`

**Behavioral Tests**
* `”testthat when parse_gitlog_entity specifies class entities to be parsed, then the resulting data table contains the names of only the classes which exist in the given repository”`
* `”testthat when parse_gitlog_entity specifies function entities to be parsed, then the resulting data table contains the names of only the functions which exist in the given repository”`
* `”testthat when parse_gitlog_entity specifies method entities to be parsed, then the resulting data table contains the names of only the methods which exist in the given repository”`
* `”testthat when parse_gitlog_entity specifies class, function, and method entities to be parsed, then the resulting data tables contains the names of classes, functions, and methods from the parsed files”`
* `”testthat when parse_gitlog_entity is given valid git repository, path to utags, parsed git log, and specifies class, function, and method entities to be parsed, then the returned data table’s commit hashes match with those from the parsed git project”`
* `”testthat when parse_gitlog_entity is given valid git repository, path to utags, parsed git log, and specifies class, function, and method entities to be parsed, then the amount of commit hashes in the returned data table’s is the same amount from the parsed git project”`

## Commit Message ID
**Path Tests**
* `”testthat when parse_commit_message_id is not given a valid parsed git log, then an error will be raised”`
* `”testthat when parse_commit_message_id is not given a valid Regex, then an error will be raised”`

**Behavioral Tests**
* `”testthat when parse_commit_message_id is given valid parsed git project and Regex, then a new column containing the commit message ID is added in the returned data table”`
* `”testthat when parse_commit_message_id is given valid parsed git project and Regex, then it correctly inputs correct commit message IDs in the new column in the outputted data table”`
* `”testthat when parse_commit_message_id is given valid parsed git project and Regex, then it uses the provided Regex to filter non-matching from commit messages the data table”` 

# Git Cmd
**These do not need Behavioral Tests, due to being wrappers for Git's own functions. Their behavior should be tested for by Git. I am planning to wrap these in try catch statements, which will output the system2 warning/error for the user, as well as stop the function and return an error message further specifying the error to the user. These wrappers currently all generally error the same way, by outputting a system2 warning, but not actually erroring and stopping the code. This occurs when they are given any invalid  by the user, which breaks the command built in system2.**

## Git Add
**Path Tests**
* `”testthat when git_add is given valid .git/, worktree path, and file path, then git add is performed”` 
* `”testthat when git_add is not given .git/, then an error will be raised”` 

## Git Commit
**Path Tests**
* `”testthat when git_commit is given valid .git/, worktree path, commit message, author, and email, then git commit is performed”` 
* `”testthat when git_commit is not given .git/, then an error will be raised”`
    
## Git Init
**Path Tests**
* `”testthat when git_init is given a valid folder path for a git repo to be initialized, then git init is performed”` 
* `”testthat when git_init is not given a valid folder path for a git repo to be initialized, then it returns an error”` 

## Git mv
**Path Tests**
* `”testthat when git_mv is given valid .git/, worktree path, an old name, and a new name, then git mv is performed”` 
* `”testthat when git_mv is not given .git/, then an error will be raised”`

## Git head
**Path Tests**
* `”testthat when git_head is given a valid .git/, then git head is performed”` 
* `”testthat when git_head is not given .git/, then an error will be raised”`

## Git log
**Path Tests**
* `”testthat when git_log is given a valid .git/, flags, and path to save the file, then git log is performed”` 
* `”testthat when git_log is not given .git/, then an error will be raised”`

## Git blame
**Path Tests**
* `”testthat when git_blame is given valid ./git, flags, commit hash, and path to a file to blame, then git blame is performed”` 
* `”testthat when git_blame is not given .git/, then an error will be raised”`
* `”testthat when git_blame is given a file deleted by the blamed commit, then NULL is returned”` 

**Git log and git blame currently already have try catch statements that I will look into. They seem to return NULL as opposed to a function warning/error, which gives the user less information about what actually went wrong. The commit that is here https://github.com/sailuh/kaiaulu/commit/67ff85fbc13728c635fb8b143488fa0ec4f5b444#diff-e8458e36c0755109d2e986623d2229882eab41bbfd1312f742d999b674e1d583 and it looks like the intended behavior for the try catch was commented out. I will uncomment it and see if anything breaks.**

## Git Create Sample Log
## Git Delete Sample Log
**Both serve a function similar to R/example.R, and so don’t need unit tests. They are immutable.**

# Transforms
## Gitlog to bipartite
**Path Tests**
* `”testthat when transform_gitlog_to_bipartite_network is not given a valid parsed git project, then an error will be raised”`
* `”testthat when transform_gitlog_to_bipartite_network is not given a valid mode, then an error will be raised”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and mode, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and mode, then the number of commit nodes is the same as the number of unique commit hashes in the git log"`
* `"testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and mode, then the number of file nodes is the same as the number of unique files in the git log"`
* `"testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and author-file mode, then the number of edges is the same as the number of author to file interactions"`
* `"testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and committer-file mode, then the number of edges is the same as the number of committer to file interactions"`
* `"testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and commit-file mode, then the number of edges is the same as the number of commit_hash to file interactions"`
* `"testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and author-committer mode, then the number of edges is the same as the number of author to committer interactions"`
* `”testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and author-file mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and committer-file mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and commit-file mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_bipartite_network is given a valid parsed git project and author-committer mode, then edge weights for all edges are calculated correctly”`

## Gitlog to entity bipartite
**Path Tests**
* `”testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project, then an error will be raised”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given invalid mode, then an error will be raised”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and mode, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and mode, then the number of committer nodes is the same as the number of unique committers in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and mode, then the number of commit nodes is the same as the number of unique commit_hashes in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and mode, then the number of entity nodes is the same as the number of unique entities in the git log"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and  author-entity mode, then the number of edges is the same as the number of author to entity interactions"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and  committer-entity mode, then the number of edges is the same as the number of committer to entity interactions"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and commit-entity mode, then the number of edges is the same as the number of commit_hash to entity interactions"`
* `"testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and author-committer mode, then the number of edges is the same as the number of author to committer interactions"`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and author-entity mode, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and committer-entity mode, then edge weights for all edges are are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and commit-entity mode, then edge weights for all edges are are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_bipartite_network is given a valid parsed entity git project and author-committer mode, then edge weights for all edges are are calculated correctly”`

## Gitlog to temporal
**Path Tests**
* `”testthat when transform_gitlog_to_temporal_network is given an invalid parsed git project, then an error will be raised”`
* `”testthat when transform_gitlog_to_temporal_network is given invalid mode, then an error will be raised”`
* `”testthat when transform_gitlog_to_temporal_network is given invalid time lag, then an error will be raised”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_temporal_network is given valid parsed git project, mode, and time lag, then a nodes data table is returned"`
* `"testthat when transform_gitlog_to_temporal_network is given valid parsed git project, mode, and time lag, then an edges data table is returned"`
* `"testthat when transform_gitlog_to_temporal_network is given valid parsed git project, author mode, and time lag, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_temporal_network is given valid parsed git project, committer mode, and time lag, then the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat when transform_gitlog_to_temporal_network is given valid parsed git project, mode, and time lag, then one_lag correctly outputs edges from only the current and previous time steps"`
* `"testthat transform_gitlog_to_temporal_network is given valid parsed git project, mode, and time lag, then all_lag correctly outputs the edges from all time steps"`
* `”testthat when transform_gitlog_to_temporal_network is given valid parsed git project, mode, and one_lag, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_temporal_network is given valid parsed git project, mode, and no_lag, then edge weights for all edges are calculated correctly”`

## Gitlog to entity temporal
**Path Tests**
* `”testthat when transform_gitlog_to_entity_temporal_network is given an invalid parsed entity git project, then an error will be raised”`
* `”testthat when transform_gitlog_to_entity_temporal_network is given invalid mode, then an error will be raised”`
* `”testthat when transform_gitlog_to_entity_temporal_network is given invalid time lag, then an error will be raised”`

**Behavioral Tests**
* `"testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, mode, and time lag, then a nodes data table is returned"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, mode, and time lag, then an edgelist data table is returned"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, author mode, and time lag, then the number of author nodes is the same as the number of unique authors that changed files in the git log"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, committer mode, and time lag, then the number of committer nodes is the same as the number of unique committers that changed files in the git log"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, mode, and time lag, then one_lag correctly outputs edges from the current and previous time steps"`
* `"testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, mode, and time lag, then all_lag correctly outputs edges from the all time steps"`
* `”testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, mode, and one_lag, then edge weights for all edges are calculated correctly”`
* `”testthat when transform_gitlog_to_entity_temporal_network is given a valid parsed entity git project, mode, and no_lag, then edge weights for all edges are calculated correctly”`

## Commit message ID to network
**Path Tests**
* `”testthat when transform_commit_message_id is given invalid parsed git project, then an error will be raised”`
* `”testthat when transform_commit_message_id is given invalid Regex, then an error will be raised”`

**Behavioral Tests**
* `"testthat when transform_commit_message_id is given valid parsed git project and Regex, then a nodes data table is returned"`
* `"testthat when transform_commit_message_id is given valid parsed git project and Regex, then an edgelist data table is returned"`
* `"testthat when transform_commit_message_id is given valid parsed git project and Regex, then the number of commit ID nodes is the same as the number of commit hashes in the git log"`
* `"testthat when transform_commit_message_id is given valid parsed git project and Regex, then the number of file nodes is the same as the number of unique files in the git log"`
* `”testthat when transform_commit_message_id is given valid parsed git project and Regex, then Regex correctly identifies the correct commit message ids”` 
* `”testthat when transform_commit_message_id is given valid parsed git project and Regex, then all edges correctly flow from commits to their respective files are correct”`
* `”testthat when transform_commit_message_id is given valid parsed git project and Regex, then all edge weights are calculated correctly”`



