parse_gitlog <- function(perceval_path,git_repo_path){
  # Remove ".git"
  git_uri <- str_split(git_repo_path,pattern=".git")[[1]][1]
  # The log will be saved to the /tmp/ folder
  gitlog_path <- "/tmp/gitlog.log"
  # Execute shell command to extract gitlog using Percerval recommended format (See it's README.md.
  perceval_output <- system2("git",
                             args = c('--git-dir',
                                      git_repo_path,
                                      'log',
                                      '--raw',
                                      '--numstat',
                                      '--pretty=fuller',
                                      '--decorate=full',
                                      '--parents',
                                      '--reverse',
                                      '--topo-order',
                                      '-M',
                                      '-C',
                                      '-c',
                                      '--remotes=origin',
                                      '--all',
                                      '>' ,
                                      gitlog_path),
                             stdout = TRUE,
                             stderr = FALSE)
  # Use percerval to parse gitlog_path. --json line is required to be parsed by jsonlite::fromJSON.
  perceval_output <- system2(perceval_path,
                             args = c('git', '--git-log',gitlog_path,git_uri,'--json-line'),
                             stdout = TRUE,
                             stderr = FALSE)
  # Return the parsed JSON output.
  return(jsonlite::stream_in(textConnection(perceval_output)))
  #return(jsonlite::read_json(perceval_output))
}
