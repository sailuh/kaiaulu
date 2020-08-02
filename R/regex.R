regex_git_blame_commit_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^([a-f0-9]{40}) (\\d+) (\\d+)")
}
regex_git_blame_author_name_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^author (.*)")
}
regex_git_blame_author_email_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^author-mail (.*)")
}
regex_git_blame_author_time_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^author-time (.*)")
}
regex_git_blame_author_tz_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^author-tz (.*)")
}
regex_git_blame_committer_name_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^committer (.*)")
}
regex_git_blame_committer_email_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^committer-mail (.*)")
}
regex_git_blame_committer_time_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^committer-time (.*)")
}
regex_git_blame_committer_tz_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^committer-tz (.*)")
}
regex_git_blame_summary_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^summary (.*)")
}
regex_git_blame_previous_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^previous ([a-f0-9]{40}) (.*)")
}
regex_git_blame_filename_line <- function(lines_content){
  stri_match_first_regex(lines_content,
                         "^filename (.*)")
}
