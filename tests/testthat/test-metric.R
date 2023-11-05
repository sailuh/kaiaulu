jira_issues <- data.table(issue_status=c("Closed","Open","Closed"),
                          issue_type=c("Bug","Bug","Feature"),
                          issue_key=c("ISSUE-1","ISSUE-2","ISSUE-3"))

project_git <- data.table(file_pathname = c("file_a","file_a","file_a","file_a"),
                          commit_message_id = c("ISSUE-1","ISSUE-1","ISSUE-2","ISSUE-3"),
                          lines_added = c(1,3,5,7),
                          lines_removed = c(0,0,0,0))

test_that("file bug frequency counts a file's number of commits linked to closed bugs", {
  file_bug_frequency <- metric_file_bug_frequency(project_git,jira_issues)

  # ISSUE-1 and ISSUE-2
  expect_equal(file_bug_frequency[file_pathname == "file_a"]$file_bug_frequency,2)
})

test_that("file non bug frequency counts a file's number of commits linked to closed non-bugs", {
  file_non_bug_frequency <- metric_file_non_bug_frequency(project_git,jira_issues)

  # ISSUE-3
  expect_equal(file_non_bug_frequency[file_pathname == "file_a"]$file_non_bug_frequency,1)
})

test_that("file non bug churn sums a file's churn linked to closed non-bugs", {
  file_non_bug_churn <- metric_file_non_bug_churn(project_git,jira_issues)

  # ISSUE 1 and ISSUE 2
  expect_equal(file_non_bug_churn[file_pathname == "file_a"]$file_non_bug_churn,7)
})

test_that("file bug churn sums a file's churn linked to closed bugs", {
  file_bug_churn <- metric_file_bug_churn(project_git,jira_issues)
  expect_equal(file_bug_churn[file_pathname == "file_a"]$file_bug_churn,1+3)
})


test_that("file churn is greather than the sum of file churn + file non bug churn if commits include open issues", {


  project_git_churn <- metric_churn_per_commit_per_file(project_git)

  file_bug_churn <- metric_file_bug_churn(project_git,jira_issues)
  file_non_bug_churn <- metric_file_non_bug_churn(project_git,jira_issues)
  file_churn <- metric_file_churn(project_git)

  file_a_bug_churn <- file_bug_churn[file_pathname == "file_a"]$file_bug_churn
  file_a_non_bug_churn <- file_non_bug_churn[file_pathname == "file_a"]$file_non_bug_churn
  file_a_churn <- file_churn[file_pathname == "file_a"]$file_churn

  # ISSUE-4 is counted in file_churn, but ISSUE-4 is not counted in neither bug_churn or non_bug_churn
  expect_gt(file_a_churn,file_a_bug_churn + file_a_non_bug_churn)

})

