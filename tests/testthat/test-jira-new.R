test_that("make_jira_issue creates an issue", {
  folder_path <- io_make_folder(folder_path = "~/Documents/test_json", folder_name = "make_issue")

  issue <- make_jira_issue_new(jira_domain_url = "https://project.org/jira",
                               issue_key = "GERONIMO-2",
                               issue_type = "Bug",
                               status = "In Progress",
                               resolution = "Incomplete",
                               title = "Bug fixes need implementation.",
                               description = "The new features have not been implemented.",
                               components = "x-core",
                               creator_name = "Moe",
                               reporter_name = "Larry",
                               assignee_name = "Curly",
                               comments = c(
                                 "This is the first body comment.",
                                 "This is the second body comment."
                               )
  )

  issues <- list(issue)
  make_jira_issue_tracker_new(issues,
                              save_filepath=file.path(folder_path,"issue2.json"))
})
