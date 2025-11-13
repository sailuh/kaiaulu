test_that("github issue without description can be parsed as NA", {

  # Generate synthetic GitHub issue
  github_json_path <- example_github_issue_no_description("/tmp", "github_issue_no_desc")

  # Read JSON
  all_issue <- lapply(list.files(github_json_path, 
                                full.names = TRUE), jsonlite::read_json)

  # Bind as data.table 
  all_issue <- lapply(all_issue,
                        github_parse_project_issue)  

  all_issue <- rbindlist(all_issue,fill=TRUE)
  
  expect_true(is.na(all_issue$body[1]))
  
  if (dir.exists(file.path("/tmp", "github_issue_no_desc"))) {
    io_delete_folder("/tmp", "github_issue_no_desc")
  }
})
