test_that("weighted churn temporal projection of dev 2 changing dev 1's file is accurate", {

  timestamps <-  as.POSIXct(c("Tue Aug 17 15:59:33 1999 +0000","Tue Aug 17 16:59:33 1999 +0000",
                              "Tue Aug 17 17:59:33 1999 +0000","Tue Aug 17 18:59:33 1999 +0000",
                              "Tue Aug 17 19:59:33 1999 +0000"),
                            format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


  project_git <- data.table(file_pathname = c("file_a","file_a","file_a","file_a","file_a"),
                            author_name_email = c("dev 1","dev 1","dev 2","dev 1","dev 2"),
                            committer_name_email = c("dev 1","dev 1","dev 2","dev 1","dev 2"),
                            author_datetimetz = timestamps,
                            committer_datetimetz = timestamps,
                            lines_added = c(1,1,3,5,7),
                            lines_removed = c(0,0,0,0,0))

  project_git$weight <- project_git$lines_added + project_git$lines_removed

  temporal_projection <- transform_gitlog_to_temporal_network(project_git,
                                                              mode = "author",
                                                              lag = "one_lag",
                                                              weight_scheme_function = weight_scheme_sum_edges)


  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 1"]$weight, (1+3) + (5+7))
})


test_that("one time lag temporal projection of dev 2 changing dev 1's file twice has weight schme delete count of 2", {

  timestamps <-  as.POSIXct(c("Tue Aug 17 15:59:33 1999 +0000","Tue Aug 17 16:59:33 1999 +0000",
                              "Tue Aug 17 17:59:33 1999 +0000","Tue Aug 17 18:59:33 1999 +0000",
                              "Tue Aug 17 19:59:33 1999 +0000"),
                            format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


  project_git <- data.table(file_pathname = c("file_a","file_a","file_a","file_a","file_a"),
                            author_name_email = c("dev 1","dev 1","dev 2","dev 1","dev 2"),
                            committer_name_email = c("dev 1","dev 1","dev 2","dev 1","dev 2"),
                            author_datetimetz = timestamps,
                            committer_datetimetz = timestamps,
                            lines_added = c(1,1,3,5,7),
                            lines_removed = c(0,0,0,0,0))

  project_git$weight <- project_git$lines_added + project_git$lines_removed

  temporal_projection <- transform_gitlog_to_temporal_network(project_git,
                                                              mode = "author",
                                                              lag = "one_lag",
                                                              weight_scheme_function = weight_scheme_count_deleted_nodes)

  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 1"]$weight, 1+1)
})

test_that("all time lag temporal projection matches original formulation", {

  timestamps <-  as.POSIXct(c("Tue Aug 17 15:59:33 1999 +0000","Tue Aug 17 16:59:33 1999 +0000",
                               "Tue Aug 17 17:59:33 1999 +0000","Tue Aug 17 18:59:33 1999 +0000"),
                             format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


  project_git <- data.table(file_pathname = c("file_a","file_a","file_a","file_a"),
                             author_name_email = c("dev 1","dev 2","dev 2","dev 3"),
                             committer_name_email = c("dev 1","dev 2","dev 2","dev 3"),
                             author_datetimetz = timestamps,
                             committer_datetimetz = timestamps,
                             lines_added = c(4,2,3,2),
                             lines_removed = c(0,0,0,0))

  project_git$weight <- project_git$lines_added + project_git$lines_removed

  temporal_projection <- transform_gitlog_to_temporal_network(project_git,
                                                              mode = "author",
                                                              lag = "all_lag",
                                                              weight_scheme_function = weight_scheme_cum_temporal)

  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 1"]$weight, 4+2+3)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 3" & to == "dev 1"]$weight, 4+2)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 3" & to == "dev 2"]$weight, 2+3+2)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 2"]$weight, 2+3)
})

test_that("all time lag temporal projection matches original formulation with alternating authors", {

  timestamps <-  as.POSIXct(c("Tue Aug 17 15:59:33 1999 +0000","Tue Aug 17 16:59:33 1999 +0000",
                              "Tue Aug 17 17:59:33 1999 +0000","Tue Aug 17 18:59:33 1999 +0000"),
                            format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


  project_git <- data.table(file_pathname = c("file_a","file_a","file_a","file_a"),
                            author_name_email = c("dev 1","dev 2","dev 1","dev 2"),
                            committer_name_email = c("dev 1","dev 2","dev 1","dev 2"),
                            author_datetimetz = timestamps,
                            committer_datetimetz = timestamps,
                            lines_added = c(1,3,5,7),
                            lines_removed = c(0,0,0,0))



  project_git$weight <- project_git$lines_added + project_git$lines_removed
  temporal_projection <- transform_gitlog_to_temporal_network(project_git,
                                                              mode = "author",
                                                              lag = "all_lag",
                                                              weight_scheme_function = weight_scheme_cum_temporal)

  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 1"]$weight, 1+3+5+7)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1" & to == "dev 2"]$weight, 3+5)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1" & to == "dev 1"]$weight, 1+5)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 2"]$weight, 3+7)
})

test_that("all time lag temporal projection correctly assign weights on distinct file contributions", {

  timestamps <-  as.POSIXct(c("Tue Aug 17 14:59:33 1999 +0000",
                              "Tue Aug 17 15:59:33 1999 +0000","Tue Aug 17 16:59:33 1999 +0000",
                              "Tue Aug 17 17:59:33 1999 +0000","Tue Aug 17 18:59:33 1999 +0000"),
                            format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


  project_git <- data.table(file_pathname = c("file_a","file_a","file_a","file_b","file_b"),
                            author_name_email = c("dev 1","dev 2","dev 1","dev 1","dev 2"),
                            committer_name_email = c("dev 1","dev 2","dev 1","dev 1","dev 2"),
                            author_datetimetz = timestamps,
                            committer_datetimetz = timestamps,
                            lines_added = c(1,3,20,5,7),
                            lines_removed = c(0,0,0,0,0))

  project_git$weight <- project_git$lines_added + project_git$lines_removed

  temporal_projection <- transform_gitlog_to_temporal_network(project_git,
                                                              mode = "author",
                                                              lag = "all_lag",
                                                              weight_scheme_function = weight_scheme_cum_temporal)

  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 1"]$weight, (1+3) + (7+5))
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1" & to == "dev 1"]$weight, (20+1))
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1" & to == "dev 2"]$weight, 3+20)
})

test_that("temporal projections with only one author changing a file returns an empty table", {

  # Note this is a special case where auto loops are not reported, as there are no collaborations
  # in the first place.

  timestamps <-  as.POSIXct(c("Tue Aug 17 15:59:33 1999 +0000","Tue Aug 17 16:59:33 1999 +0000",
                              "Tue Aug 17 17:59:33 1999 +0000","Tue Aug 17 18:59:33 1999 +0000"),
                            format = "%a %b %d %H:%M:%S %Y %z", tz = "UTC")


  project_git <- data.table(file_pathname = c("file_a","file_a","file_a","file_a"),
                            author_name_email = c("dev 1","dev 1","dev 1","dev 1"),
                            committer_name_email = c("dev 1","dev 1","dev 1","dev 1"),
                            author_datetimetz = timestamps,
                            committer_datetimetz = timestamps,
                            lines_added = c(1,3,5,7),
                            lines_removed = c(0,0,0,0))

  git_graph <- copy(project_git)


  temporal_projection <- transform_gitlog_to_temporal_network(git_graph,
                                       mode = "author",
                                       lag = "all_lag",
                                       weight_scheme_function = weight_scheme_cum_temporal)


  expect_equal(nrow(temporal_projection[["edgelist"]]), 0)
})
