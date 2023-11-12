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

  #git_graph <- transform_gitlog_to_bipartite_network(project_git,"author-file")
  git_graph <- copy(project_git)
  setnames(git_graph,
           old = c("author_name_email",
                   "file_pathname"),
           new = c("from",
                   "to"))
  git_graph <- model_directed_graph(git_graph,is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = FALSE)
  git_graph[["edgelist"]]$weight <- git_graph[["edgelist"]]$lines_added + git_graph[["edgelist"]]$lines_removed

  temporal_projection <- temporal_one_lag_graph_projection(git_graph,mode=TRUE,timestamp_column ="author_datetimetz",
                                                   weight_scheme_function = weight_scheme_sum_edges)

  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 1"]$weight, (1+3) + (5+7))
})


test_that("one time lag temporal projection of dev 2 changing dev 1's file twice has weight 2", {

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

  #git_graph <- transform_gitlog_to_bipartite_network(project_git,"author-file")
  git_graph <- copy(project_git)
  setnames(git_graph,
           old = c("author_name_email",
                   "file_pathname"),
           new = c("from",
                   "to"))
  git_graph <- model_directed_graph(git_graph,is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = FALSE)
  git_graph[["edgelist"]]$weight <- git_graph[["edgelist"]]$lines_added + git_graph[["edgelist"]]$lines_removed

  temporal_projection <- temporal_one_lag_graph_projection(git_graph,mode=TRUE,timestamp_column ="author_datetimetz",
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

  #git_graph <- transform_gitlog_to_bipartite_network(project_git,"author-file")
  git_graph <- copy(project_git)
  setnames(git_graph,
           old = c("author_name_email",
                   "file_pathname"),
           new = c("from",
                   "to"))
  git_graph <- model_directed_graph(git_graph,is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = FALSE)
  git_graph[["edgelist"]]$weight <- git_graph[["edgelist"]]$lines_added + git_graph[["edgelist"]]$lines_removed

  temporal_projection <- temporal_all_lag_graph_projection(git_graph,mode=TRUE,timestamp_column ="author_datetimetz",
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


  #git_graph <- transform_gitlog_to_bipartite_network(project_git,"author-file")
  git_graph <- copy(project_git)
  setnames(git_graph,
           old = c("author_name_email",
                   "file_pathname"),
           new = c("from",
                   "to"))
  git_graph <- model_directed_graph(git_graph,is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = FALSE)
  git_graph[["edgelist"]]$weight <- git_graph[["edgelist"]]$lines_added + git_graph[["edgelist"]]$lines_removed

  temporal_projection <- temporal_all_lag_graph_projection(git_graph,mode=TRUE,timestamp_column ="author_datetimetz",
                                                          weight_scheme_function = weight_scheme_cum_temporal)

  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 1"]$weight, 1+3+5+7)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1" & to == "dev 2"]$weight, 3+5)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1" & to == "dev 1"]$weight, 1+5)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 2" & to == "dev 2"]$weight, 3+7)
})

