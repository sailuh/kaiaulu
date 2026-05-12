tools_path <- test_path("testdata", "tools.yml")
tool <- yaml::read_yaml(tools_path)
tmp_folderpath <- tool[["tmp"]]

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

test_that("Parsing git log function entities on R function declarations returns correct weight", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_function_in_files(folder_path = tmp_folderpath,
                                                      folder_name = "example_function_in_files")

  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)

  io_delete_folder(folder_path=tmp_folderpath, "example_function_in_files")

  temporal_projection <- transform_gitlog_to_entity_temporal_network(result,
                                                                     mode = "author",
                                                                     lag = "all_lag",
                                                                     weight_scheme_function = weight_scheme_cum_temporal)



  expect_equal(temporal_projection[["edgelist"]][from == "Author 2 <author2@email.com>" &
                                                   to == "Author 1 <author1@email.com>"]$weight, (3+1))

})

test_that("Parsing git log function entities on alternating devs changing the same function returns correct weight", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_notebook_alternating_function_in_files(folder_path = tmp_folderpath,
                                                      folder_name = "example_alternating_devs")

  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)

  io_delete_folder(folder_path=tmp_folderpath, "example_alternating_devs")

  temporal_projection <- transform_gitlog_to_entity_temporal_network(result,
                                                                     mode = "author",
                                                                     lag = "all_lag",
                                                                     weight_scheme_function = weight_scheme_cum_temporal)



  expect_equal(temporal_projection[["edgelist"]][from == "dev 2 <>" & to == "dev 1 <>"]$weight, 1+3+5+7)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1 <>" & to == "dev 2 <>"]$weight, 3+5)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1 <>" & to == "dev 1 <>"]$weight, 1+5)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 2 <>" & to == "dev 2 <>"]$weight, 3+7)

})


test_that("Check Pair Wise Cumulative Temporal Sum reflects Codeface actual implementation", {
  tools_path <- file.path(tools_path)
  tool <- yaml::read_yaml(tools_path)
  perceval_path <- tool[["perceval"]]
  utags_path <- tool[["utags"]]
  git_repo_path <- example_notebook_alternating_function_in_files(folder_path = tmp_folderpath,
                                                                  folder_name = "example_alternating_devs")

  project_git <- parse_gitlog(perceval_path, git_repo_path)
  result <- parse_gitlog_entity(git_repo_path=git_repo_path,
                                utags_path = utags_path,
                                project_git_log = project_git,
                                kinds=list( r=c('f')),
                                progress_bar = FALSE)

  io_delete_folder(folder_path=tmp_folderpath, "example_alternating_devs")

  temporal_projection <- transform_gitlog_to_entity_temporal_network(result,
                                                                     mode = "author",
                                                                     lag = "all_lag",
                                                                     weight_scheme_function = weight_scheme_pairwise_cum_temporal)



  # (c4+c1) + (c4 +c3) + (c2 + c1)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 2 <>" & to == "dev 1 <>"]$weight, (7+1) + (7+5) + (3+1))
  # (c3 + c2)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1 <>" & to == "dev 2 <>"]$weight, 3+5)
  # (c3 + c1)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 1 <>" & to == "dev 1 <>"]$weight, 5+1)
  # (c4 + c2)
  expect_equal(temporal_projection[["edgelist"]][from == "dev 2 <>" & to == "dev 2 <>"]$weight, 7+3)

})

########### Model Directed Graph ############

test_that("When model_directed_graph is given a valid edgelist, color, then node and edgelist tables are returned", {
  dt <- data.table(
    from = c(
      "Node 1",
      "Node 2"
    ),
    to = c(
      "Node 1",
      "Node 2")
  )
  graph <- model_directed_graph(dt, is_bipartite = FALSE, color = c("black","#f4dbb5"), aggregate_duplicate = FALSE)

  expect_is(graph$nodes, "data.table")
  expect_is(graph$edgelist, "data.table")

})

test_that("When model_directed_graph is given a valid edgelist, color, and aggregate_duplicate is set to TRUE, the function generates edge weights", {
  dt <- data.table(
    from = c(
      "Node 1",
      "Node 1",
      "Node 2"
    ),
    to = c(
      "Node 1",
      "Node 1",
      "Node 2")
  )
  graph <- model_directed_graph(dt, is_bipartite = FALSE, color = c("black","#f4dbb5"), aggregate_duplicate = TRUE)

  expect_equal(graph$edgelist$weight[1], 2)
})

test_that("When model_directed_graph is given valid edgelist with one node and one edge connecting it to itself, it returns the same one node one edge connnection", {
  dt <- data.table(
    from = c(
      "Node 1")
    ,
    to = c(
      "Node 1")
    )

  graph <- model_directed_graph(dt, is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = TRUE)

  expect_equal(graph$node$name[1], "Node 1")
  expect_equal(graph$edgelist$from[1], "Node 1")
  expect_equal(graph$edgelist$to[1], "Node 1")
})

test_that("When model_directed_graph is given a valid edgelist with one edge connecting two nodes and takes a normal graph, it returns two nodes and one edge", {
  dt <- data.table(
    from = c(
      "Node 1")
    ,
    to = c(
      "Node 2")
  )

  graph <- model_directed_graph(dt, is_bipartite = FALSE, color = c("black","#f4dbb5"), aggregate_duplicate = TRUE)

  expect_equal(graph$node$name[1], "Node 1")
  expect_equal(graph$node$name[2], "Node 2")
  expect_equal(graph$edgelist$from[1], "Node 1")
  expect_equal(graph$edgelist$to[1], "Node 2")
})

test_that("When model_directed_graph is given a valid edgelist with one edge connecting two nodes and takes a bipartite graph, it returns two nodes and one edge", {
  dt <- data.table(
    from = c(
      "Node 1")
    ,
    to = c(
      "Node 2")
  )

  graph <- model_directed_graph(dt, is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = TRUE)

  expect_equal(graph$node$name[1], "Node 1")
  expect_equal(graph$node$name[2], "Node 2")
  expect_equal(graph$edgelist$from[1], "Node 1")
  expect_equal(graph$edgelist$to[1], "Node 2")
})

test_that("When model_directed_graph is given a valid edgelist with two disconnected edges and takes a normal graph, it returns four nodes and two edges", {
  # Creates a disconnected graph
  dt <- data.table(
    from = c(
      "Node 1",
      "Node 3")
    ,
    to = c(
      "Node 2",
      "Node 4")
  )

  graph <- model_directed_graph(dt, is_bipartite = FALSE, color = c("black","#f4dbb5"), aggregate_duplicate = TRUE)

  expect_equal(graph$node$name, c("Node 1","Node 3", "Node 2", "Node 4"))
  expect_equal(graph$edgelist$from, c("Node 1", "Node 3"))
  expect_equal(graph$edgelist$to, c("Node 2", "Node 4"))
})


test_that("When model_directed_graph is given a valid edgelist with two disconnected edges and takes a bipartite graph, it returns four nodes and two edges", {
  # Creates a disconnected bipartite graph
  dt <- data.table(
    from = c(
      "Node 1",
      "Node 3")
    ,
    to = c(
      "Node 2",
      "Node 4")
  )

  graph <- model_directed_graph(dt, is_bipartite = TRUE, color = c("black","#f4dbb5"), aggregate_duplicate = TRUE)

  expect_equal(graph$node$name, c("Node 1","Node 3", "Node 2", "Node 4"))
  expect_equal(graph$edgelist$from, c("Node 1", "Node 3"))
  expect_equal(graph$edgelist$to, c("Node 2", "Node 4"))
})

########### Bipartite Graph Projection ############

test_that("When bipartite_graph_projection is given a valid graph, mode, and weight_scheme_function, then node and edgelist data tables are returned", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "Author 2", "File 1"),
    type = c(TRUE, TRUE, FALSE),
    color = c("black", "black", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1", "Author 2"),
    to   = c("File 1", "File 1"),
    weight = c(1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = NULL)

  expect_is(bigraph$nodes, "data.table")
  expect_is(bigraph$edgelist, "data.table")
})

test_that("When bipartite_graph_projection is given a bipartite graph with author nodes connecting to file nodes and mode is set to TRUE, then the function will return only author nodes", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "Author 2", "File 1"),
    type = c(TRUE, TRUE, FALSE),
    color = c("black", "black", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1", "Author 2"),
    to   = c("File 1", "File 1"),
    weight = c(1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = NULL)

  expect_equal(bigraph$nodes$name[1], "Author 1")
  expect_equal(bigraph$nodes$name[2], "Author 2")
})

test_that("When bipartite_graph_projection is given a bipartite graph with author nodes connecting to file nodes and mode is set to FALSE, then the function will return only author nodes", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "File 1", "File 2"),
    type = c(TRUE, FALSE, FALSE),
    color = c("black", "#f4dbb5", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1", "Author 1"),
    to   = c("File 1", "File 2"),
    weight = c(1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, FALSE, weight_scheme_function = NULL)

  expect_equal(bigraph$nodes$name[1], "File 1")
  expect_equal(bigraph$nodes$name[2], "File 2")
})

test_that("When bipartite_graph_projection is given a bipartite graph with three author nodes connecting to one file node and mode is set to TRUE, then there are edges created between all three authors", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "Author 2", "Author 3","File 1"),
    type = c(TRUE, TRUE, TRUE, FALSE),
    color = c("black", "black", "black", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1", "Author 2", "Author 3"),
    to   = c("File 1", "File 1", "File 1"),
    weight = c(1L, 1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = NULL)

  expect_equal(bigraph$edgelist$to_projection, c("Author 2", "Author 3", "Author 3"))
  expect_equal(bigraph$edgelist$from_projection, c("Author 1", "Author 1", "Author 2"))
})

test_that("When bipartite_graph_projection is given a bipartite graph with Author 1 having two connections to File 1 and Author 2 having one connection to File 1, and weight_scheme_function = weight_scheme_sum_edges, and mode = TRUE, then their returned edge is 4", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "Author 2","File 1"),
    type = c(TRUE, TRUE, FALSE),
    color = c("black", "black", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1", "Author 1", "Author 2"),
    to   = c("File 1", "File 1", "File 1"),
    weight = c(1L, 1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = weight_scheme_sum_edges)

  expect_equal(bigraph$edgelist$weight[1], 4)
})

test_that("When bipartite_graph_projection is given a bipartite graph with Author 1 having one connection to File 1, File 2, and File 3, and Author 2 having one connection to File 1, File 2, and File 3, and weight_scheme_function = weight_scheme_count_deleted_nodes, and mode = TRUE, then their returned edge is 3", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "Author 2", "File 1", "File 2", "File 3"),
    type = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    color = c("black", "black", "#f4dbb5", "#f4dbb5", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1", "Author 1", "Author 1", "Author 2", "Author 2", "Author 2"),
    to   = c("File 1", "File 2", "File 3", "File 1", "File 2", "File 3"),
    weight = c(1L, 1L, 1L, 1L, 1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = weight_scheme_count_deleted_nodes)

  expect_equal(bigraph$edgelist$weight[1], 3)
})

test_that("When bipartite_graph_projection is given two bipartite graphs with each containing one author node connecting to a file node and mode is set to TRUE, then the function will return the correctly projected authors with no edges", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "Author 2", "File 1", "File 2"),
    type = c(TRUE, TRUE, FALSE, FALSE),
    color = c("black", "black", "#f4dbb5", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1", "Author 2"),
    to   = c("File 1", "File 2"),
    weight = c(1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = NULL)

  expect_equal(bigraph$nodes$name, c("Author 1", "Author 2"))
  expect_equal(bigraph$edgelist$to_projection, character(0))
  expect_equal(bigraph$edgelist$from_projection, character(0))
})

test_that("When bipartite_graph_projection is given a graph with one edge connecting an author and file node, and mode is set to TRUE then only a singular author node is returned", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1", "File 1"),
    type = c(TRUE, FALSE),
    color = c("black", "#f4dbb5")
  )
  edgelist <- data.table(
    from = c("Author 1"),
    to   = c("File 1"),
    weight = c(1L, 1L)
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = NULL)

  expect_equal(bigraph$nodes$name, c("Author 1"))
})

test_that("When bipartite_graph_projection is given a bipartite graph with one author node with no edges and mode is set to TRUE, then the function will return the author node", {
  # Create bipartite graph data
  nodes <- data.table(
    name = c("Author 1"),
    type = c(TRUE),
    color = c("black")
  )
  edgelist <- data.table(
    from = c(""),
    to   = c(""),
    weight = c("")
  )
  # Combine into graph
  dt <- list(
    nodes = nodes,
    edgelist = edgelist
  )

  bigraph <- bipartite_graph_projection(dt, TRUE, weight_scheme_function = NULL)

  expect_equal(bigraph$nodes$name, c("Author 1"))
})

