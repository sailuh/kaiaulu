# Two modules of two authors each, connected only through dev 2 and dev 3
nodes <- data.table(name = c("dev 1","dev 2","dev 3","dev 4"))
edgelist <- data.table(from = c("dev 1","dev 2","dev 3"),
                       to = c("dev 2","dev 3","dev 4"),
                       weight = c(1,1,1))
graph <- list(nodes = nodes, edgelist = edgelist)

test_that("authors that are the only link between two modules in a layer are found", {

  cluster_table <- data.table(file_path = c("dev 1","dev 2","dev 3","dev 4"),
                              module = c("M0","M0","M1","M1"),
                              layer = c("L0","L0","L0","L0"))

  brokers <- smell_radio_silence_by_layer(graph, cluster_table)

  expect_equal(sort(brokers$boundary_spanner), c("dev 2","dev 3"))
  expect_equal(unique(brokers$layer), "L0")
})

test_that("a layer with a single module besides Isolated is skipped", {

  cluster_table <- data.table(file_path = c("dev 1","dev 2","dev 3","dev 4"),
                              module = c("M0","M0","M0","Isolated"),
                              layer = c("L0","L0","L0","L0"))

  brokers <- smell_radio_silence_by_layer(graph, cluster_table)

  expect_equal(nrow(brokers), 0)
})

test_that("each layer is reported on its own", {

  cluster_table <- data.table(file_path = c("dev 1","dev 2","dev 3","dev 4",
                                            "dev 1","dev 2","dev 3","dev 4"),
                              module = c("M0","M0","M1","M1",
                                         "M0","M0","M0","M0"),
                              layer = c("L0","L0","L0","L0",
                                        "L1","L1","L1","L1"))

  brokers <- smell_radio_silence_by_layer(graph, cluster_table)

  expect_equal(unique(brokers$layer), "L0")
})
