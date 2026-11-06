# Uses the project, analysis and module files shipped with Kaiaulu.
conf_dir <- test_path("..", "..", "conf")
module_dir <- test_path("..", "..", "inst", "modules")

test_that("load_config combines project and analysis files", {
  conf <- load_config("geronimo", "collab_network", conf_dir = conf_dir)
  expect_equal(get_file_extensions(conf), "java")
  expect_equal(get_substring_filepath(conf), "test")
  expect_equal(get_filter_commit_size(conf), 30)
  expect_equal(get_period(conf), list(start = "2009-01-01", end = "2010-01-01"))
})

test_that("load_config applies overrides", {
  conf <- load_config("geronimo", "collab_network", conf_dir = conf_dir,
                      overrides = list(period = "late"))
  expect_equal(get_period(conf)$start, "2013-05-01")
})

test_that("load_config stops on a missing file", {
  expect_error(load_config("no_such_project", conf_dir = conf_dir))
  expect_error(load_config("geronimo", "no_such_analysis", conf_dir = conf_dir))
})

test_that("get_period warns on an undefined period", {
  conf <- load_config("geronimo", conf_dir = conf_dir,
                      overrides = list(period = "no_such_period"))
  expect_warning(period <- get_period(conf))
  expect_null(period)
})

test_that("getters still read old single-file configurations", {
  conf <- parse_config(file.path(conf_dir, "geronimo.yml"))
  expect_equal(get_file_extensions(conf), conf[["filter"]][["keep_filepaths_ending_with"]])
  expect_equal(get_substring_filepath(conf), conf[["filter"]][["remove_filepaths_containing"]])
  expect_equal(get_filter_commit_size(conf), conf[["filter"]][["remove_filepaths_on_commit_size_greather_than"]])
})

test_that("check_config reports missing required entries", {
  conf <- load_config("geronimo", "collab_network", conf_dir = conf_dir)
  conf[["version_control"]][["branch"]] <- NULL
  expect_warning(result <- check_config(conf, "git", module_dir = module_dir),
                 "version_control.branch")
  expect_false(result[entry == "version_control.branch"]$present)
  expect_true(result[entry == "conventions.source_extensions"]$present)
})
