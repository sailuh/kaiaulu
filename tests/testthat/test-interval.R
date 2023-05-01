commit_hash_one <- "319857233b75121f357801cbdd9b5028e3057dbd"
commit_hash_two <- "83c52a8d7ab0be9215a70351b2a4d27938092d72"
commit_hash_three <- "9e7ba8cbacd5d09eb2af4c76eb82f7df5e0b8739"

datetime_one <- "2006-04-23 11:16:01"
datetime_two <- "2006-6-11 20:12:42"
datetime_three <- "2006-05-24 21:45:31"

pathname_one <- "path/to/file/one.c"
pathname_two <- "path/to/file/two.py"
pathname_three <- "path/to/file/three.py"

commit_hash <- c(commit_hash_one, commit_hash_two, commit_hash_three)
author_datetimetz <- c(datetime_one, datetime_two, datetime_three)
file_pathname <- c(pathname_one, pathname_two, pathname_three)

test_that("Datetime found amongst multiple rows", {
  minimal_dt = data.table(author_datetimetz, commit_hash, file_pathname)

  output_datetime <- get_date_from_commit_hash(minimal_dt, commit_hash_one)
  expect_equal(output_datetime, datetime_one)
})

test_that("Null returned when table doesn't have author_datetimetz column", {
  minimal_dt = data.table("random_name" = author_datetimetz, commit_hash, file_pathname)
  output_datetime <- get_date_from_commit_hash(minimal_dt, commit_hash_one)
  expect_equal(output_datetime, NULL)
})

test_that("NA returned if table doesn't have correct commit_hash", {
  commit_hash_to_find <- "3nv8938cbacd5d09eb2af4c76eb82f7df5e0b8739"

  minimal_dt = data.table(author_datetimetz, commit_hash, file_pathname)
  output_datetime <- get_date_from_commit_hash(minimal_dt, commit_hash_to_find)
  print(class(output_datetime))
  expect_equal(is.na(output_datetime), TRUE)
})

test_that("Return table with correctly filtered pathnames", {
  file_extensions <- c("py")
  minimal_dt = data.table(author_datetimetz, commit_hash, file_pathname)

  output_dt = filter_by_file_extension(minimal_dt, file_extensions, "file_pathname")
  for (filename in output_dt$file_pathname) {
    expect_equal(stri_sub(filename, -3, -1), ".py")
  }
})

test_that("Return empty table when pathname doesn't exist in table", {
  file_extensions <- c("txt")
  minimal_dt = data.table(author_datetimetz, commit_hash, file_pathname)

  output_dt = filter_by_file_extension(minimal_dt, file_extensions, "file_pathname")
  expect_equal(nrow(output_dt), 0)
})

test_that("Return empty table when table has no pathname column", {
  file_extensions <- c("py")
  minimal_dt = data.table(author_datetimetz, commit_hash, file_pathname)

  output_dt = filter_by_file_extension(minimal_dt, file_extensions, "random_name")
  expect_equal(nrow(output_dt), 0)
})
