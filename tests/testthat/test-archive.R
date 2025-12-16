test_that("reading from an archive works", {
  line <- archive_read_2(
    "2013-citibike-tripdata.zip",
    "2013-citibike-tripdata/201309-citibike-tripdata.csv"
  ) |>
    readr::read_lines(n_max = 1)

  expect_true(stringr::str_starts(line, "\"tripduration\""))
})

test_that("reading from an archive within an archive works", {
  line <- archive_read_2(
    "2022-citibike-tripdata.zip",
    "2022-citibike-tripdata/202205-citibike-tripdata.zip",
    "202205-citibike-tripdata_3.csv"
  ) |>
    readr::read_lines(n_max = 1)

  expect_true(stringr::str_starts(line, "ride_id"))
})

test_that("listing an archive with CSV files works", {
  d <- archive_ls(2013)
  expect_equal(purrr::pluck(d, "archive", 1), "2013-citibike-tripdata.zip")
  expect_equal(nrow(d), 17)
})

test_that("listing an archive with archives works", {
  d <- archive_ls(2022)
  expect_equal(purrr::pluck(d, "archive", 1), "2022-citibike-tripdata.zip")
  expect_equal(nrow(d), 36)
})
