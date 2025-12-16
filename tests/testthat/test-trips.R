# TODO: Test that

# Format = 1
# trips_read_one_file(
#   "2013-citibike-tripdata.zip",
#   "2013-citibike-tripdata/201309-citibike-tripdata.csv"
# )

# Format = 2
# trips_read_one_file(
#   "2016-citibike-tripdata.zip",
#   "2016-citibike-tripdata/12_December/201612-citibike-tripdata_1.csv"
# )

# Format = 3
# trips_read_one_file(
#   "2016-citibike-tripdata.zip",
#   "2016-citibike-tripdata/12_December/201612-citibike-tripdata_1.csv"
# )

# Format = 4
# trips_read_one_file(
#   "2020-citibike-tripdata.zip",
#   "2020-citibike-tripdata/202004-citibike-tripdata.zip",
#   "202004-citibike-tripdata_1.csv"
# )

test_that("the assumption about the content of csv files holds", {
  # Files in base folder
  d1 <- archive_ls(2013) |>
    dplyr::filter(depth == 1) |>
    purrr::pmap(trips_read_one_file) |>
    purrr::list_rbind() |>
    dplyr::mutate(end_station_id = as.integer(end_station_id)) |>
    dplyr::arrange(started_at)

  # Files in month folders inside base folder
  d2 <- archive_ls(2013) |>
    dplyr::filter(depth == 2) |>
    purrr::pmap(trips_read_one_file) |>
    purrr::list_rbind() |>
    dplyr::mutate(end_station_id = as.integer(end_station_id)) |>
    dplyr::arrange(started_at)

  # Compare
  expect_equal(d1, d2)
})

test_that("we do not read duplicate data", {
  d <- trips_raw_read(2013, .progress = F)
  expect_equal(nrow(d), nrow(dplyr::distinct(d)))
})
