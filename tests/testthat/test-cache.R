test_that("caching an archive works", {
  d <- cache_archive(
    "2022-citibike-tripdata.zip",
    "2022-citibike-tripdata/202209-citibike-tripdata.zip"
  ) |>
    archive::archive()
  expect_equal(nrow(d), 4)
})
