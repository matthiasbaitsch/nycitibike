# Clean up before
cache_clear()

test_that("caching an archive works", {
  ac <- function() {
    cache_archive(
      "2022-citibike-tripdata.zip",
      "2022-citibike-tripdata/202209-citibike-tripdata.zip"
    ) |>
      archive::archive()
  }
  expect_equal(nrow(ac()), 4)
  expect_equal(nrow(ac()), 4)
})
