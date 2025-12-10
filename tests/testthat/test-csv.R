archives = c(
  "2013-citibike-tripdata.zip",
  "2018-citibike-tripdata.zip",
  "2014-citibike-tripdata.zip",
  "2015-citibike-tripdata.zip",
  "2022-citibike-tripdata.zip"
)

paths = c(
  "2013-citibike-tripdata/201309-citibike-tripdata.csv",
  "2018-citibike-tripdata/201809-citibike-tripdata.csv",
  "2014-citibike-tripdata/12_December/201412-citibike-tripdata_1.csv",
  "2015-citibike-tripdata/6_June/201506-citibike-tripdata_1.csv",
  "2022-citibike-tripdata/202208-citibike-tripdata.zip"
)

path2s = c(NA, NA, NA, NA, "202208-citibike-tripdata_2.csv")

dates = c(
  "2013-09-01 00:00:02",
  "2018-09-01 00:00:05.2690",
  "12/1/2014 00:00:28",
  "6/1/2015 0:00",
  "2022-08-25 13:43:42.805"
)

date_times = c(
  lubridate::make_datetime(2013, 9, 1, 0, 0, 2, tz = "America/New_York"),
  lubridate::make_datetime(2018, 9, 1, 0, 0, 5, tz = "America/New_York"),
  lubridate::make_datetime(2014, 12, 1, 0, 0, 28, tz = "America/New_York"),
  lubridate::make_datetime(2015, 6, 1, 0, 0, 0, tz = "America/New_York"),
  lubridate::make_datetime(2022, 8, 25, 13, 43, 42, tz = "America/New_York")
)

test_that("header id is detected", {
  f <- trips_csv_header(
    "2013-citibike-tripdata.zip",
    "2013-citibike-tripdata/201309-citibike-tripdata.csv"
  ) |>
    csv_format_from_header()
  expect_equal(f, 1)

  f <- trips_csv_header(
    "2016-citibike-tripdata.zip",
    "2016-citibike-tripdata/4_April/201604-citibike-tripdata_2.csv"
  ) |>
    csv_format_from_header()
  expect_equal(f, 2)

  f <- trips_csv_header(
    "2016-citibike-tripdata.zip",
    "2016-citibike-tripdata/12_December/201612-citibike-tripdata_1.csv"
  ) |>
    csv_format_from_header()
  expect_equal(f, 3)

  f <- trips_csv_header(
    "2020-citibike-tripdata.zip",
    "2020-citibike-tripdata/202004-citibike-tripdata.zip",
    "202004-citibike-tripdata_1.csv"
  ) |>
    csv_format_from_header()
  expect_equal(f, 4)
})

test_that("content type identification works", {
  expect_equal(
    csv_identify_content_type(archives, paths, path2s),
    c(rep("old", 4), "new")
  )
})

test_that("date field extraction works", {
  expect_equal(csv_extract_date_field(archives, paths, path2s), dates)
})

test_that("date format identification works", {
  expect_equal(csv_identify_date_format(dates), c(1:4, 2))
})

test_that("get date parser works", {
  for (i in 1:length(dates)) {
    d <- dates[i]
    p <- csv_get_date_parser(d)
    expect_equal(p(d), date_times[i])
  }
})
