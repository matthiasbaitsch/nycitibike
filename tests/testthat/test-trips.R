test_that("headers are complete and in order", {
  years <- c(2013, 2016, 2018, 2020)

  # No years, test content
  d <- trips_list_csv_headers(
    years = years,
    with_years = FALSE
  )
  expect_equal(nrow(d), 4)
  expect_true(purrr::pluck(d, "header", 1) |> stringr::str_starts("\"tripduration"))
  expect_true(purrr::pluck(d, "header", 2) |> stringr::str_starts("Trip Duration"))
  expect_true(purrr::pluck(d, "header", 3) |> stringr::str_starts("ride_id"))
  expect_true(purrr::pluck(d, "header", 4) |> stringr::str_starts("tripduration"))

  # With years, just test count
  d <- trips_list_csv_headers(
    years = years,
    with_years = TRUE
  )
  expect_equal(nrow(d), 6)

  # With years, not reduced, just test count
  d <- trips_list_csv_headers(
    years = years,
    with_years = TRUE,
    .reduce = FALSE
  )
  expect_equal(nrow(d), 99)
})

