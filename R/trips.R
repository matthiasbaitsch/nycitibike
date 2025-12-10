get_aws_bucket_df <- function() {
  R.cache::evalWithMemoization(
    aws.s3::get_bucket_df(
      bucket = "s3://tripdata/"
    )
  )
}

trips_list_years <- function() {
  get_aws_bucket_df() |>
    dplyr::mutate(
      year = as.integer(stringr::str_extract(Key, "^\\d{4}"))
    ) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::distinct(year) |>
    dplyr::arrange(year) |>
    dplyr::pull(year)
}

trips_download_data <- function(year) {
  data_folder <- archive_path()

  if (!dir.exists(data_folder)) {
    dir.create(data_folder)
  }

  download <- function(source) {
    dest <- file.path(data_folder, source)

    if (!file.exists(dest)) {
      message(paste(" Downloading ", source))
      aws.s3::save_object(
        source,
        file = dest,
        show_progress = TRUE,
        bucket = "s3://tripdata/"
      )
    }
  }

  get_aws_bucket_df() |>
    dplyr::filter(
      stringr::str_starts(Key, as.character(year)),
      stringr::str_ends(Key, ".zip")
    ) |>
    dplyr::pull(Key) |>
    purrr::walk(download, .progress = TRUE)
}

trips_csv_header <- function(archive, path, path2) {
  archive_read_2(archive, path, path2) |>
    readr::read_lines(n_max = 1)
}

trips_list_csv_headers <- function(
  years = "all",
  with_years = FALSE,
  .reduce = TRUE
) {
  read_one_header <- function(archive, path, path2, depth, year) {
    dplyr::tibble(
      year = year,
      header = trips_csv_header(archive, path, path2)
    )
  }

  R.cache::evalWithMemoization(
    {
      if (is.character(years) && stringr::str_equal(years, "all")) {
        years <- trips_list_years()
      }

      d <- years |>
        purrr::map(archive_ls) |>
        purrr::list_rbind() |>
        purrr::pmap(read_one_header) |>
        purrr::list_rbind()

      if (.reduce) {
        d <- d |>
          dplyr::arrange(year) |>
          dplyr::group_by(header) |>
          dplyr::slice(c(1, dplyr::n())) |>
          dplyr::ungroup() |>
          dplyr::distinct() |>
          dplyr::arrange(year)
      }

      if (!with_years) {
        d <- d |>
          dplyr::select(header) |>
          dplyr::distinct() |>
          dplyr::arrange(header)
      }

      d
    },
    key = list(years = years, with_years = with_years, .reduce = .reduce)
  )
}

trips_read_one_file <- function(archive, path, path2, depth = NA, year = NA) {
  na <- c("\\N", "NULL", "")
  format <- trips_csv_header(archive, path, path2) |>
    csv_format_from_header()

  if (format <= 3) {
    # Old format

    # Read dates in fields 2 and 3 as character for later processing
    d <- readr::read_csv(
      archive_read_2(archive, path, path2),
      col_types = "nccccnnccnnncnn",
      na = na
    )

    # Uppercase format
    if (format == 3) {
      # XXX Use one line
      f2 <- trips_csv_header_from_format(2)
      f3 <- trips_csv_header_from_format(3)
      names(f3) <- f2
      d <- d |>
        dplyr::rename(all_of(f3))
    }

    # To new format
    d <- d |>
      dplyr::mutate(rideable_type = "classic_bike") |>
      dplyr::select(
        rideable_type,
        started_at = starttime,
        ended_at = stoptime,
        start_station_name = `start station name`,
        start_station_id = `start station id`,
        end_station_name = `end station name`,
        end_station_id = `end station id`,
        start_lat = `start station latitude`,
        start_lng = `start station longitude`,
        end_lat = `end station latitude`,
        end_lng = `end station longitude`,
        bike_id = bikeid
      )
  } else if (format == 4) {
    # New format
    d <-
      readr::read_csv(
        archive_read_2(archive, path, path2),
        col_types = "ccccccccddddc",
        na = na
      ) |>
      dplyr::select(
        -ride_id,
        -member_casual
      )
  }

  # Date
  dp <- purrr::pluck(d, "started_at", 1) |> csv_get_date_parser()

  d <- d |>
    dplyr::mutate(
      started_at = dp(started_at),
      ended_at = dp(ended_at)
    )

  d
}

# Cache all years with
# `trips_list_years() |> walk(trips_cache)`
trips_cache <- function(year, .progress = TRUE) {
  cf <- cache_path_trips_raw.rds(year)
  if (!file.exists(cf)) {
    if (.progress) {
      message(paste("Reading raw trips for", year))
    }
    d <- archive_ls(year) |>
      dplyr::slice_min(order_by = depth) |>
      purrr::pmap(trips_read_one_file, .progress = .progress) |>
      purrr::list_rbind()
    readr::write_rds(d, cf)
    d
  }
}

trips_read <- function(year, .progress = TRUE) {
  cf <- cache_path_trips_raw.rds(year)
  if (file.exists(cf)) {
    d <- readr::read_rds(cf)
  } else {
    d <- trips_cache(year, .progress = .progress)
  }
  d
}
