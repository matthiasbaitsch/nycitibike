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
    # Fields 2 and 3 are dates, however, read it as character since
    # date format varies
    d <- readr::read_csv(
      archive_read_2(archive, path, path2),
      col_types = "nccccnnccnnncnn",
      na = na
    )

    # Uppercase format
    if (format == 3) {
      f2 <- trips_csv_header_from_format(2)
      f3 <- trips_csv_header_from_format(3)
      names(f3) <- f2
      d <- d |>
        rename(all_of(f3))
    }

    # To new format
    d <- d |>
      mutate(rideable_type = "classic_bike") |>
      select(
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
      )
  } else if (format == 4) {
    # New format
    d <- read_csv(
      archive_read_2(archive, path, path2),
      col_types = "ccccccccddddc",
      na = na
    ) |>
      select(
        -ride_id,
        -member_casual
      )
  }

  # Date
  dp <- pluck(d, "started_at", 1) |> csv_get_date_parser()

  d <- d |>
    mutate(
      started_at = dp(started_at),
      ended_at = dp(ended_at)
    )

  d
}

trips_read <- function(year) {
  archive_ls(year) |>
    pmap(trips_read_one_file, .progress = TRUE) |>
    list_rbind()
}
