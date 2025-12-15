trips_read_one_file <- function(archive, path, path2, depth = NA, year = NA) {
  na <- c("\\N", "NULL", "")
  format <- csv_header(archive, path, path2) |>
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
      f2 <- csv_header_from_format(2)
      f3 <- csv_header_from_format(3)
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

# Cache all years with `archive_years() |> walk(trips_raw_cache)`
trips_raw_cache <- function(year, .progress = TRUE) {
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

# Cache all years with `archive_years() |> walk(trips_cache)`
trips_cache <- function(year, .progress = TRUE) {
  cf <- cache_path_trips.rds(year)
  if (!file.exists(cf)) {
    d <- trips_raw_read(year, .progress) |>
      dplyr::mutate(
        start_station_id = stations_id_repair(start_station_id),
        end_station_id = stations_id_repair(end_station_id)
      )
    readr::write_rds(d, cf)
    d
  }
}

trips_raw_read <- function(year, .progress = TRUE) {
  cf <- cache_path_trips_raw.rds(year)
  if (file.exists(cf)) {
    d <- readr::read_rds(cf)
  } else {
    d <- trips_raw_cache(year, .progress = .progress)
  }
  d
}

trips_read <- function(year, .progress = TRUE) {
  cf <- cache_path_trips.rds(year)
  if (file.exists(cf)) {
    d <- readr::read_rds(cf)
  } else {
    d <- trips_cache(year, .progress = .progress)
  }
  d
}
