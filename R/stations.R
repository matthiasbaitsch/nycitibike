stations_id_repair <- function(s) {
  dplyr::if_else(
    stringr::str_ends(s, "\\.\\d"),
    paste0(s, "0"),
    s
  ) |>
    stringr::str_replace("_Pillar", "")
}

stations_raw_extract <- function(d_trips) {
  ds1 <- d_trips |>
    dplyr::select(
      station_id = start_station_id,
      station_name = start_station_name,
      lat = start_lat,
      lng = start_lng
    )
  ds2 <- d_trips |>
    dplyr::select(
      station_id = end_station_id,
      station_name = end_station_name,
      lat = end_lat,
      lng = end_lng
    )
  purrr::bind_rows(ds1, ds2) |>
    dplyr::distinct() |>
    dplyr::arrange(station_id)
}
