stations_id_repair <- function(s) {
  if_else(
    str_ends(s, "\\.\\d"),
    paste0(s, "0"),
    s
  ) |>
  str_replace("_Pillar", "")
}

stations_raw_extract <- function(d_trips) {
  ds1 <- d_trips |>
    select(
      station_id = start_station_id,
      station_name = start_station_name,
      lat = start_lat,
      lng = start_lng
    )
  ds2 <- d_trips |>
    select(
      station_id = end_station_id,
      station_name = end_station_name,
      lat = end_lat,
      lng = end_lng
    )
  bind_rows(ds1, ds2) |>
    distinct() |>
    arrange(station_id)
}

