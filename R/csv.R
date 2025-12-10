csv_date_regexps <- c(
  # yyyy-mm-dd hh:mm:ss
  "^\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}$",
  # yyyy-mm-dd hh:mm:ss.ms
  "^\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{3,4}$",
  # mm/dd/yyyy hh:mm:ss
  "^\\d\\d?/\\d\\d?/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
  # mm/dd/yyyy hh:mm
  "^\\d\\d?/\\d\\d?/\\d{4} \\d\\d?:\\d{2}$"
)

csv_date_parsers <- c(
  \(d) lubridate::ymd_hms(d, tz = "America/New_York"),
  \(d) lubridate::ymd_hms(d, tz = "America/New_York"),
  \(d) lubridate::mdy_hms(d, tz = "America/New_York"),
  \(d) lubridate::mdy_hm(d, tz = "America/New_York")
)

# format = 1: Old form lowercase, quoted (""\"tripduration\",\"starttime\",\"stoptime\",...")
# format = 2: Old form lowercase ("tripduration,starttime,stoptime,...")
# format = 3: Old form uppercase ("Trip Duration,Start Time,Stop Time,...")
# format = 4: New form ("ride_id,rideable_type,started_at,ended_at,...")
# Strings from "trips_list_csv_headers() |> pluck(h, "header", 2)"
trips_csv_header_from_format <- function(format, .split = TRUE) {
  # TODO use array
  if (format == 1) {
    h <- "\"tripduration\",\"starttime\",\"stoptime\",\"start station id\",\"start station name\",\"start station latitude\",\"start station longitude\",\"end station id\",\"end station name\",\"end station latitude\",\"end station longitude\",\"bikeid\",\"usertype\",\"birth year\",\"gender\""
  } else if (format == 2) {
    h <- "tripduration,starttime,stoptime,start station id,start station name,start station latitude,start station longitude,end station id,end station name,end station latitude,end station longitude,bikeid,usertype,birth year,gender"
  } else if (format == 3) {
    h <- "Trip Duration,Start Time,Stop Time,Start Station ID,Start Station Name,Start Station Latitude,Start Station Longitude,End Station ID,End Station Name,End Station Latitude,End Station Longitude,Bike ID,User Type,Birth Year,Gender"
  } else if (format == 4) {
    h <- "ride_id,rideable_type,started_at,ended_at,start_station_name,start_station_id,end_station_name,end_station_id,start_lat,start_lng,end_lat,end_lng,member_casual"
  } else {
    stop("Illegal ID")
  }
  if (.split) {
    h <- h |> stringr::str_split_1(",")
  }
  h
}

csv_format_from_header <- function(h) {
  # TODO use purrr
  for (i in 1:10) {
    if (stringr::str_equal(h, trips_csv_header_from_format(i, .split = F))) {
      return(i)
    }
  }
}

csv_identify_date_format <- Vectorize(
  function(date) {
    purrr::detect_index(csv_date_regexps, \(regexp) {
      stringr::str_detect(date, regexp)
    })
  },
  USE.NAMES = FALSE
)

csv_identify_content_type <- Vectorize(
  function(archive, path, path2 = NA) {
    c1 <- archive_read_2(archive, path, path2) |>
      readr::read_lines(n_max = 1) |>
      purrr::pluck(1) |>
      stringr::str_split_1(",") |>
      purrr::pluck(1)
    if (c1 == "ride_id") {
      return("new")
    } else if (c1 %in% c("\"tripduration\"", "tripduration", "Trip Duration")) {
      return("old")
    }
    stop("File type could not be detected")
  },
  USE.NAMES = FALSE
)

csv_extract_date_field <- Vectorize(
  function(archive, path, path2 = NA) {
    type = csv_identify_content_type(archive, path, path2)
    field = dplyr::if_else(type == "old", 2, 3)
    archive_read_2(archive, path, path2) |>
      readr::read_lines(n_max = 2) |>
      purrr::pluck(2) |>
      stringr::str_split_1(",") |>
      purrr::pluck(field) |>
      stringr::str_replace_all("\"", "")
  },
  USE.NAMES = FALSE
)

csv_get_date_parser <- function(d) {
  csv_date_parsers |> purrr::pluck(csv_identify_date_format(d))
}
