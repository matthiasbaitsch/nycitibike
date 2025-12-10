library(devtools)
library(tidyverse)
devtools::load_all()


# trips_csv_header_from_format(2, .split = F)

# Format = 1
# trips_read_one_file(
#   "2013-citibike-tripdata.zip",
#   "2013-citibike-tripdata/201309-citibike-tripdata.csv"
# )

# Format = 2
# trips_read_one_file(
#   "2016-citibike-tripdata.zip",
#   "2016-citibike-tripdata/12_December/201612-citibike-tripdata_1.csv"
# )

# Format = 3
# trips_read_one_file(
#   "2016-citibike-tripdata.zip",
#   "2016-citibike-tripdata/12_December/201612-citibike-tripdata_1.csv"
# )

# Format = 4
# trips_read_one_file(
#   "2020-citibike-tripdata.zip",
#   "2020-citibike-tripdata/202004-citibike-tripdata.zip",
#   "202004-citibike-tripdata_1.csv"
# )

d <- trips_read(2024)

# archive_read_2(
#   "2014-citibike-tripdata.zip",
#   "2014-citibike-tripdata/12_December/201412-citibike-tripdata_1.csv"
# ) |>
#   read_lines(n_max = 2)

dt <- d |>
  select(started_at, ended_at, start_station_id, end_station_id, rideable_type)


ds1 <- d |>
  select(
    station_id = start_station_id,
    station_name = start_station_name,
    lat = start_lat,
    lng = start_lng
  )
ds2 <- d |>
  select(
    station_id = end_station_id,
    station_name = end_station_name,
    lat = end_lat,
    lng = end_lng
  )
ds <- bind_rows(ds1, ds2) |>
  distinct() |>
  arrange(station_id)
