library(arrow, warn.conflicts = FALSE)
library(diffdf)
library(devtools)
library(tidyverse, warn.conflicts = FALSE, verbose = FALSE)

devtools::load_all()

system.time(
  {
    message("Read CSV")
    d1 <- archive_ls(2013) |>
      filter(depth == 2) |>
      pmap(trips_read_one_file, .progress = TRUE) |>
      list_rbind() |>
      arrange(started_at)
  }
)

tmp <- tempdir()
d1.rds <- file.path(tmp, "d1.rds")
d1.parquet <- file.path(tmp, "d1.parquet")

write_rds(d1, file = d1.rds)
write_dataset(d1, path = d1.parquet)

system.time(
  {
    message("Read RDS")
    d2 <- read_rds(file = d1.rds)
  }
)

system.time(
  {
    message("Read Parquet")
    d3 <- open_dataset(sources = d1.parquet) |>
      collect() |>
      arrange(started_at)
  }
)

message("Compare CSV and RDS")
all.equal(d1, d2)
message("Compare CSV and Parquet")
diffdf(d1, d3)

# d4 <- trips_read(2024)

# d5 <- d4 |>
#   select(
#     starts_with("start"),
#     starts_with("end"),
#     -ends_with("lat"),
#     -ends_with("lng"),
#     -ends_with("name"),
#     rideable_type
#   ) |>
#   mutate(
#     rideable_type = as.factor(rideable_type)
#   )
# d5

# object.size(d4)
# object.size(d5)
