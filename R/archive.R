archive_path <- function(file = NULL) {
  ifelse(
    is.null(file),
    here::here("data"),
    file.path(here::here("data"), file)
  )
}

get_aws_bucket_df <- function() {
  R.cache::evalWithMemoization(
    aws.s3::get_bucket_df(
      bucket = "s3://tripdata/"
    )
  )
}

archive_download <- function(year, .progress = TRUE) {
  if (!fs::dir_exists(archive_path())) {
    fs::dir_create(archive_path())
  }

  download <- function(f) {
    destfile <- archive_path(f)
    if (!fs::file_exists(destfile)) {
      url <- paste0("https://s3.amazonaws.com/tripdata/", f)
      if (.progress) {
        message(paste("Downloading ", f))
      }
      curl::multi_download(url, destfile, progress = .progress, resume = TRUE)
    }
  }

  files <- get_aws_bucket_df() |>
    dplyr::filter(
      stringr::str_starts(Key, as.character(year)),
      stringr::str_ends(Key, ".zip")
    ) |>
    dplyr::pull(Key) |>
    walk(download, .progress = .progress)
}

archive_years <- function() {
  get_aws_bucket_df() |>
    dplyr::mutate(
      year = as.integer(stringr::str_extract(Key, "^\\d{4}"))
    ) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::distinct(year) |>
    dplyr::arrange(year) |>
    dplyr::pull(year)
}

archive_read_2 <- function(f, p1, p2 = NULL) {
  ap <- file.path(archive_path(), f)
  # XXX assert::assert(fs::is_file(ap), paste("No such file:", ap))
  if (stringr::str_ends(p1, "csv")) {
    archive::archive_read(ap, p1)
  } else if (stringr::str_ends(p1, "zip")) {
    archive::archive_read(cache_archive(ap, p1), p2)
  } else {
    stop("Should not happen")
  }
}

archive_ls <- function(year) {
  #
  # Helper function: List content of one archive file
  list_one_archive <- function(f) {
    archive::archive(f) |>
      dplyr::filter(
        stringr::str_ends(path, "csv|zip"),
        stringr::str_detect(path, "MACOSX", negate = TRUE)
      ) |>
      dplyr::mutate(
        archive = basename(f),
        file = basename(path),
        depth = stringr::str_count(path, "/"),
        year = year
      )
  }

  #
  # List content of archive file in archive file
  archive_list <- Vectorize(function(f, p) {
    if (stringr::str_ends(p, "zip")) {
      cache_archive(f, p) |>
        archive::archive() |>
        dplyr::arrange(path) |>
        dplyr::pull(path) |>
        unlist()
    } else {
      as.character(NA)
    }
  })

  list.files(
    path = archive_path(),
    pattern = paste0("^", year, ".*\\.zip"),
    full.names = TRUE
  ) |>
    purrr::map(list_one_archive) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      path2 = archive_list(archive, path)
    ) |>
    tidyr::unnest(path2) |>
    dplyr::select(archive, path, path2, depth, year)
}
