archive_path <- function() here::here("data")

archive_read_2 <- function(f, p1, p2 = NULL) {
  ap <- file.path(archive_path(), f)

  # assert::assert(file.exists(ap), paste("No such file:", ap))

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
