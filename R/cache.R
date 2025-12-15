cache_path <- function(file = NULL) {
  if (is.null(file)) {
    file.path(archive_path(), "cache")
  } else {
    file.path(archive_path(), "cache", file)
  }
}

cache_path_trips_raw.rds <- function(year) {
  cache_path(paste0("trips-raw-", year, ".rds"))
}

cache_path_trips.rds <- function(year) {
  cache_path(paste0("trips-", year, ".rds"))
}

cache_clear <- function() {
  fs::dir_delete(cache_path())
}

#' Unzip file from archive to cache folder.
#'
#' @param archive Archive file
#' @param path Path within archive
#'
#' @returns Path to chached file
cache_archive <- function(archive, path) {
  cachedir <- file.path(cache_path(), paste0(basename(archive), ".dir"))
  path <- file.path(cachedir, path)

  # Create cache folder if needed
  if (!dir.exists(cachedir)) {
    dir.create(cachedir, recursive = TRUE)
  }

  # Extract if path does not exist
  if (!file.exists(path)) {
    archive::archive_extract(archive, dir = cachedir, files = path)
  }

  path
}
