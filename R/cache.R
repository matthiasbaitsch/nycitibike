cache_path <- function(file = NULL) {
  if (is.null(file)) {
    fs::path(archive_path(), "cache")
  } else {
    fs::path(archive_path(), "cache", file)
  }
}

cache_clear <- function() {
  d <- cache_path()
  if (fs::dir_exists(d)) {
    fs::dir_delete(d)
  }
  R.cache::clearCache(prompt = FALSE)
}

cache_path_trips_raw.rds <- function(year) {
  cache_path(paste0("trips-raw-", year, ".rds"))
}

cache_path_trips.rds <- function(year) {
  cache_path(paste0("trips-", year, ".rds"))
}

#' Unzip file from archive to cache folder.
#'
#' @param archive Archive file
#' @param path Path within archive
#'
#' @returns Path to chached file
cache_archive <- function(archive, path) {
  cachedir <- cache_path(paste0(basename(archive), ".dir"))
  destfile <- fs::path(cachedir, path)
  if (!fs::file_exists(destfile)) {
    if (!fs::dir_exists(cachedir)) {
      fs::dir_create(cachedir, recurse = TRUE)
    }
    archive::archive_extract(
      archive_path(archive),
      dir = cachedir,
      files = path
    )
  }
  destfile
}
