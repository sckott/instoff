#' Install locally from your own machine
#'
#' @export
#' @param pkg (character) A package name. Required.
#' @param version (character) If NULL, we try to install from the latest
#' version prior to your current version
#' @param force (logical) force installation attempt. Default: \code{TRUE}
#' @examples \dontrun{
#' in_stall(pkg = "abind")
#' in_stall(pkg = "abind", version = "3.2")
#' }
in_stall <- function(pkg, version = NULL, force = TRUE) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    if (force) {
      warning(pkg, " already installed", call. = FALSE)
    } else {
      stop(pkg, " already installed", call. = FALSE)
    }
  }
  from_version(pkg, version)
}

r_home <- function(ver = NULL) {
  if (is.null(ver)) {
    ver <- paste(R.version$major, R.version$minor, sep = ".")
    ver <- sub("\\.0$", "", ver)
  }
  list(
    home = file.path(sub("Resources", "Versions", R.home())),
    current = ver
  )
}

r_versions <- function() {
  list.files(r_home()$home, pattern = "[0-9]")
}

r_version <- function(x = NULL) {
  if (is.null(x)) x <- r_home()$current
  file.path(r_home()$home, x, "Resources")
}

from_version <- function(pkg, version) {
  if (is.null(version)) {
    version <- last(as.numeric(r_versions())[as.numeric(r_versions()) < as.numeric(r_home()$current)])
  }
  from <- file.path(r_version(version), "library", pkg)
  to <- file.path(r_version(), "library")
  if (!file.exists(file.path(to, pkg))) dir.create(file.path(to, pkg), showWarnings = FALSE, recursive = TRUE)
  file.copy(from, to, recursive = TRUE)
}
