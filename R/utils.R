# modified from drake::assert_pkg
assert_package <- function (pkg, version = NULL, install = "install.packages") {

  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("This function requires the ", pkg, " package, which is not installed. Install with ",
          install, "(\"", pkg, "\").", call. = FALSE)
  }
  if (!is.null(version)) {

    installed_version <- as.character(utils::packageVersion(pkg))
    is_too_old <- utils::compareVersion(installed_version, version) < 0
    if (is_too_old) {
      stop("This function package requires the ", pkg, " package to be version ",
            version, " or heigher. ", "Found version ",
            version, " installed.", "Update it with ",
            install, "(\"", pkg, "\").",
           call. = FALSE)
    }
  }

  invisible()

}
