## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

.resources <- new.env()

globalVariables(c(".G",
                   ".GE",
                   ".GET",
                   ".I",
                   ".IT",
                   ".M",
                   ".MT",
                   ".P",
                   ".S",
                   ".SB",
                   ".SS",
                   ".c",
                  ".Init",
                  ".x")
)

.onLoad <- function(libname, pkgname) {
  data(slim_classes, package = pkgname,
       envir = parent.env(environment()))
  .resources$classes_regex <- paste0("(", paste(c(slim_classes$class_name, slim_classes$class_abbr),
                                    collapse = "|"), ")")
}
