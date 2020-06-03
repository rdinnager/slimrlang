
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
  utils::data(slim_classes, package = pkgname,
       envir = parent.env(environment()))
  .resources$classes_regex <- paste0("(", paste(c(slim_classes$class_name, slim_classes$class_abbr),
                                    collapse = "|"), ")")
  .resources$temp_slimr_template <- list()
  .resources$temp_slimr_template$var_name <- list()
  .resources$temp_slimr_template$default <- list()
  .resources$temp_slimr_template$unquote <- list()
  .resources$temp_slimr_input <- list()
  .resources$temp_slimr_output <- list()
}
