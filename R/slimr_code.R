SLiMify <- function(code) {
  code <- slimr_code_add_semicolons_one(code)
  code <- slimr_code_replace_dots_one(code)
  code <- slimr_code_remove_special_classes_one(code)
  code <- slimr_code_replace_ternary_one(code)

  code
}

SLiMify_all <- function(code) {
  code <- slimr_code_add_semicolons(code)
  code <- slimr_code_replace_dots(code)
  code <- slimr_code_remove_special_classes(code)
  code <- slimr_code_replace_ternary(code)

  code
}

slimr_code_add_semicolons_one <- function(code_one) {
  brace_lines <- stringr::str_detect(code_one,
                                     "(\\{$|\\}$)")
  code_one[!brace_lines] <- paste0(code_one[!brace_lines], ";")
  code_one
}

slimr_code_add_semicolons <- function(code) {
  purrr::map(code,
             ~slimr_code_add_semicolons_one(.x))
}


slimr_code_replace_dots_one <- function(code_one) {
  stringr::str_replace_all(code_one,
                           glue::glue(" \\%\\.\\% {.resources$classes_regex}\\$"),
                           ".")
}

slimr_code_replace_dots <- function(code) {
  purrr::map(code,
             ~slimr_code_replace_dots_one(.x))
}

slimr_code_remove_special_classes_one <- function(code_one) {
  stringr::str_remove_all(code_one,
                           "(\\.Init|Initialize|\\.SS|SLiMBuiltin)\\$")
}

slimr_code_remove_special_classes <- function(code) {
  purrr::map(code,
             ~slimr_code_remove_special_classes_one(.x))
}

slimr_code_replace_ternary_one <- function(code_one) {
  code <- stringr::str_replace_all(code_one,
                                   "\\%?\\%",
                                   "?")
  stringr::str_replace_all(code_one,
                           "\\%else\\%",
                           "else")
}

slimr_code_replace_ternary <- function(code) {
  purrr::map(code,
             ~slimr_code_replace_ternary_one(.x))
}

assert_valid_code <- function(code_txt) {
  code <- try(parse(text = code_txt))
  if(inherits(code, "try-error")) {
    stop(paste("Not valid R code; something went wrong in translation.", "error:", code))
  }
  code_txt
}
