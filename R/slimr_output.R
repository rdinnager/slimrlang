slimr_output <- function(slimr_expr, name, do_every = NULL) {
  slimr_expr <- rlang::enexpr(slimr_expr)

  expr_txt <- purrr::map_chr(slimr_expr,
                             ~rlang::expr_deparse(.x))

  if(slimr_code_detect_output(expr_txt)) {
    new_code <- rlang::exprs(cat("<slimr_output:start>" + "\\n" + paste(sim.generation) + "\\n"),
                             !!slimr_expr,
                             cat("\\n" + "<slimr_output:end>"))
  } else {
    new_code <- rlang::exprs(cat("<slimr_output:start>" + "\\n" + paste(sim.generation) + "\\n"),
                             expr(paste(!!slimr_expr)),
                             cat("\\n" + "<slimr_output:end>"))
  }

  new_code

}

sout <- function(slimr_expr, name, do_every = NULL) {
  slimr_output(slimr_expr, name, do_every)
}

out_replace <- function(code) {
  code <- stringr::str_replace_all(code, "slimr_output", "!!slimr_output")
  code <- stringr::str_replace_all(code, "sout", "!!sout")
  code_expr <- rlang::parse_exprs(paste(code, collapse = ""))
  code <- purrr::map(code_expr, ~rlang::expr_interp(.x)) %>%
    unlist()
  code
}

