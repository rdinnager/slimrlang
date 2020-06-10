slimr_output <- function(slimr_expr, name, do_every = 1) {
  slimr_expr <- rlang::enexpr(slimr_expr)

  expr_txt <- rlang::expr_deparse(slimr_expr)

  if(slimr_code_detect_output(expr_txt)) {
    new_code <- rlang::exprs(
      if(sim.generation %% !!do_every == 0) {
        cat("<slimr_out:start>" + paste(sim.generation) + ',"' +
              !!name + '","')
        !!slimr_expr
        cat('"<slimr_out:end>')
      }
    )
  } else {
    new_code <- rlang::exprs(
      if(sim.generation %% !!do_every == 0) {
        cat("<slimr_out:start>" + paste(sim.generation) + ',"' +
              !!name + '","')
        rlang::exprs(catn(paste(!!slimr_expr)))
        cat('"<slimr_out:end>')
    })
  }


  .resources$temp_slimr_output$code_for_slim <- c(.resources$temp_slimr_output$code_for_slim,
                                                  paste(new_code, collapse = "\n"))
  .resources$temp_slimr_output$code_for_display <- c(.resources$temp_slimr_output$code_for_display,
                                                     paste(expr_txt, collapse = "\n"))
  .resources$temp_slimr_output$output_name <- c(.resources$temp_slimr_output$output_name,
                                                name)

  new_code

}

sout <- function(slimr_expr, name, do_every = NULL) {
  slimr_output(slimr_expr, name, do_every)
}

out_replace <- function(code) {
  code <- stringr::str_replace_all(code, "slimr_output", "!!slimr_output")
  code <- stringr::str_replace_all(code, "sout", "!!sout")
  code_expr <- rlang::parse_exprs(paste(code, collapse = "\n"))
  code <- purrr::map(code_expr, ~rlang::expr_interp(.x)) %>%
    unlist()
  code <- purrr::map(code,
                     ~rlang::expr_deparse(.x))

  if(any(purrr::map_lgl(code, ~inherits(.x, "list")))) {
    code <- code %>%
      purrr::flatten()
  }
  code
}

gather_out_one <- function(code_one) {
  .resources$temp_slimr_output$code_for_slim <- list()
  .resources$temp_slimr_output$output_name <- list()
  .resources$temp_slimr_output$code_for_display <- list()

  code_one <- out_replace(code_one)
  output_info <- list(code_for_slim = .resources$temp_slimr_output$code_for_slim,
                      code_for_display = .resources$temp_slimr_output$code_for_display,
                      output_name = .resources$temp_slimr_output$output_name)
  list(new_code = code_one, output_info = output_info)
}

gather_out <- function(code) {
  res <- purrr::map(code,
                    ~gather_out_one(.x))
  res
}

process_output <- function(code, block_names) {
  output_processed <- gather_out(as.character(code)) %>%
    purrr::transpose()

  slimr_output_attr <- purrr::transpose(output_processed$output_info) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate("block_name" := block_names) %>%
    tidyr::unnest(c("code_for_slim", "code_for_display", "output_name"),
                  keep_empty = TRUE) %>%
    dplyr::mutate_at(c("code_for_slim", "code_for_display", "output_name"),
                     ~purrr::map(.,
                                 ~ purrr::`%||%`(.x, NA))) %>%
    dplyr::mutate_at(c("code_for_slim", "code_for_display", "output_name"),
                     ~vec_unchop(.))

  #new_code <- SLiMify_all(output_processed$new_code)

  new_code <- purrr::map(output_processed$new_code,
                         ~unlist(.x))

  list(new_code, slimr_output_attr)
}

