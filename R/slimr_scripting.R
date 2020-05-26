#' Create a SLiMR script
#'
#' @param ... A list of \code{\link{slim_block}} objects comprising a SLiM script (written in slimr code)
#'
#' @return A \code{slim_script} object that can be used with \code{slimr}'s \code{\link{slim_run_script}}
#' or converted into a text file for use with SLiM directly using \code{\link{as.character}}.
#' @export
#'
#' @examples
slim_script <- function(...) {
  script_list <- list(...)

  n_row <- length(script_list)

  script <- script_list %>%
    purrr::transpose() %>%
    purrr::simplify_all()

  block_names = paste0("block_", stringr::str_pad(seq_len(n_row),
                                                 nchar(trunc(n_row)),
                                                 pad = "0"))

  block_names = ifelse(script$callback == "initialize()",
                       "block_init",
                       block_names)


  suppressWarnings(end_gen <- max(as.numeric(c(script$start_gen, script$end_gen)), na.rm = TRUE))

  script$end_gen <- purrr::map_chr(script$end_gen,
                              ~glue::glue(.x, .na = NULL) %>%
                                as.character())

  code <- vec_unchop(script$code)

  template_processed <- gather_tmplt(as.character.slimr_code(code)) %>%
    purrr::transpose()

  new_code <- SLiMify_all(template_processed$new_code)

  code <- new_slimr_code(new_code)

  slimr_input_attr <- purrr::transpose(template_processed$input_info) %>%
    tibble::as_tibble() %>%
    dplyr::mutate("block_name" := block_names) %>%
    tidyr::unnest(c(var_names, defaults),
                  keep_empty = TRUE)

  script <- new_slimr_script(block_name = block_names,
                             block_id = script$block_id,
                             start_gen = script$start_gen,
                             end_gen = script$end_gen,
                             callback = script$callback,
                             code = code,
                             slimr_input = slimr_input_attr)

  script
}

#' Setup a SLiM code block
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
slim_block <- function(...) {

  args <- eval(substitute(alist(...)))

  n_args <- length(args)

  if(n_args > 5) {
    stop("You've provided too many arguments. There shouldn't be more than 5 at max (block id, start generation, end generation, callback function, and slimr_code block expression)")
  }

  if(n_args < 1) {
    stop("slim_block requires at least one argument")
  }

  if(!is.call(args[[n_args]])) {
    stop("The last argument of slim_block should be a valid slimr_code block expression.")
  }

  code <- deparse(args[[n_args]], width.cutoff = 500, control = NULL)

  if(code[1] == "{") {
    code <- code[2:(length(code) - 1L)]
  }

  code <- SLiMify(code)

  code <- new_slimr_code(list(code))

  if(n_args > 1L) {
    other_args <- args[-n_args]
    args_eval <- lapply(other_args, eval, envir = callbacks)
    arg_signature <- lapply(args_eval, class)
    arg_signature <- lapply(arg_signature, function(x) ifelse(x == "integer", "numeric", x))

    if(any(unlist(arg_signature) == "name")) {
      if(any(unlist(arg_signature)[unlist(arg_signature) == "name"] != "")) {
        stop(paste0("Invalid use of a name in arguments. Problem argument(s) are ",
                    paste(which(unlist(arg_signature)[unlist(arg_signature) == "name"] != ""))))
      }
    }

    arg_signature <- purrr::map_chr(arg_signature,
                                    ~stringr::str_sub(.x, 1, 2)) %>%
      paste0(collapse = "")

    block_row <- switch(
      arg_signature,
      chnunuca = list(block_id = args_eval[[1]],
                      start_gen = as.character(args_eval[[2]]),
                      end_gen = as.character(args_eval[[3]]),
                      callback = args_eval[[4]],
                      code = code),

      chnunaca = list(block_id = args_eval[[1]],
                      start_gen = as.character(args_eval[[2]]),
                      end_gen = {end_gen},
                      callback = args_eval[[4]],
                      code = code),

      nunuca = list(block_id = "",
                    start_gen = as.character(args_eval[[1]]),
                    end_gen = as.character(args_eval[[2]]),
                    callback = args_eval[[3]],
                    code = code),

      nunaca = list(block_id = "",
                    start_gen = as.character(args_eval[[1]]),
                    end_gen = "{end_gen}",
                    callback = args_eval[[3]],
                    code = code),

      chnuca = list(block_id = args_eval[[1]],
                    start_gen = as.character(args_eval[[2]]),
                    end_gen = NA_character_,
                    callback = args_eval[[3]],
                    code = code),

      chnunu = list(block_id = args_eval[[1]],
                    start_gen = as.character(args_eval[[2]]),
                    end_gen = as.character(args_eval[[3]]),
                    callback = "early()",
                    code = code),

      chnuna = list(block_id = args_eval[[1]],
                    start_gen = as.character(args_eval[[2]]),
                    end_gen = "{end_gen}",
                    callback = "early()",
                    code = code),

      nunu = list(block_id = "",
                  start_gen = as.character(args_eval[[1]]),
                  end_gen = as.character(args_eval[[2]]),
                  callback = callbacks$early(),
                  code = code),

      nuna = list(block_id = "",
                  start_gen = as.character(args_eval[[1]]),
                  end_gen = "{end_gen}",
                  callback = callbacks$early(),
                  code = code),

      chnu = list(block_id = args_eval[[1]],
                  start_gen = as.character(args_eval[[2]]),
                  end_gen = NA_character_,
                  callback = callbacks$early(),
                  code = code),

      chca = list(block_id = args_eval[[1]],
                  start_gen = "1",
                  end_gen = "{end_gen}",
                  callback = args_eval[[2]],
                  code = code),

      nuca = list(block_id = "",
                  start_gen = as.character(args_eval[[1]]),
                  end_gen = NA_character_,
                  callback = args_eval[[2]],
                  code = code),

      ch = list(block_id = args_eval[[1]],
                start_gen = "1",
                colon = ":",
                end_gen = "{end_gen}",
                callback = callbacks$early(),
                code = code),

      nu = list(block_id = "",
                start_gen = as.character(args_eval[[1]]),
                end_gen = NA_character_,
                callback = callbacks$early(),
                code = code),

      ca = list(block_id = "",
                start_gen = "1",
                end_gen = "{end_gen}",
                callback = args_eval[[1]],
                code = code),

      list(block_id = NA,
           start_gen = NA,
           end_gen = NA,
           callback = NA,
           code = NA)
    )

  } else {

    block_row <- list(block_id = "",
                      start_gen = "1",
                      end_gen = "{end_gen}",
                      callback = "early()",
                      code = code)

  }

  if(all(sapply(block_row, is.na))) {
    stop("You have input an invalid combination of arguments, please see documentation of slim_block for details on how to specify its arguments.")
  }

  if(block_row$callback == "initialize()") {
    block_row$start_gen <- NA_character_
    block_row$end_gen <- NA_character_
  }

  block_row

}

#' slimrlang stub for the SLiM '.' operator
#'
#' Use this in place of '.' from SLiM to specify a method or a property coming from a
#' particular SLiM class. Note that the R function is a stub, it does not do anything in R
#' (except bring up this documentation). It will only do anything useful when used inside
#' a \\code{\\link{slim_block}} function further nested in a \\code{\\link{slim_script}}
#' function call, where it will be translated into valid SLiM code as part of a full SLiM script.
#'
#' @param lhs Object of class \code{rhs}, to extract methods or properties from
#' @param rhs SLiM class R object (such as \code{Subpopulation}, \code{.M}, etc.). Type \code{\link{slim_classes}} for a table of possible values.
#'
#' @export
#'
#' @section Copyright:
#'  This is documentation for a function in the SLiM software, and has been reproduced from the official manual,
#'  which can be found here: \\url{http://benhaller.com/slim/SLiM_Manual.pdf}. This documentation is
#'  Copyright © 2016–2020 Philipp Messer. All rights reserved. More information about SLiM can be found
#'  on the official website: \\url{https://messerlab.org/slim/}
#' @author Benjamin C Haller (\\email{bhaller@benhaller.com}) and Philipp W Messer (\\email{messer@cornell.edu})
`%.%` <- function(lhs, rhs) {
  print(paste0(lhs, ".", rhs))
  ?`%.%`
}

#' slimrlang stub for the first part of the SLiM ternary operator (\code{condition ? yes else no}).
#'
#' This is used in conjunction with %else% to use the SLiM ternary operator in \code{slimrlang}
#' which will make the code valid in R. Note that the R function is a stub, it does not do anything in R
#' (except bring up this documentation). It will only do anything useful when used inside
#' a \\code{\\link{slim_block}} function further nested in a \\code{\\link{slim_script}}
#' function call, where it will be translated into valid SLiM code as part of a full SLiM script.
#'
#' @param lhs A condition
#' @param rhs A SLiM expression to be executed if the condition is \code{TRUE}
#'
#' @export
#'
#' @section Copyright:
#'  This is documentation for a function in the SLiM software, and has been reproduced from the official manual,
#'  which can be found here: \\url{http://benhaller.com/slim/SLiM_Manual.pdf}. This documentation is
#'  Copyright © 2016–2020 Philipp Messer. All rights reserved. More information about SLiM can be found
#'  on the official website: \\url{https://messerlab.org/slim/}
#' @author Benjamin C Haller (\\email{bhaller@benhaller.com}) and Philipp W Messer (\\email{messer@cornell.edu})
`%?%` <- function(lhs, rhs) {
  print(paste0(lhs, " ? ", rhs))
  ?`%?%`
}

#' slimrlang stub for the second part of the SLiM ternary operator (\code{condition ? yes else no}).
#'
#' This is used in conjunction with %?% to use the SLiM ternary operator in \code{slimrlang}
#' which will make the code valid in R. Note that the R function is a stub, it does not do anything in R
#' (except bring up this documentation). It will only do anything useful when used inside
#' a \\code{\\link{slim_block}} function further nested in a \\code{\\link{slim_script}}
#' function call, where it will be translated into valid SLiM code as part of a full SLiM script.
#'
#' @param lhs A SLiM expression to be executed if the condition (before the companion %?%)
#' is \code{TRUE}
#' @param rhs A SLiM expression to be executed if the condition (before the companion %?%)
#' is \code{FALSE}
#'
#' @export
#'
#' @section Copyright:
#'  This is documentation for a function in the SLiM software, and has been reproduced from the official manual,
#'  which can be found here: \\url{http://benhaller.com/slim/SLiM_Manual.pdf}. This documentation is
#'  Copyright © 2016–2020 Philipp Messer. All rights reserved. More information about SLiM can be found
#'  on the official website: \\url{https://messerlab.org/slim/}
#' @author Benjamin C Haller (\\email{bhaller@benhaller.com}) and Philipp W Messer (\\email{messer@cornell.edu})
`%?%` <- function(lhs, rhs) {
  print(paste0(lhs, " ? ", rhs))
  ?`%?%`
}

slimr_template <- function(var_name, default = NULL) {
  .resources$temp_slimr_input$var_name <- c(.resources$temp_slimr_input$var_name,
                                   var_name)
  .resources$temp_slimr_input$default <- c(.resources$temp_slimr_input$default,
                                   default)
  rlang::sym(paste0("..", var_name, ".."))
}

tmplt <- function(var_name, default = NULL) {
  slimr_template(var_name, default)
}

tmplt_replace <- function(code) {
  code <- stringr::str_replace_all(code, "slimr_template", "!!slimr_template")
  code_expr <- rlang::parse_exprs(paste(code, collapse = ""))
  code <- purrr::map_chr(code_expr, ~rlang::expr_interp(.x) %>%
                           rlang::expr_deparse())
  code
}

gather_tmplt_one <- function(code_one) {
  code_one <- tmplt_replace(code_one)
  input_info <- list(var_names = .resources$temp_slimr_input$var_name,
                     defaults = .resources$temp_slimr_input$default)
  .resources$temp_slimr_input$var_name <- list()
  .resources$temp_slimr_input$default <- list()
  list(new_code = code_one, input_info = input_info)
}

gather_tmplt <- function(code) {
  res <- purrr::map(code,
                    ~gather_tmplt_one(.x))
  res
}
