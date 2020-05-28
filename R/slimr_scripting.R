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

  ## process for template
  c(code, slimr_template_attr) %<-% process_template(code, block_names)

  code <- new_slimr_code(code)

  script <- new_slimr_script(block_name = block_names,
                             block_id = script$block_id,
                             start_gen = script$start_gen,
                             end_gen = script$end_gen,
                             callback = script$callback,
                             code = code,
                             slimr_template = slimr_template_attr)

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
  .resources$temp_slimr_template$var_name <- c(.resources$temp_slimr_template$var_name,
                                   var_name)
  .resources$temp_slimr_template$default <- c(.resources$temp_slimr_template$default,
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
  .resources$temp_slimr_template$var_name <- list()
  .resources$temp_slimr_template$default <- list()
  code_one <- tmplt_replace(code_one)
  input_info <- list(var_names = .resources$temp_slimr_template$var_name,
                     defaults = .resources$temp_slimr_template$default)
  list(new_code = code_one, input_info = input_info)
}

gather_tmplt <- function(code) {
  res <- purrr::map(code,
                    ~gather_tmplt_one(.x))
  res
}

process_template <- function(code, block_names) {
  template_processed <- gather_tmplt(as.character.slimr_code(code)) %>%
    purrr::transpose()

  slimr_template_attr <- purrr::transpose(template_processed$input_info) %>%
    tibble::as_tibble() %>%
    dplyr::mutate("block_name" := block_names) %>%
    tidyr::unnest(c(var_names, defaults),
                  keep_empty = TRUE) %>%
    dplyr::mutate_at(c("var_names", "defaults"),
                     ~purrr::map(.,
                          ~ purrr::`%||%`(.x, NA))) %>%
    dplyr::mutate_at(c("var_names"),
                     ~vec_unchop(.))

  new_code <- SLiMify_all(template_processed$new_code)

  list(new_code, slimr_template_attr)
}


#' Render a SLiM script with special slimrlang formatting
#'
#' If your \code{slimr_script} object has made use of special \code{slimrlang}
#' syntax such as \code{\link{slimr_template}}, \code{\link{slimr_input}},
#' or \code{\link{slimr_output}}, this function will 'render' the \code{slimr_script}
#' into valid SLiM syntax, ready to be run with SLiM or \code{\link[slimr]{slim_run_script}}
#'
#' @param slimr_script The \code{slimr_script} object to be rendered
#' @param template A list or data.frame containing values for any templated variables. If a list,
#' it must be named, where the names correspond to the variables. If a list of lists, the internal
#' lists must be names with the variable names, and \code{slimr_script_render} will render a
#' separate \code{slimr_script} for each top-level lsit element and return it as a \code{slimr_script_coll}
#' object. If a \code{data.frame} (or \code{tibble}), then the column names should match the templated
#' variables, and \code{slimr_script_render} will render a separate \code{slim_script} for each row
#' and return it as a \code{slimr_script_coll} object.
#'
#' @return
#' @export
#'
#' @examples
slimr_script_render <- function(slimr_script, template = NULL, replace_NAs = FALSE) {
  list_length_1 <- FALSE
  slimr_template_attr <- attr(slimr_script, "slimr_template")
  if(any(!is.na(slimr_template_attr$var_names))) {
    if(is.null(template)) {
      stop("This slimr_script has templating.. You must provide a template argument, which can be a list, a data.frame, or an environment")
    }
    if(inherits(template, "data.frame")) {
      template <- purrr::transpose(template)
    } else {
      if(!inherits(template, "list")) {
        stop("The template argument must be a list or inherit from a data.frame")
      }
    }

    if(!inherits(template[[1]], "list")) {
      list_length_1 <- TRUE
      template <- list(template)
    }

    new_scripts <- purrr::map(template,
                              ~replace_double_dots(slimr_script,
                                                   .x,
                                                   slimr_template_attr = slimr_template_attr,
                                                   replace_NAs = replace_NAs))
    if(list_length_1) {
      new_scripts <- new_scripts[[1]]
    }

  }

  if(!list_length_1) {
    new_scripts <- new_slimr_script_coll(new_scripts)
  }

  new_scripts

}

replace_double_dots <- function(slimr_script, envir = parent.frame(), slimr_template_attr, replace_NAs) {

  templated <- !is.na(slimr_template_attr$var_names)
  templated_vars <- slimr_template_attr$var_names[templated]
  not_specified <- !(templated_vars %in% names(envir))

  if(any(not_specified)) {
    defaults <- slimr_template_attr$defaults[templated][not_specified]
    missing_defaults <- purrr::map_lgl(defaults,
                                       ~is.na(.x))

    if(any(missing_defaults)) {
      stop("Some templated variables have not been fully specified in template, and no default was provided")
    } else {
      new_envir <- defaults
      names(new_envir) <- templated_vars[not_specified]
      envir <- c(envir, new_envir)
      warning("Warning: A templated variable was not specified in the template and has been replaced by its default value.\n")
    }

  }

  missing_dat <- purrr::map_lgl(envir,
                                ~is.na(.x))
  if(any(missing_dat)) {
    if(replace_NAs) {
      the_defaults <- slimr_template_attr$defaults[templated]
      names(the_defaults) <- templated_vars
      envir[missing_dat] <- the_defaults[names(envir)[missing_dat]]
      warning("Warning: There are missing values in template and replace_NAs = TRUE, so they will be replaced by their defaults\n")
    } else {
      warning("Warning: There are missing values in template and replace_NAs = FALSE, so the rendered script will have NA values\n")
    }
  }

  code_text <- as.character.slimr_code(code(slimr_script))
  new_code <- purrr::map(code_text,
                         ~glue::glue(.x,
                                     .envir = envir,
                                     .open = "..",
                                     .close = "..") %>%
                           stringr::str_split("\n") %>%
                           unlist())

  block_names <- field(slimr_script, "block_name")

  c(new_code, slimr_template_attr) %<-% process_template(new_code, block_names)

  new_code <- new_slimr_code(new_code)
  slimr_script <- new_slimr_script(block_name = block_names,
                                   block_id = field(slimr_script, "block_id"),
                                   start_gen = field(slimr_script, "start_gen"),
                                   end_gen = field(slimr_script, "end_gen"),
                                   callback = field(slimr_script, "callback"),
                                   code = new_code,
                                   slimr_template = slimr_template_attr)
  slimr_script
}

#' Get information on templating in a slimr_script
#'
#' Returns information on templated variables and their default values in a \code{slimr_script}
#'
#' @param script_temp A templated \code{slimr_script} to retrieve information from
#'
#' @return A list of lists. The top-level is named for the blocks in which templated variables exist.
#' For each block with templated variables the element is a list named with all templated variables
#' in that block, and its values are equal to the default values for those variables.
#' @export
#'
#' @examples
slimr_template_info <- function(script_temp) {
  slimr_template_attr <- attr(script_temp, "slimr_template")
  if(any(!is.na(slimr_template_attr$var_names))) {
    info_group <- slimr_template_attr %>%
      dplyr::group_by(!! rlang::sym("block_name"))

    temp_split <- dplyr::group_split(info_group)
    temp_names <- dplyr::group_keys(info_group)

    temp_info <- purrr::map(temp_split,
                            ~.x$defaults %>%
                              setNames(.x$var_names))

    names(temp_info) <- temp_names$block_name

    temp_info <- purrr::map(temp_info,
                            ~if(all(is.na(names(.x)))) {
                              NULL
                            } else {
                              .x
                            }) %>%
      purrr::compact()

    temp_info
  } else {
    NULL
  }
}
