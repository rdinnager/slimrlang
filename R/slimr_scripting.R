#' Create a SLiMR script
#'
#' Setup a SLiMR script. Each argument should be a call to \code{\link{slim_block}}.
#' See details for more information.
#'
#' @param ... A list of \code{\link{slim_block}} objects comprising a SLiM script
#' (written in slimr code)
#'
#' @return A \code{slim_script} object that can be used with \code{slimr}'s
#' \code{\link[slimr]{slim_run_script}} or converted into a text file for use with
#' SLiM directly using \code{\link{as.character}}.
#' @export
#'
#' @examples
#'slim_script(
#'    slim_block(initialize(),
#'           {
#'             initializeMutationRate(1e-7);
#'             initializeMutationType("m1", 0.5, "f", 0.0);
#'             initializeGenomicElementType("g1", m1, 1.0);
#'             initializeGenomicElement(g1, 0, 99999);
#'             initializeRecombinationRate(1e-8);
#'           }),
#'    slim_block(1,
#'           {
#'             sim.addSubpop("p1", 500);
#'           }),
#'    slim_block(10000,
#'           {
#'             sim.simulationFinished();
#'           })
#')
slim_script <- function(...) {
  script_list <- list(...)

  classes <- purrr::map_lgl(script_list,
                                 ~inherits(.x, "slimr_block"))

  if(!any(classes)) {
    stop("slim_script only accepts arguments that are slim_block function calls or their results.")
  }

  .call <- sys.call()

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

  if(!"block_init" %in% block_names) {
    warning("The arguments do not include an initialize block (use an initialize()
            callback to create one) and so the resulting script will not be a valid SLiM
            script. You can add an initialize block later through concatenation (using c())")
  }


  suppressWarnings(end_gen <- max(as.numeric(c(script$start_gen, script$end_gen)), na.rm = TRUE))
  if(is.infinite(end_gen)) {
    end_gen <- ""
  }

  script$end_gen <- purrr::map_chr(script$end_gen,
                              ~glue::glue(.x, .na = NULL) %>%
                                as.character())

  code <- vec_unchop(script$code)

  ## process forcing operators
  code <- process_forcing(code)

  ## process for template
  c(code, slimr_template_attr) %<-% process_template(code, block_names)

  code <- new_slimr_code(code)

  ## process for output

  c(code, slimr_output_attr) %<-% process_output(code, block_names)

  code <- purrr::map(code,
                     ~unlist(.x))

  #code <- SLiMify_all(code)


  code <- new_slimr_code(code)



  script <- new_slimr_script(block_name = block_names,
                             block_id = script$block_id,
                             start_gen = script$start_gen,
                             end_gen = script$end_gen,
                             callback = script$callback,
                             code = code,
                             slimr_output = slimr_output_attr,
                             slimr_template = slimr_template_attr,
                             slimrlang_orig = .call,
                             script_info = list(end_gen = as.numeric(end_gen)))

  script
}

#' Setup a SLiM code block
#'
#' @param ... A list of arguments corresponding to elements in SLiM code blocks.
#' See details for more information on how to specify these arguments.
#'
#' @return A slimr_block object. This is of little use outside a \code{\link{slim_script}}
#' function call.
#' @export
#'
#' @examples
#' slim_script(slim_block({print("Hello World!")}))
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

  #code <- SLiMify(code)

  code <- new_slimr_code(list(code))

  if(n_args > 1L) {
    other_args <- args[-n_args]

    ddots <- unlist(lapply(other_args, function(x) class(x) == "name" & x == ".."))
    if(sum(ddots) > 1) {
      stop(".. can only be used once in a slim_block function call")
    }

    if(sum(ddots) == 1) {
      other_args[[which(ddots)]] <- ".."
    }

    args_eval <- lapply(other_args, eval, envir = callbacks)
    if(sum(ddots) == 1) {
      class(args_eval[[which(ddots)]]) <- "na"
    }
    arg_signature <- lapply(args_eval, class)
    arg_signature <- lapply(arg_signature, function(x) ifelse(x == "integer", "numeric", x))

    if(any(unlist(arg_signature) == "name")) {
      if(any(unlist(arg_signature)[unlist(arg_signature) == "name"] != "")) {
        stop(paste0("Invalid use of a name in arguments. Problem argument(s) are ",
                    paste(which(unlist(arg_signature)[unlist(arg_signature) == "name"] != ".."))))
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
                      end_gen = "{end_gen}",
                      callback = args_eval[[4]],
                      code = code),

      nunuca = list(block_id = NA_character_,
                    start_gen = as.character(args_eval[[1]]),
                    end_gen = as.character(args_eval[[2]]),
                    callback = args_eval[[3]],
                    code = code),

      nunaca = list(block_id = NA_character_,
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

      nunu = list(block_id = NA_character_,
                  start_gen = as.character(args_eval[[1]]),
                  end_gen = as.character(args_eval[[2]]),
                  callback = callbacks$early(),
                  code = code),

      nuna = list(block_id = NA_character_,
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

      nuca = list(block_id = NA_character_,
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

      nu = list(block_id = NA_character_,
                start_gen = as.character(args_eval[[1]]),
                end_gen = NA_character_,
                callback = callbacks$early(),
                code = code),

      ca = list(block_id = NA_character_,
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

  class(block_row) <- "slimr_block"

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
#' @param replace_NAs Should \code{NA} values in the template be replaced by their default values?
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


