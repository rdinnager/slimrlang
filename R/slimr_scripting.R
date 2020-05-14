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

  if(!is.call(args[[n_args]])) {
    stop("The last argument of slim_block should be a valid slimr_code block expression.")
  }

  if(n_args > 5) {
    stop("You've provided too many arguments. There shouldn't be more than 5 at max (block id, start generation, end generation, callback function, and slimr_code block expression)")
  }

  if(n_args > 1L) {
    other_args <- args[-n_args]
    args_eval <- lapply(other_args, eval, envir = callbacks)
    arg_signature <- lapply(args_eval, class)
    arg_signature <- lapply(arg_signature, function(x) ifelse(x == "integer", "numeric", x))

    arg_signature <- purrr::map_chr(arg_signature,
                                    ~stringr::str_sub(.x, 1, 2)) %>%
      paste0(collapse = "")

    code <- deparse(args[[n_args]])

    if(code[1] == "{") {
      code <- code[2:(length(code) - 1L)]
    }

    block_row <- switch(
      arg_signature,
      chnunuca = list(block_id = args_eval[[1]],
                      start_gen = args_eval[[2]],
                      end_gen = args_eval[[3]],
                      callback = args_eval[[4]],
                      code = list(code)),

      nunuca = list(block_id = "",
                    start_gen = args_eval[[1]],
                    end_gen = args_eval[[2]],
                    callback = args_eval[[3]],
                    code = list(code)),

      chnuca = list(block_id = args_eval[[1]],
                    start_gen = args_eval[[2]],
                    end_gen = "{end_gen}",
                    callback = args_eval[[3]],
                    code = list(code)),

      chnunu = list(block_id = args_eval[[1]],
                    start_gen = args_eval[[2]],
                    end_gen = args_eval[[3]],
                    callback = "early()",
                    code = list(code)),

      nunu = list(block_id = "",
                  start_gen = args_eval[[1]],
                  end_gen = args_eval[[2]],
                  callback = "early()",
                  code = list(code)),

      chnu = list(block_id = args_eval[[1]],
                  start_gen = args_eval[[2]],
                  end_gen = "{end_gen}",
                  callback = "early()",
                  code = list(code)),

      chca = list(block_id = args_eval[[1]],
                  start_gen = "1",
                  end_gen = "{end_gen}",
                  callback = args_eval[[2]],
                  code = list(code)),

      nuca = list(block_id = "",
                  start_gen = args_eval[[1]],
                  end_gen = "{end_gen}",
                  callback = args_eval[[2]],
                  code = list(code)),

      ch = list(block_id = args_eval[[1]],
                start_gen = "1",
                end_gen = "{end_gen}",
                callback = "early()",
                code = list(code)),

      nu = list(block_id = "",
                start_gen = args_eval[[1]],
                end_gen = "{end_gen}",
                callback = "early()",
                code = list(code)),

      ca = list(block_id = "",
                start_gen = "1",
                end_gen = "{end_gen}",
                callback = args_eval[[1]],
                code = list(code)),

      TRUE ~ list(block_id = NA,
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
                      code = list(code))

  }

  block_row

}

