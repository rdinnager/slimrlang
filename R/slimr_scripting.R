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
    new_slim_script(nrow = n_row) %>%
    dplyr::mutate_all(~unlist(.x, recursive = FALSE)) %>%
    dplyr::mutate(block_name = paste0("block_", stringr::str_pad(seq_len(n_row),
                                                                nchar(trunc(n_row)),
                                                                pad = "0"))) %>%
    dplyr::mutate(block_name = ifelse(.data$callback == "initialize()",
                                      "block_init",
                                      .data$block_name)) %>%
    dplyr::select(block_name,
                  block_id,
                  start_gen,
                  colon,
                  end_gen,
                  callback,
                  code) %>%
    new_slim_script(nrow = n_row)

  suppressWarnings(end_gen <- max(as.numeric(c(script$start_gen, script$end_gen)), na.rm = TRUE))

  script$end_gen <- purrr::map_chr(script$end_gen,
                              ~glue::glue(.x, .na = NULL) %>%
                                as.character())


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

    if(any(unlist(arg_signature) == "name")) {
      if(any(unlist(arg_signature)[unlist(arg_signature) == "name"] != "")) {
        stop(paste0("Invalid use of a name in arguments. Problem argument(s) are ",
                    paste(which(unlist(arg_signature)[unlist(arg_signature) == "name"] != ""))))
      }
    }

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
                      start_gen = as.character(args_eval[[2]]),
                      colon = ":",
                      end_gen = as.character(args_eval[[3]]),
                      callback = args_eval[[4]],
                      code = list(code)),

      chnunaca = list(block_id = args_eval[[1]],
                      start_gen = as.character(args_eval[[2]]),
                      colon = ":",
                      end_gen = {end_gen},
                      callback = args_eval[[4]],
                      code = list(code)),

      nunuca = list(block_id = "",
                    start_gen = as.character(args_eval[[1]]),
                    colon = ":",
                    end_gen = as.character(args_eval[[2]]),
                    callback = args_eval[[3]],
                    code = list(code)),

      nunaca = list(block_id = "",
                    start_gen = as.character(args_eval[[1]]),
                    colon = ":",
                    end_gen = "{end_gen}",
                    callback = args_eval[[3]],
                    code = list(code)),

      chnuca = list(block_id = args_eval[[1]],
                    start_gen = as.character(args_eval[[2]]),
                    colon = "",
                    end_gen = NA,
                    callback = args_eval[[3]],
                    code = list(code)),

      chnunu = list(block_id = args_eval[[1]],
                    start_gen = as.character(args_eval[[2]]),
                    colon = ":",
                    end_gen = as.character(args_eval[[3]]),
                    callback = "early()",
                    code = list(code)),

      chnuna = list(block_id = args_eval[[1]],
                    start_gen = as.character(args_eval[[2]]),
                    colon = ":",
                    end_gen = "{end_gen}",
                    callback = "early()",
                    code = list(code)),

      nunu = list(block_id = "",
                  start_gen = as.character(args_eval[[1]]),
                  colon = ":",
                  end_gen = as.character(args_eval[[2]]),
                  callback = callbacks$early(),
                  code = list(code)),

      nuna = list(block_id = "",
                  start_gen = as.character(args_eval[[1]]),
                  colon = ":",
                  end_gen = "{end_gen}",
                  callback = callbacks$early(),
                  code = list(code)),

      chnu = list(block_id = args_eval[[1]],
                  start_gen = as.character(args_eval[[2]]),
                  colon = "",
                  end_gen = NA,
                  callback = callbacks$early(),
                  code = list(code)),

      chca = list(block_id = args_eval[[1]],
                  start_gen = "1",
                  colon = ":",
                  end_gen = "{end_gen}",
                  callback = args_eval[[2]],
                  code = list(code)),

      nuca = list(block_id = "",
                  start_gen = as.character(args_eval[[1]]),
                  colon = "",
                  end_gen = NA,
                  callback = args_eval[[2]],
                  code = list(code)),

      ch = list(block_id = args_eval[[1]],
                start_gen = "1",
                colon = ":",
                end_gen = "{end_gen}",
                callback = callbacks$early(),
                code = list(code)),

      nu = list(block_id = "",
                start_gen = as.character(args_eval[[1]]),
                colon = "",
                end_gen = NA,
                callback = callbacks$early(),
                code = list(code)),

      ca = list(block_id = "",
                start_gen = "1",
                colon = ":",
                end_gen = "{end_gen}",
                callback = args_eval[[1]],
                code = list(code)),

      list(block_id = NA,
           start_gen = NA,
           colon = NA,
           end_gen = NA,
           callback = NA,
           code = NA)
    )

  } else {

    block_row <- list(block_id = "",
                      start_gen = "1",
                      colon = ":",
                      end_gen = "{end_gen}",
                      callback = "early()",
                      code = list(code))

  }

  if(all(sapply(block_row, is.na))) {
    stop("You have input an invalid combination of arguments, please see documentation of slim_block for details on how to specify its arguments.")
  }

  if(block_row$callback == "initialize()") {
    block_row$start_gen <- NA
    block_row$end_gen <- NA
    block_row$colon <- ""
  }

  block_row

}

#' slim_script constructor and validator
#'
#' Constructs or Validates a `slim_script` object. This is mostly for internal use.
#'
#' @param x A named list where each element is a list or vector component of a `slim_script`,
#' which includes "block_name", "block_id", "start_gen", "colon", "end_gen", "callback", and "code"
#' @param ... Optional attributes to add to the `slim_script` object, as name-value pairs.
#' @param nrow The number of rows, required
#' @param slim_output Optional `slim_output` attribute
#' @param slim_input Optional `slim_input` attribute
#'
#' @export
#' @rdname new_slim_script
#'
#' @examples
new_slim_script <- function(x, ..., nrow, slim_output = "None", slim_input = "None") {
  tibble::new_tibble(x,
                     ...,
                     nrow = nrow,
                     class = "slim_script")
}

#' slim_script constructor and validator
#'
#' @param slim_script A `slim_script` object
#'
#' @export
#' @rdname new_slim_script
#'
#' @examples
validate_slim_script <- function(slim_script) {
  tibble::validate_tibble(x)
}

#' Print a slim_script object with highlighting
#'
#' @param x slim_script object to print.
#'
#' @return The formatted string (invisibly)
#' @export
#'
#' @examples
#' print(slimr::slim_get_recipe() %>% slimr::slim_script_from_text())
print.slim_script <- function(x, ...) {

  # code <- purrr::map(x$code,
  #                    ~slim_code_Rify(.x))
  code_pretty <- purrr::map(x$code,
                            ~prettycode::highlight(.x))
  # code_pretty <- purrr::map(code_pretty,
  #                           ~slim_code_SLiMify(.x))
  x$code <- code_pretty
  string <- as.character(x)

  # string <- purrr::map(code,
  #                      ~stringr::str_replace_all(.x, "([-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)",
  #                                                "<highlight>crayon::green('\\1')</highlight>"))

  names(string) <- x$block_name

  string <- purrr::imap_chr(string,
                            ~purrr::assign_in(.x, 1, paste0("<highlight>crayon::bold$bgCyan('", .y, "')</highlight> ", .x[1])) %>%
                              paste(collapse = "\n")) %>%
    paste(collapse = "\n")


  string <- glue::glue(string, .open = "<highlight>", .close = "</highlight>")

  cat(string)

  return(invisible(string))


}

#' Convert a slim_script object into text.
#'
#' @param slim_script
#'
#' @return
#' @export
#'
#' @examples
as.character.slim_script <- function(x) {
  code <- paste0(ifelse(is.na(x$block_id), "", paste0(x$block_id, " ")),
                 ifelse(is.na(x$start_gen), "", x$start_gen),
                 x$colon, ifelse(is.na(x$end_gen), "", x$end_gen),
                 " ",
                 x$callback,
                 " {\n",
                 purrr::map_chr(x$code, ~paste(paste0("\t", .x), collapse = "\n")),
                 "\n}\n")
  code
}
