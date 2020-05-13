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
  }

  arg_signature

}

