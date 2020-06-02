#' Copy original slimrlang code used to create a slimr_script object to the clipboard
#'
#' @param slimr_script_name Name of slimr_script to copy its original code
#' to the clipboard, as a string
#'
#' @return The code as a string (invisibly)
#' @export
#'
#' @examples
slimr_clip_original <- function(slimr_script_name = NULL) {
  assert_package("clipr")

  if(is.null(slimr_script_name)) {
    assert_package("rstudioapi")
    sel <- rstudioapi::getActiveDocumentContext()$selection
    slimr_script_name <- sel[[length(sel)]]$text %>%
      stringr::str_trim()
  }

  slimr_script <- get(slimr_script_name)

  orig_code <- deparse(attr(slimr_script, "slimrlang_orig"))
  clipr::write_clip(orig_code)
  invisible(orig_code)
}

#' Open original slimrlang code used to create a slimr_script object in a new document
#'
#' Open the original code used to create a slimr_script in a new document. Requires RStudio.
#'
#' @param slimr_script_name Name of slimr_script to open in a new document as a string.
#'
#' @return The code as a string (invisibly)
#' @export
#'
#' @examples
slimr_open_original <- function(slimr_script_name = NULL) {
  assert_package("rstudioapi")

  if(is.null(slimr_script_name)) {
    sel <- rstudioapi::getActiveDocumentContext()$selection
    slimr_script_name <- sel[[length(sel)]]$text %>%
      stringr::str_trim()
  }

  slimr_script <- get(slimr_script_name)

  orig_code <- deparse(attr(slimr_script, "slimrlang_orig"))
  rstudioapi::documentNew(paste(orig_code, collapse = "\n"))
  invisible(orig_code)
}
