#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name slimrlang-vctrs
NULL

#' @export
new_slimr_code <- function(x = list()) {
  x <- assert_valid_code(x)
  new_list_of(x, ptype = character(), class = "slimr_code")
}

#' @export
slimr_code <- function(...) {
  x <- list(...)
  x <- lapply(x, vec_cast, character())
  new_slimr_code(x)
}

#' @export
vec_ptype_full.slimr_code <- function(x, ...) "slimr_code"
#' @export
vec_ptype_abbr.slimr_code <- function(x, ...) "s-code"

#' @export
format.slimr_code <- function(x, ...) {
  format_one <- function(x) {
    if (length(x) == 0) {
      return(prettycode::highlight("{}"))
    } else {
      prettycode::highlight(paste0("{", paste(stringr::str_trim(x), collapse = " "), "}"))
    }
  }
  purrr::map_chr(x,
                 ~format_one(.x))
}

#' @export
obj_print_data.slimr_code <- function(x, ...) {
  if (length(x) == 0)
    return()
  cat(format(x))
}

#' @export
vec_ptype2.slimr_code.slimr_code <- function(x, to, ...) x


#' @export
as.character.slimr_code <- function(x, ...) {
  purrr::map_chr(x,
                 ~paste(.x, collapse = "\n"))
}


#' @export
new_slimr_script <- function(block_name = character(),
                             block_id = character(),
                             start_gen = character(),
                             end_gen = character(),
                             callback = character(),
                             code = new_slimr_code(),
                             slimr_output = "none",
                             slimr_input = "none") {

  vec_assert(block_name, ptype = character())
  vec_assert(block_id, ptype = character())
  vec_assert(start_gen, ptype = character())
  vec_assert(end_gen, ptype = character())
  vec_assert(callback, character())
  vec_assert(code, new_slimr_code())

  new_rcrd(list(block_name = block_name,
                block_id = block_id,
                start_gen = start_gen,
                end_gen = end_gen,
                callback = callback,
                code = code),
           slimr_output = slimr_output,
           slimr_input = slimr_input,
           class = "slimr_script")
}

#' @export
vec_ptype_full.slimr_script <- function(x, ...) "slimr_script"
#' @export
vec_ptype_abbr.slimr_script <- function(x, ...) "s-scrpt"

#' @export
as.character.slimr_script <- function(x, ...) {
  code <- paste0(ifelse(is.na(field(x, "block_id")), "", paste0(field(x, "block_id"), " ")),
                 ifelse(is.na(field(x, "start_gen")), "", field(x, "start_gen")),
                 ifelse(is.na(field(x, "end_gen")), "", ":"),
                 ifelse(is.na(field(x, "end_gen")), "", field(x, "end_gen")),
                 " ",
                 field(x, "callback"),
                 " {\n",
                 purrr::map_chr(field(x, "code"), ~paste(.x, collapse = "\n")),
                 "\n}\n")
  code
}

#' @export
format.slimr_script <- function(x, add_block_names = TRUE, ...) {

  if(length(x) == 0) {
    return("{}")
  } else {

    string <- as.character(x)

    if(add_block_names) {
      string <- paste0(field(x, "block_name"), ":\n", string)
    }

  }

  string
}

#' @export
obj_print_data.slimr_script <- function(x, add_block_names = TRUE, suppress_cat = FALSE, ...) {
  if (length(x) == 0) {
    return()
  } else {

    string <- format(x, add_block_names)
    if(add_block_names) {
      string <- stringr::str_replace_all(string,
                                         "(block_(.*?)\\:)\n",
                                         glue::glue("<<crayon::bold$bgCyan('\\\\1')>>\n",
                                                    .open = "<<",
                                                    .close = ">>"))
    }

    code <- stringr::str_match_all(string,
                                   stringr::regex("\\{\n(.*)\n\\}$", dotall = TRUE))

    prettify_code <- function(code) {
      code <- stringr::str_split(code, "\n")[[1]] %>%
        prettycode::highlight() %>%
        paste(collapse = "\n")
      code
    }

    string <- stringr::str_replace_all(string,
                                       stringr::regex("\\{\n((.*))\n\\}$", dotall = TRUE),
                                       prettify_code)

    string <- paste(string, collapse = "\n")

  }
  if(!suppress_cat) {
    cat(string)
  }
  return(invisible(string))

}

#' @export
get_block <- function(x, i) {
  #vec_assert(x, new_slimr_script())
  vec_slice(x, vec_as_location(i, vec_size(x), names = field(x, "block_name")))
}

#' @export
code.slimr_script <- function(x) {
  field(x, "code")
}


#' @export
new_slimr_script_coll <- function(x = list()) {
  new_list_of(x, ptype = new_slimr_script(), class = "slimr_script_coll")
}

#' @export
slimr_script_coll <- function(...) {
  x <- list(...)
  x <- lapply(x, vec_cast, new_slimr_script())
  new_slimr_script_coll(x)
}

#' @export
format.slimr_script_coll <- function(x, add_block_names = TRUE, ...) {

  if(length(x) == 0) {
    return("{}")
  } else {

    string <- vapply(x, format, character(), add_block_names = add_block_names)

  }

  paste(string, collapse = "\n\n")
}

#' @export
obj_print_data.slimr_script_coll <- function(x, add_block_names = TRUE, ...) {

  if(length(x) == 0) {
    return("{}")
  } else {

    string <- vapply(x, obj_print_data, character(1),
                     add_block_names = add_block_names,
                     suppress_cat = TRUE)

  }

  string <- paste(string, collapse = "\n\n")
  cat(string)
}


