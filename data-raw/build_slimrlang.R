## code to prepare `build_slimrlang` dataset goes here

library(dplyr)

slim_lang_txtfiles <- list.files("data-raw/build/slim_reference", full.names = TRUE)

initialize_methods_index <- grep("Initialize", slim_lang_txtfiles, fixed = TRUE)
initialize_methods_txtfile <- slim_lang_txtfiles[initialize_methods_index]
slim_lang_txtfiles <- slim_lang_txtfiles[-initialize_methods_index]

slim_lang_methods_txtfiles <- grep("_methods", slim_lang_txtfiles, value = TRUE, fixed = TRUE)
slim_lang_properties_txtfiles <- grep("_properties", slim_lang_txtfiles, value = TRUE, fixed = TRUE)

slim_lang_methods_txt <- purrr::map(slim_lang_methods_txtfiles,
                                        ~readr::read_lines(.x) %>%
                                          paste(collapse = "\n")) %>%
  setNames(basename(slim_lang_methods_txtfiles))

type_legend <- dplyr::tibble(abbr = c("v", "N", "i", "f", "l", "s", "n", "+", "*"),
                             type = c("void", "null", "integer", "float", "logical", "string",
                               "numeric", "any but object", "any"))

# arg_type <- func_data$return_type
collect_types <- function(arg_type) {
  obs <- stringr::str_match(arg_type, "o(bject)?<(.*?)>")
  new_arg_types <- stringr::str_remove_all(arg_type, "o(bject)?<(.*?)>")
  types <- stringr::str_match(new_arg_types, "void|integer|float|logical|string|numeric|\\+|\\*")
  types[ , 1] <- stringr::str_sub(types[ , 1], 1, 1)
  if(any(is.na(types[ , 1]))) {
    multiple_types <- stringr::str_match_all(new_arg_types, "(v|N|i|f|l|s)")
    multiple_types <- purrr::map(multiple_types,
                                 ~.x[ , 2])
    multiple_types[!is.na(types[ , 1])] <- types[!is.na(types[ , 1]), 1]
    types <- multiple_types

  } else {
    types <- list(types[ , 1])
  }

  types <- purrr::map_chr(types,
                          ~ dplyr::tibble(abbr = .x) %>%
                            dplyr::left_join(type_legend, by = "abbr") %>%
                            dplyr::pull(type) %>%
                            paste(collapse = " or "))

  types[types == ""] <- NA

  objects <- obs[ , 3]
  objects[!is.na(objects)] <- paste(objects[!is.na(objects)], "object")
  types <- cbind(types, objects)
  types <- apply(types, 1, function(x) paste(na.omit(x), collapse = " or "))
  types
}

#txt <- initialize_txt
#txt <- slim_lang_methods_txt[1]
extract_methods <- function(txt, init = FALSE) {

  txt <- stringr::str_replace_all(txt, "\nTOC.*?\n", "\n")
  txt <- stringr::str_replace_all(txt, "\nEidos events.*?\n", "\n")
  if(init) {
    txt <- stringr::str_replace_all(txt, "\n((?!\\((void|object|integer|float|logical|string|numeric|\\*|\\+)))", " \\1")
  } else {
    txt <- stringr::str_replace_all(txt, "\n([^–+])", " \\1")
  }
  txt <- stringr::str_split(txt, "\n")[[1]]

  if(init) {
    func_data <- unglue::unglue_data(txt,
                                     "({return_type}){function_name}({arguments}){description}")
  } else {
    func_data <- unglue::unglue_data(txt,
                                     "{symbol} ({return_type}){function_name}({arguments}){description}")
  }

  #args_txt <- func_data$arguments[[1]]
  gather_args <- function(args_txt) {
    arg_df <- dplyr::tibble(args = stringr::str_split(args_txt, stringr::fixed(", "))[[1]]) %>%
      dplyr::mutate(arg_num = 1:n())
    patterns <- unglue::unglue_data(arg_df$args,
                               c("[{arg_type} {arg_name} = {arg_default}]",
                                 "{arg_type} {arg_name}",
                                 "{arg_type}"))
    arg_df <- arg_df %>%
      dplyr::bind_cols(patterns)

    types <- collect_types(arg_df$arg_type)

    arg_df <- arg_df %>%
      dplyr::mutate(arg_type_desc = types) %>%
      dplyr::mutate(arg_singleton = ifelse(stringr::str_detect(arg_type, "\\$"),
                                           TRUE,
                                           FALSE))

    arg_df
  }

  func_data <- func_data %>%
    dplyr::mutate(arg_data = purrr::map(arguments,
                                        ~gather_args(.x)))

  func_data <- func_data %>%
    dplyr::mutate(return_type_desc = collect_types(return_type)) %>%
    dplyr::mutate(return_singleton = ifelse(stringr::str_detect(return_type, "\\$"),
                                         TRUE,
                                         FALSE))

  func_data
}

initialize_txt <- readr::read_lines(initialize_methods_txtfile) %>%
  paste(collapse = "\n")

initialize_methods_data <- extract_methods(initialize_txt, init = TRUE)

all_methods_data <- purrr::map(slim_lang_methods_txt,
                               ~extract_methods(.x, init = FALSE))
names(all_methods_data) <- stringr::str_remove(names(all_methods_data), "_methods.txt")

all_methods_data <- c(list(initialize_methods_data),
                      all_methods_data)

names(all_methods_data)[1] <- "Initialize"

usethis::use_data(all_methods_data, overwrite = TRUE, internal = TRUE)

####### generate roxygen docs #############

load("R/sysdata.rda")

func_template <- "
SLiM method <<function_name>>
Documentation for SLiM function \\code{<<function_name>>}, which is a method of the SLiM class \\code{<<class_name>>}.
<<params>>
@return An object of type <<return_type_desc>>. <<ifelse(return_singleton, 'Return will be of length 1 (a singleton)', '')>>
@details <<description>>
#..<<function_name>> <- function(<<paste(arg_data[[1]]$arg_name, collapse = ', ')>>) {
#..  <<class_abbr>>$<<function_name>>(<<paste(arg_data[[1]]$arg_name, collapse = ', ')>>)
#..}
#..
#..<<class_abbr>>$<<function_name>> <- function(<<paste(arg_data[[1]]$arg_name, collapse = ', ')>>) {
#..  ?<<function_name>>
#..}"

arg_roxy_template <- "
@param {arg_name} An object of type {arg_type_desc}. {ifelse(arg_singleton, 'Must be of length 1 (a singleton). ', ' ')} See details for description.
"

func_table <- all_methods_data$Chromosome[1, ]
class_name <- "Chromosome"
class_abbr <- "Ch"
make_slim_function <- function(func_table, class_name, class_abbr) {
  params <- purrr::map_chr(purrr::transpose(func_table$arg_data[[1]]),
                           ~glue::glue_data(.x, arg_roxy_template) %>%
                             stringr::str_wrap()) %>%
    paste(collapse = "\n")


  func_txt <- glue::glue_data(func_table, func_template, .open = "<<", .close = ">>")

  func_txt <- func_txt %>%
    stringr::str_split("\n") %>%
    purrr::map(~stringr::str_wrap(.x) %>%
                 stringr::str_split("\n") %>%
                 unlist() %>%
                 paste0("#'", .)) %>%
    unlist() %>%
    stringr::str_remove_all(stringr::fixed("#'#..")) %>%
    paste(collapse = "\n")

  func_txt


}

make_slim_function(func_table, class_name, class_abbr) %>%
  cat()



