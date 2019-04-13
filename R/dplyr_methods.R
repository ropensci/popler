#' @noRd
reclass <- function(x, result) {
  UseMethod('reclass')
}

#' @noRd
reclass.browse <- function(.data, result) {
  
  class(result) <- unique(c(class(.data), class(result)))
  attr(result, 'search_expr') <- attr(.data, 'search_expr') 
  
  return(result)
  
}

#' @noRd
reclass.get_data <- function(.data, result) {
  # add class
  class(result) <- unique(c(class(.data), class(result)))
  
  # add attributes
  attributes(result) <- attributes(.data)
  
  return(result)
}

#' @noRd
new_dplyr_verb <- function(fun) {
  function(.data, ...) {
    result <- NextMethod()
    reclass(.data, result)
  }
}

#' @name reexports
#' @rdname reexports
NULL 

#' @importFrom dplyr filter
#' @export filter 
dplyr::filter

#' @importFrom dplyr mutate
#' @export mutate
dplyr::mutate

#' @rdname filter
#' @title Methods for dplyr verbs
#' @description Subsets a \code{browse} or \code{get_data} object based on
#' logical statements.
#'  
#' @param .data A \code{browse} or \code{get_data} object
#' @param ... logical conditions
#' 
#' @importFrom dplyr filter
#' @export
filter.browse <- new_dplyr_verb(dplyr::filter)

#' @rdname filter
#' @inheritParams filter.browse
#' @export
filter.get_data <- new_dplyr_verb(dplyr::filter)

#' @rdname mutate
#' @title Methods for dplyr verbs
#' @description Add new columns to a \code{browse} or \code{get_data} object.
#' 
#' @param .data A \code{browse} or \code{get_data} object
#' @param ... Name-value pairs of expressions. Use \code{NULL} to drop a 
#' variable.
#' 
#' @importFrom dplyr mutate
#' @export
mutate.browse <- new_dplyr_verb(dplyr::mutate)


#' @rdname mutate
#' @inheritParams mutate.browse
#' @export
mutate.get_data <- new_dplyr_verb(dplyr::mutate)

# credit here: 
# https://stackoverflow.com/questions/41967700/defining-custom-dplyr-methods-in-r-package/41968721

 