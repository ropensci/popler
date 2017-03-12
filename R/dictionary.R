#' Dictionary of the popler metadata
#'
#' Provides information on the columns of metadata contained in the popler database, and the kind of data contained in those columns.
#' @param ... A sequence of (unquoted) variables specifying one or more columns of popler's main table for which dictionary information is needed
#' @param full_tbl Should the function return the standard columns, or the full main table?
#' @export
#' @examples
#' # Column names
#' column_names <- dictionary(full_tbl = FALSE)
#' 
#' # Dictionary information
#' dictionary_lter <- dictionary(lterid, full_tbl = FALSE)
#' 
#' # multiple columns
#' dictionary_lter_lat <- dictionary(lterid,lat_lter, full_tbl = FALSE)

dictionary <- function(..., full_tbl = FALSE){
  
  # Load main data table and convert factors to characters
  main_t        <- factor_to_character(main_popler)
  
  # Case insensitive matching ("lower" everything)
  names(main_t) <- tolower( names(main_t) )
  main_t        <- class_order_names(main_t)
  possible_arg  <- possibleargs
  vars          <- vars_dict(...)
  
  # if no column specified, return ALL column names
  if( is.null(vars) ){
    # select data based on 
    tmp   <- table_select(main_t, full_tbl, possible_arg)
    out   <- dictionary_explain(tmp)
  # if colums specified.
  } else {
    out   <- dict_list(main_t, vars)
  }
  
  return(out)
  
}
