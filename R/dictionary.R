#' Dictionary of the popler metadata
#'
#' Provides information on the columns of metadata contained in the popler database, and the kind of data contained in those columns.
#' @param select_columns A character string specifying one column of popler's main table for which dictionary information is needed
#' @param full_table Should the function return the standard columns, or the full main table?
#' @export
#' @examples
#' # Column names
#' column_names <- dictionary(select_columns = NULL, full_table = FALSE)
#' 
#' # Dictionary information
#' dictionary_lter <- dictionary(select_columns = "lterid", full_table = FALSE)
#' 
#' # multiple columns
#' dictionary_lter_lat <- dictionary(select_columns = c("lterid","lat_lter"), full_table = FALSE)

dictionary <- function(select_columns = NULL, full_table = FALSE){ 
  
  # Load main data table and convert factors to characters
  x <- popler:::factor_to_character(popler:::main_popler)
  
  # Case insensitive matching ("lower" everything)
  names(x) <- tolower( names(x) )
  
  # select data based on 
  x <- popler:::table_select(x, full_table)
  
  # if no column specified, return ALL column names
  if( is.null(select_columns) ) {
    out   <- names(x)
  # if colums specified...
  } else {
    out   <- popler:::dict_list(x, select_columns)
  } 
  
  return(out)
  
}
