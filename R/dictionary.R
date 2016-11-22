#' Dictionary of the popler metadata
#'
#' Provides information on the columns of metadata contained in the popler database, and the kind of data contained in those columns.
#' @param variables A character string specifying one column of popler's main table for which dictionary information is needed
#' @param full_table Should the function return the standard columns, or the full main table?
#' @export
#' @examples
#' # Column names
#' column_names <- dictionary(variables = NULL, full_table = FALSE)
#' 
#' # Dictionary information
#' dictionary_lter <- dictionary(variables = "lterid", full_table = FALSE)
#' 
#' # multiple columns
#' dictionary_lter_lat <- dictionary(variables = c("lterid","lat_lter"), full_table = FALSE)

dictionary <- function(variables = NULL, full_table = FALSE){
  
  # Load main data table and convert factors to characters
  main_t        <- popler:::factor_to_character(popler:::main_popler)
  
  # Case insensitive matching ("lower" everything)
  names(main_t) <- tolower( names(main_t) )
  main_t        <- popler:::class_order(main_t)
  
  # if no column specified, return ALL column names
  if( is.null(variables) ){
    # select data based on 
    tmp   <- popler:::table_select(main_t, full_table)
    out   <- popler:::dictionary_explain(tmp)
  # if colums specified.
  } else {
    out   <- popler:::dict_list(main_t, variables)
  }
  
  return(out)
  
}
