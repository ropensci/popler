#' Dictionary of the popler main table
#'
#' Provides information on the columns of the popler database, and the kind of data contained in those columns
#' @param select_columns One character string specifying one column of popler's main table for which dictionary information is needed
#' @param full.table Should the function return the standard columns, or the full main table?
#' @export
#' @examples
#' # Column names
#' column_names <- dictionary(select_columns = NULL, full.table = FALSE)
#' # Dictionary information
#' dictionary_lter <- dictionary(select_columns = "lterid", full.table = FALSE)
#' # multiple columns
#' dictionary_lter_lat <- dictionary(select_columns = c("lterid","lat_lter"), full.table = FALSE)

dictionary <- function(select_columns = NULL, full.table = FALSE){ 
  
  # Load main data table and convert factors to characters
  x <- popler:::factor_to_character(popler:::dataPoplerFunction)
  
  # Case insensitive matching ("lower" everything)
  names(x) <- tolower( names(x) )
  
  # select data based on 
  x <- popler:::full_table(x, full.table)
  
  # if no column specified, return ALL column names
  if( is.null(select_columns) ) { 
    out   <- names(x) 
  # if colums specified...
  } else {
    tmp   <- x[,select_columns,drop = FALSE]
    out   <- list()
    for(i in 1:length(select_columns)){
      
      # if numeric
      if( is.numeric(tmp[,i]) ) {
        out[[i]]  <- paste("numeric field: from",min(tmp[,i],na.rm = TRUE),
                           "to", max(tmp[,i],na.rm = TRUE))
      # if not numeric, return unique values
      } else {
        out[[i]]  <- unique(tmp[,i])
      }
      
    }
    
  } 
  
  return(out)
  
}
