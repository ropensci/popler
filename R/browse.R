#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of factors (or columns) of that characterize datasets in the database.
#' @param select_columns A vector of characters that specifies which columns of popler's main table should be selected.
#' @param full.table Should the function return the standard columns, or the full main table?
#' @return A data frame of the main table, or a subset thereof 
#' @export
#' @examples
#' # No arguments return the standard 16 columns of popler's main table
#' browse()
#' # full.table==T returns the full table
#' browse(full.table = T)
#' # Select only the data you need
#' browse(c("title","genus","species"), full.table = T)


# The actual popler function
browse <- function( select_columns = NULL, full.table = F ){

  # Load main data table and convert factors to characters
  x <- popler:::factor_to_character(popler:::dataPoplerFunction)

  # Case insensitive matching ("lower" everything)
  names(x) <- tolower(names(x))
  #if(!is.null(group_factors)) group_factors=tolower(group_factors)
  #if(!is.null(tally_by))  tally_by=tolower(tally_by)

  
  # If full.table is FALSE, consider only the 17 standard columns ----------------
  if( full.table == F) {
    
    # Initial group_factors
    possibleargs <- tolower(c("lterid","title",
                              "datatype","studytype","duration_years",
                              "community","structured",
                              "lat_lter","lng_lter",
                              "species","kingdom","phylum","clss","ordr","family","genus"))
  
    # If no column specified, return possible arguments 
    if( is.null(select_columns) )   out <- x[,possibleargs]
    # If columns selected, return those columns (provided no mistake in column names)
    if( !is.null(select_columns) )  {
      
      # Error message if column names are incorrect
      popler:::err_standard_tab(select_columns,possibleargs)
      # If not, select said columns
      out <- x[,select_columns]
      
    }
    
    
  # If full.table is TRUE, consider all 62 columns ----------------------------
  } else {
    
    # is no columns selected, return ALL columns
    if( is.null(select_columns) ){   
      out <- x
    }
    # return selected columns (provided no mistake in column names)
    if( !is.null(select_columns) ){
      
      err_full_tab(select_columns, names(x))
      out <- x[,select_columns]
      
    }
    
  }
   
  # return output
  return(out) 

}
