#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of factors (or columns) of that characterize datasets in the database.
#' @param criteria A logical expression to subset popler's main table
#' @param full.table Should the function return the standard columns, or the full main table?
#' @param select_columns A vector of characters that specifies which columns of popler's main table should be selected.
#' @return A data frame of the main table, or a subset thereof 
#' @export
#' @examples
#' # No arguments return the standard 16 columns of popler's main table
#' standard_columns = browse()
#' # full.table==T returns the full table
#' full_table = browse(full_table = TRUE)
#' # subset only data from the sevilleta LTER 
#' sev_data = browse(lterid == "SEV")
#' # consider only plant data sets 
#' plant_data = browse(kingdom == "Plantae")
#' # Select only the data you need
#' three_columns = browse(select_columns = c("title","genus","species"))


# The actual popler function
browse <- function(criteria = NULL, full_table = FALSE, select_columns = NULL){

  # Load main data table and convert factors to characters
  x           <- popler:::factor_to_character(popler:::dataPoplerFunction)

  # Case insensitive matching ("lower" everything)
  names(x)    <- tolower(names(x))

  # Select by criteria 
  subset_data <- popler:::select_by_criteria(x,substitute(criteria))
  
  # select data based on 
  subset_data <- popler:::table_select(subset_data, full_table)
  
  
  # If no column specified, return possible arguments 
  if( is.null(select_columns) ){
    out <- subset_data
  } else{
    # Error message if column names are incorrect
    popler:::err_full_tab(select_columns,names(x))
    # If not, select said columns
    out <- subset_data[,select_columns]
  }
   
  # return output
  return(as.data.frame(out))

}
