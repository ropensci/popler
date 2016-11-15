#' Browse the project metadata contained in the popler database
#'
#' browse() reports the metadata of LTER studies contained in the popler database. 
#' The user can subset what data and what columns to visualize.  
#' @param criteria A logical expression to subset popler's main table
#' @param full_table Should the function return the standard columns, or the full main table?
#' @param select_columns A vector of characters that specifies which columns of popler's main table should be selected.
#' @param trim if TRUE, strings are truncated at the 50th character. Default is TRUE.
#' @return A data frame combining the metadata of each project and the taxonomic units associated with each project.
#' @export
#' @examples
#' # No arguments return the standard 16 columns of popler's main table
#' standard_columns = browse()
#' 
#' # full.table==T returns the full table
#' full_table = browse(full_table = TRUE)
#' 
#' # subset only data from the sevilleta LTER 
#' sev_data = browse(lterid == "SEV")
#' 
#' # consider only plant data sets 
#' plant_data = browse(kingdom == "Plantae")
#' 
#' # Select only the data you need
#' three_columns = browse(select_columns = c("title","genus","species"))


# The browse popler function
browse <- function(criteria = NULL, full_table = FALSE, select_columns = NULL, trim = TRUE){

  # Load main data table and convert factors to characters
  x           <- popler:::factor_to_character(popler:::main_popler)

  # Case insensitive matching ("lower" everything)
  names(x)    <- tolower(names(x))

  # Select by criteria 
  subset_data <- popler:::select_by_criteria(x, substitute(criteria))
  
  # select data based on 
  subset_data <- popler:::table_select(subset_data, full_table)
  
  # If no column specified, return all columns
  if( is.null(select_columns) ){
    out_cols <- subset_data
  } else{
    # Error message if column names are incorrect
    popler:::err_full_tab(select_columns,names(x))
    # If not, select said columns
    out_cols <- subset_data[,select_columns]
  }
  
  out      <- popler:::elastic_tab(out_cols, shrink = TRUE, full_table)
  out_trim <- popler:::trim_display(out, trim)
  
  # return output
  return(out_trim)

}
