#' Browse the project metadata contained in the popler database
#'
#' browse() reports the metadata of LTER studies contained in the popler database. 
#' The user can subset what data and what columns to visualize.  
#' @param ... A logical expression to subset popler's main table
#' @param full_table Should the function return the standard columns, or the full main table?
#' @param variables A vector of characters that specifies which columns of popler's main table should be selected.
#' @param trim If TRUE, strings are truncated at the 50th character. Default is TRUE.
#' @param view If TRUE, opens up a spreadsheet-style data viewer. If view == "fix" it opens the data frame in a text editor rather than a spreadsheet-style viewer
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
#' three_columns = browse(variables = c("title","proj_metadata_key","genus","species"))


# The browse popler function
browse <- function(..., full_table = FALSE, variables = NULL, trim = TRUE, view = FALSE){

  # LOAD two object data types
  # Data table; convert factors to characters
  main_t        <- popler:::factor_to_character(popler:::main_popler)
  
  # Case insensitive matching ("lower" everything)
  names(main_t) <- tolower( names(main_t) )
  main_t        <- popler:::class_order(main_t)
  
  # Select by subset 
  subset_data <- popler:::select_by_criteria(main_t, substitute(...) )
  
  # select data based on 
  subset_data <- popler:::table_select(subset_data, full_table)
  
  # If no column specified, return all columns
  if( is.null(variables) ){
    out_cols <- subset_data
  } else{
    # Error message if column names are incorrect
    popler:::err_full_tab( variables,names(orig) )
    # If not, select said columns
    out_cols <- subset_data[,variables]
  }
  
  out_form <- popler:::elastic_tab(out_cols, shrink = TRUE, full_table)
  out_form <- popler:::trim_display(out_form, trim)
  
  # write output
  if(view == TRUE) View(out_form) ; if(view == "fix") fix(out_form) 
  
  # attribute class "popler"
  out            <- structure(out_form, 
                              class = c(class(out_form), "popler"),
                              search_argument = substitute(...) )
  
  return(out)
  
}
