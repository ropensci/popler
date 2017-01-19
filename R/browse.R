#' Browse the project metadata contained in the popler database
#'
#' browse() reports the metadata of LTER studies contained in the popler database. 
#' The user can subset what data and what columns to visualize.  
#' @param ... A logical expression to subset popler's main table
#' @param full_tbl Should the function return the standard columns, or the full main table?
#' @param vars A vector of characters: which variables of popler's main table should be selected?
#' @param trim If TRUE, strings are truncated at the 50th character. Default is TRUE.
#' @param view If TRUE, opens up a spreadsheet-style data viewer.
#' @param keyword A string that selects 
#' @return A data frame combining the metadata of each project and the taxonomic units associated with each project.
#' @return This data frame is of class "popler", "data.frame", "tbl_df", and "tbl".  
#' @export
#' @examples
#' # No arguments return the standard 16 columns of popler's main table
#' default_vars = browse()
#' 
#' # full.table==T returns the full table
#' all_vars = browse(full_tbl = TRUE)
#' 
#' # subset only data from the sevilleta LTER 
#' sev_data = browse(lterid == "SEV")
#' 
#' # consider only plant data sets 
#' plant_data = browse(kingdom == "Plantae")
#' 
#' # Select only the data you need
#' three_columns = browse(vars = c("title","proj_metadata_key","genus","species"))
#' 
#' # Select only the data you need
#' study_21 = browse( proj_metadata_key == 21)
#' 
#' # Select based on key words
#' parasite_npp = browse( grepl("parasit|npp",title,ignore.case=T) , trim = FALSE)
#' 
#' # More straighforward using keyword operator, %=%
#' parasite_npp = browse( title %=% c("parasit","npp"), trim = FALSE)


# The browse popler function
browse <- function(..., full_tbl = FALSE, vars = NULL, trim = TRUE, view = FALSE, keyword = NULL){

  # LOAD two object data types
  # Data table; convert factors to characters
  main_t        <- popler:::factor_to_character(popler:::main_popler)
  
  # Case insensitive matching ("lower" everything)
  names(main_t) <- tolower( names(main_t) )
  main_t        <- popler:::class_order_names(main_t)

  # Select by subset 
  sbst_popler   <- popler:::update_call( substitute(...) )
  key_subset    <- popler:::key_arg(main_t, keyword ,sbst_popler)
  subset_data   <- popler:::select_by_criteria(key_subset$tab, sbst_popler )
  
  # select data based on 
  possible_arg  <- popler:::possibleargs
  subset_data   <- popler:::table_select(subset_data, full_tbl, possible_arg)
  
  # If no column specified, return all columns
  if( is.null(vars) ){
    out_cols <- subset_data
  } else{
    # include proj_metadata_key if in vars
    vars      <- popler:::vars_check(vars)
    # Error message if column names are incorrect
    popler:::err_full_tab( vars, names(main_t), possible_arg )
    # If not, select said columns
    out_cols  <- subset_data[,vars]
  }
  
  out_form <- popler:::elastic_tab(out_cols, shrink = TRUE, full_tbl)
  out_form <- popler:::trim_display(out_form, trim)
  
  
  # write output
  if(view == TRUE) View(out_form)
  
  # attribute class "popler"
  out            <- structure(out_form, 
                              class = c("popler", class(out_form) ),
                              search_argument = c(sbst_popler,key_subset$s_arg)[[1]]
                              )
  
  return(out)
  
}
