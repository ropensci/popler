#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database.
#' @param browsed_data A on object produced by the function browse() 
#' @param subset A logical argument A string that to specify which columns to add in the default columns used in queries to the database. See Details for default columns.
#' @param add_columns A string to specify which columns the user wants to add in queries to the database 
#' @param subtract_columns A string to specify which, among the default columns, the user wishes to discard in queries to the database 
#' @return A data frame of the selected data.
#' @export
#' @examples
#' 
#' # browse a study, then get the data associated with it
#' grasshop = browse(proj_metadata_key == 21)
#' gh_data = get_data(grasshop)
#' 
#' # further subset this data set, based on year
#' gh_data_96_99 = get_data(grasshop, year > 1995 & year < 2000)
#' 
#' # insect data sets from the SEV lter site
#' insect_sev = browse(class == "Insecta" & lterid == "SEV")
#' insect_21_yrs96_99 = get_data(insect_sev, year > 1995 & year < 2000 & proj_metadata_key == 21)
#' 
#' insect_21_25 = get_data(subset = (proj_metadata_key == 21 | proj_metadata_key == 25) & year > 1995)


# Function that connects and gathers the information from the database
get_data <- function(browsed_data = NULL, subset = NULL,
                     add_columns = NULL, subtract_columns = NULL){
  
  
  # define possible columns ---------------------------------------------------------------
  
  # possible columns 
  potential_vars  <- popler:::query_cols()
  all_columns     <- potential_vars$all_cols
  default_columns <- potential_vars$default_cols
  
  
  # selected columns ----------------------------------------------------------------------
  # "inherit" variables from search arguments 
  inherit_vars    <- popler:::inherit_variables(browsed_data, substitute(subset), all_columns)

  # variables (de)selected explicitly
  actual_vars     <- unique( c(default_columns, add_columns, inherit_vars) )
  select_vars     <- paste( setdiff(actual_vars, subtract_columns), collapse = ", ")
  
  
  # subset argument(s) --------------------------------------------------------------------
  search_arg      <- popler:::subset_arguments(browsed_data, substitute(subset) )
  
  
  # query ---------------------------------------------------------------------------------
  conn <- src_postgres(
    dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")
  
  output_data <- popler:::query_popler(conn, select_vars, search_arg)
  
  # Change "ordr" and "clss" to "order" and "class"
  output_data <- popler:::class_order_names(output_data)
  
  # outputs -------------------------------------------------------------------------------
  
  # assign class
  output_data <- structure(output_data, 
                           unique_projects = unique(output_data$proj_metadata_key),
                           unique_authors  = unique(output_data[,c("proj_metadata_key",
                                                                   "authors",
                                                                   "authors_contact")]),
                           class = c("popler", class(output_data)) 
                           )
  
  # Informational message
  popler:::data_message(output_data)
  
  return(output_data)
  
}
