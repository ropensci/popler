#' Count the number of observations in one or more of the popler database variables 
#'
#' summary_popler() counts the number of observations (e.g. number of species) contained in one or more of the popler database variables.
#' "Counts" refer to the number of unique occurrences in each field.  
#' Observations can be counted one by one, or by grouping them based on other variables (e.g. number of species, grouped by study) 
#' @param group_vars Variables used to group observation counts.
#' @param count_vars Names of variables whose observations will be counted. If no group variables defined, the function counts unique fields contained in the specified variable(s)
#' @param trim If TRUE, strings are truncated at the 25th character.
#' @return An object of class "tbl_df", "tbl", and "data.frame". 
#' @export
#' @examples
#' # Tallies without grouping factor
#' summary_popler(count_vars = "title", trim = TRUE)
#'          
#' # Number of species by study
#' summary_popler(group_vars = "title", 
#'                count_vars = "species", trim = TRUE)
#'          
#' # Number of studies by LTER site
#' summary_popler(group_vars = "lterid", 
#'                count_vars = "title", trim = TRUE)

# The summary_popler function
summary_popler <- function(group_vars = NULL, count_vars = "title", trim = TRUE){
  
  # read data
  browse_data <- popler:::factor_to_character(popler:::main_popler)
  browse_data <- popler:::class_order_names(browse_data)
  
  # tally cases, if tally_by is not NULL
  out <- popler:::tallies(browse_data, count_vars, group_vars, trim)
  
  return(out) # return output

}
