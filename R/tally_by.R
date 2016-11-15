#' Tally the number of instances in the popler database
#'
#' tally_by() tallies instances (e.g. number of species), in the popler database
#' Instances can be counted by themselves, or using grouping factors (e.g. species by study) 
#' @param x A data frame produced by the browse() function
#' @param tally_column Should the function return the standard columns, or the full main table?
#' @param group_factor grouping factor by which to count instances.
#' @param trim if TRUE, strings are truncated at the 50th character.
#' @return A data frame showing title's, or a subset thereof 
#' @export
#' @examples
#' # Tallies without grouping factor
#' tally_by(browse(), tally_columns = "title", trim = TRUE)
#'          
#' # Number of species by study
#' tally_by(browse(), tally_columns = "species",
#'          group_factors = "title", trim = TRUE)
#'          
#' # Number of studies by LTER site
#' tally_by(browse(), tally_columns = "title",
#'          group_factors = "lterid", trim = TRUE)

# The tally_by function
tally_by <- function(browse_data, tally_columns = "species", 
                     group_factors = NULL, trim = TRUE){
  
  # "Expand" table
  # standard table if ncol(.x) < 43
  if( ncol(browse_data) < 43){
    tmp <- popler:::elastic_tab(browse_data, shrink = FALSE, full_table = FALSE)
  # full table if ncol(.x) == 43
  } else {
    tmp <- popler:::elastic_tab(browse_data, shrink = FALSE, full_table = TRUE)
  }
  
  # tally cases, if tally_by is not NULL
  out <- popler:::tallies(tmp, tally_columns, group_factors, trim = TRUE)
  
  return(out) # return output

}

  