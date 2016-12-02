#' Get author information from a data object
#'
#' Obtain author name and email from a data object downloaded from popler.
#' @param data_object An object produced by the function get_data().
#' @return A data frame with project IDs, names of the authors of each data set, and their email.
#' @export
#' @examples
#' 
#' # get author names and email from studies containing data from the genus Poa
#' poa_data = get_data(subset = genus == "Poa")
#' authors( poa_data )

# function definition
authors <- function( data_object ){
  return( as.tbl(attributes( data_object )$unique_authors) )
}
