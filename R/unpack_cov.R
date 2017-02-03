#' Unpack the covariates for a data set
#'
#' Obtain author name and email from a data object downloaded from popler.
#' @param An object produced by the function get_data(). NOTE: temporarily, data can come from 1 study only. 
#' @return A data frame whose columns represent the covariates of the data set.
#' @export
#' @examples
#' 
#' # get author names and email from studies containing data from the genus Poa
#' demo_d <- get_data(proj_metadata_key == 8)
#' as.tbl( unpack_cov( demo_d ) )


# function to unpack covariates
unpack_cov <- function(x){
  
  # Extract all characters between the brackets and split the string on each comma (',')
  # followed by a space ('\\s')
  key_value_pair_dictionary_list <- str_split(
    trimws(str_match(x$covariates, "\\{(.*)\\}"))[,2], ",\\s", simplify = T)
  
  # Extract values from dictionary syntax (key: value)
  value_data <- data.frame(apply(
    gsub("\\'", "", key_value_pair_dictionary_list), 2, function(x) {
      str_match(x, "\\:\\s(.*)")[,2]}))
  colnames(value_data) <- paste0(colnames(value_data) , '_value')
  
  
  # Extract keys from dicitonary
  key_data <- data.frame(apply(
    gsub("\\'", "", key_value_pair_dictionary_list), 2, function(x) {
      str_match(x, "(.*)\\:")[,2]}))
  colnames(key_data) <- paste0(colnames(key_data) , '_label')
  
  # combing them into a dataframe
  covariate_data <- cbind(value_data, key_data)
  # And ordering the columns by alphabetical order
  covariate_data <- covariate_data[, order(names(covariate_data))] ### END PRODUCT ####
  return(covariate_data)
}
