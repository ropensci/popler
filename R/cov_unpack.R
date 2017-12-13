#' Unpack the covariates contained in downloaded data sets
#'
#' Create a data frame by "extracting" the \code{covariates} column
#' contained in an object downloaded with the \code{get_data} function.
#' @param x object produced by the function get_data(). \emph{NOTE}: temporarily, 
#' data can come from 1 study only. 
#' @return A data frame whose columns represent the covariates of the data set.
#'
#' @importFrom stringr str_split str_match
#' @importFrom dplyr %>%
#' @export
#' @examples
#' \dontrun{
#' demo_d <- get_data(proj_metadata_key == 8)
#' as.tbl( cov_unpack( demo_d ) )
#' }

# function to unpack covariates
cov_unpack <- function(x){
  
  # Extract all characters between the brackets and split the string on each comma (',')
  # followed by a space ('\\s')
  key_value_pair_dictionary_list <- stringr::str_split(
    trimws(stringr::str_match(x$covariates, "\\{(.*)\\}"))[,2], ",\\s", simplify = TRUE)
  
  # Extract values from dictionary syntax (key: value)
  value_data <- apply(
    gsub("\\'", "", key_value_pair_dictionary_list), 2, function(x) {
      stringr::str_match(x, "\\:\\s(.*)")[,2]
      }
    ) %>% data.frame()
  
  colnames(value_data) <- paste0(colnames(value_data) , '_value')
  
  
  # Extract keys from dicitonary
  key_data <- data.frame(apply(
    gsub("\\'", "", key_value_pair_dictionary_list), 2, function(x) {
      stringr::str_match(x, "(.*)\\:")[,2]}))
  colnames(key_data) <- paste0(colnames(key_data) , '_label')
  
  # combing them into a dataframe
  covariate_data <- cbind(value_data, key_data)
  # And ordering the columns by alphabetical order
  covariate_data <- covariate_data[ ,order(names(covariate_data))] ### END PRODUCT ####
  return(covariate_data)
}
