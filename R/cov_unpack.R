#' @title Unpack the covariates contained in the dataset downloaded via 
#' \code{pplr_get_data()}
#'
#' @description Create a data frame by "extracting" the \code{covariates} column
#' contained in an dataset downloaded with \code{pplr_get_data()}.
#' 
#' @param input An object of class \code{get_data}.
#' 
#' @return A data frame whose columns represent the covariates of the dataset
#' downloaded via \code{pplr_get_data()}. Note that these covariates are 
#' contained in the \code{covariates} column datasets downloaded using
#' \code{pplr_get_data()}.
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#' demo_d <- pplr_get_data(proj_metadata_key == 8)
#' as.tbl( pplr_cov_unpack( demo_d ) )
#' }
#' 
#' @importFrom stringr str_split str_match
#' @importFrom dplyr %>%
#' @export

# function to unpack covariates
pplr_cov_unpack <- function(input){
  # Extract all characters between the brackets and split the string on each comma (',')
  # followed by a space ('\\s')
  key_value_pair_dictionary_list <- stringr::str_split(
    trimws(stringr::str_match(input$covariates, "\\{(.*)\\}"))[ ,2], 
    ",\\s",
    simplify = TRUE)
  
  # Extract values from dictionary syntax (key: value)
  value_data <- apply(
    gsub("\\'", "", key_value_pair_dictionary_list),
    2,
    function(x) {
      stringr::str_match(x, "\\:\\s(.*)")[ ,2]
    }
    ) %>% 
    data.frame()
  
  colnames(value_data) <- paste0(colnames(value_data) , "_value")
  
  
  # Extract keys from dicitonary
  key_data <- data.frame(apply(
    gsub("\\'", "", key_value_pair_dictionary_list),
    2, 
    function(x) {
      stringr::str_match(x, "(.*)\\:")[,2]
    }))
  
  colnames(key_data) <- paste0(colnames(key_data), "_label")
  
  # combing them into a dataframe
  covariate_data <- cbind(value_data, key_data)
  # And ordering the columns by alphabetical order
  covariate_data <- covariate_data[ ,order(names(covariate_data))] ### END PRODUCT ####
  return(covariate_data)
}
