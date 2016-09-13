#' Error message regarding the tally_by arguments
#'
#' Service function for the popler() function
#' @param a vector of column names
#' @param possible arguments that popler() can work with (internally defined)
#' @return an error message, if one of the vector elements is misspelled
#' @export

# Errors in tally_by
errorsTally=function(tally_by,possibleargs){

  if(!is.null(tally_by)) {
    if( !all( is.element(tally_by,possibleargs) ) ){
      stop(paste0("Error: the following 'tally_by' entry was mispelled: '",
                  setdiff(tally_by,possibleargs),"' "))
    }
  }

}
