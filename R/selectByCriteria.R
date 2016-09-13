#' Select which records to keep from the popler main table.
#'
#' Service function for the popler() function
#' @param The popler main table
#' @param A logical expression that specifies which records to keep.
#' @return an error message, if one of the vector elements is misspelled
#' @export


## Summarizing function
selectByCriteria=function(x,criteria){

  if(!is.null(criteria)) {
    r <- which(eval(criteria, x, parent.frame()))
    subsetDat=tbl_df(x[r,,drop=F]) #tbl_df() to make object "work" with dplyr functions
  } else { subsetDat=tbl_df(x) }
  return(subsetDat)

}
