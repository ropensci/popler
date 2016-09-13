#' Find unique values from the popler main table
#'
#' Service function for the popler() function
#' @param A selection of, or the entire popler main table.
#' @param A logical expression that specifies which records to keep.
#' @return an error message, if one of the vector elements is misspelled
#' @export

# Unique values for arguments
uniqueValues=function(subsetDat,columnNames){
  if(!is.null(columnNames)) out=trimDisplay(distinct(select(subsetDat,one_of(columnNames))))
  if(is.null(columnNames) ) out=trimDisplay(distinct(subsetDat))
  return(out)
}
