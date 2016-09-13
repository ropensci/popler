#' Formats the main table of popler()
#'
#' This function converts every column encoded as "factor" into "character"
#' @param main table of popler()
#' @return formatted popler() main table
#' @export

formatMainTable=function(x){

  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)

}
