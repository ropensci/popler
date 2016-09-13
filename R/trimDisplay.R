#' Trim the display of the popler() output
#'
#' Service function for the popler() function
#' @param a data frame
#' @return a data frame whose string elements are trimmed at the 50th character
#' @export

# trim the display of character values. Mostly for project "titles"
trimDisplay=function(x){
  tmp=as.data.frame(x)
  for(i in 1:ncol(tmp)){
    if(is.character(tmp[,i])){ tmp[,i]=strtrim(tmp[,i],50) }
  }
  tmp=as.tbl(tmp)
  return(tmp)
}
