#' Print popler study names in a legible form.
#'
#' Prints the names of popler studies as paragraphs of known width. This is a wrapper of the strwrap function in base. 
#' @param browsed An object created by the function browse()
#' @param width A positive integer giving the target column for wrapping lines in the output (from base function strwrap) 
#' @export
#' @examples
#' # Look up names
#' #tmp = browse()
#' #study_names(tmp)
#' vcr_dat = browse(lterid == "VCR")
#' study_names(vcr_dat)
#' study_names(vcr_dat, width = 30)

study_names <- function(browsed, width = 60){
  
  # re-browse data, with trim OFF
  if( is.null(attributes(browsed)$search_expr) ){
    new_browsed <- browse(trim = F)
  } else {
    new_browsed <- browse(eval(attributes(browsed)$search_expr), trim = F)
  }
  
  tmp = lapply(new_browsed$title,strwrap, width = width)
  for(i in 1:length(tmp)){
    
    tmp[[i]] = c(tmp[[i]],"\n")
    writeLines(tmp[[i]])
    
  }

}

