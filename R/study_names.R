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
#' #study_names(vcr_dat, width = 30)

study_names <- function(browsed, width = 60){
  
  # re-browse data, with trim OFF
  if( is.null(attributes(browsed)$search_expr) ){
    new_browsed <- browse(trim = FALSE)
  } else {
    call        <- attributes(browsed)$search_expr
    new_browsed <- subset(summary_table, eval(call) )
  }
  
  # extract titles from object
  titles  <- unique( new_browsed$title )
  # Store 'wrapped' titles in a list 
  title_l <- lapply(titles,strwrap, width = width)
  
  for(i in 1:length(title_l)){
    
    title_l[[i]] <- c(title_l[[i]],"\n")
    writeLines(title_l[[i]])
    
  }

}

