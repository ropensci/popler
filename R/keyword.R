#' keyword binary operator
#'
#' Match one or more string patterns with the contents of a string vector.
#' This is a wrapper of the grepl() function in base.
#' @param var The name of a vector object.
#' @param keyword A string containing the keyword(s) that need be matched 
#' @return A logical vector. 
#' @export
#' @examples
#' letters %=% "a"
#' 
#' # matching is case insensitive 
#' letters %=% "A"
#' 
#' # Matches multiple patterns
#' letters %=% c("A","c","d")

# define the binary operator
`%=%` <- function(var, pattern) {
  
  if( length(pattern) == 1) {
    
    l_out <- grepl(pattern, var, ignore.case = T)

  } else {
    
    pattern <- paste(pattern, collapse = "|")
    l_out <- grepl(pattern, var, ignore.case = T)
    
  }
  
  return(l_out)
  
}

