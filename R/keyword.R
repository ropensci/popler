#' keyword binary operator
#'
#' Match one string pattern with the contents of a string vector.
#' This is a wrapper of the grepl() function in base.
#' @param var The name of a character vector.
#' @param keyword A character string containing a keyword to be matched to the argument `var` 
#' @return A logical vector. 
#' @export
#' @examples
#' letters %=% "a"
#' 
#' # matching is case insensitive 
#' letters %=% "A"


# define the binary operator
`%=%` <- function(var, keyword) {
  
  if( length(keyword) == 1) {
    l_out   <- grepl(keyword, var, ignore.case = T)
  } else {
    keyword <- paste(keyword, collapse = "|")
    l_out   <- grepl(keyword, var, ignore.case = T)
  }
  return(l_out)
  
}

