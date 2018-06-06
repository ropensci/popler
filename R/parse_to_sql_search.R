# Translate R search w/ dbplyr

#' @importFrom dbplyr translate_sql_ translate_sql
#' @noRd

#This function procudes a string with an sql statement
#From an R statement that could be used on "
parse_to_sql_search <- function(..., lazy = FALSE){
  #Use Hadley's translate_sql
  if (lazy == TRUE) { # with lazy evaluation
    #sqlTranslated=translate_sql(...)
    sqlTranslated <- dbplyr::translate_sql(...)
    
    # Remove quotation marks
    sqlPaste <- gsub('"', "", sqlTranslated)
  } else { # without lazy evaluation
    #sqlTranslated=translate_sql_(...)
    sqlTranslated <- dbplyr::translate_sql_(list(...))
    
    # Remove quotation marks
    sqlPaste <- gsub('"', '', sqlTranslated)
  }
  
  #Substitute 'class' with 'clss'
  sqlPaste <- gsub("class", "clss", sqlPaste)
  #Substitute 'order' with 'ordr'
  sqlPaste <- gsub("order", "ordr", sqlPaste)
  
  #Done!
  return(sqlPaste)
  
}
