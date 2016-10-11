#' Translate R logical statements into SQL
#'
#' USe dplyr translate_sql() to translate R logical statements into SQL
#' @param ... A logical expression that specifies which records to keep.
#' @param lazy logical argument: lazy evaluation of logical expression or not? 
#' @return A string with an SQL statement
#' @export

#This function procudes a string with an sql statement
#From an R statement that could be used on "
parse_to_sql_search= function(...,lazy=T){

  #Use Hadley's translate_sql
  if(lazy == T){ # with lazy evaluation
    sqlTranslated=translate_sql(...)
  } else { # without lazy evaluation
    sqlTranslated=translate_sql_(...)
  }

  #Remove quotation marks
  sqlPaste=gsub('"',"",sqlTranslated)

  #Substitute 'class' with 'clss'
  sqlPaste=gsub("class","clss",sqlPaste)
  #Substitute 'order' with 'ordr'
  sqlPaste=gsub("order","ordr",sqlPaste)

  #Done!
  return(sqlPaste)

}
