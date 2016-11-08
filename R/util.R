
# Converts factor columns into character format
factor_to_character=function(x){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)
  
}


# Error for misspelled columns in standard table 
err_standard_tab=function(select_columns,possibleargs){
  
  #Check for spelling mistakes
  if( !all( is.element(select_columns,possibleargs) ) ) {
    
    opt <- options(error=NULL)
    on.exit(opt)
    stop(paste0("Error: the following 'argument' entry was mispelled: '",
                setdiff(select_columns,possibleargs),"' "))
  }

}


# Error for misspelled columns in full table
err_full_tab=function(select_columns,columns_full_tab){
  
  #Check for spelling mistakes
  if( !all( is.element(select_columns,columns_full_tab) ) ) {
    
    opt <- options(error=NULL)
    on.exit(opt)
    stop(paste0("Error: the following 'argument' entry was mispelled: '",
                setdiff(select_columns,possibleargs),"' "))
  }
  
}

