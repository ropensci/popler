
# Converts factor columns into character format
factor_to_character <- function(x){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)
  
}


# Error for misspelled columns in standard table 
err_standard_tab <- function(select_columns,possibleargs){
  
  #Check for spelling mistakes
  if( !all( is.element(select_columns,possibleargs) ) ) {
    
    opt <- options(error=NULL)
    on.exit(opt)
    stop(paste0("Error: the following 'argument' entry was mispelled: '",
                setdiff(select_columns,possibleargs),"' "))
  }

}


# Error for misspelled columns in full table
err_full_tab <- function(select_columns,columns_full_tab){
  
  #Check for spelling mistakes
  if( !all( is.element(select_columns,columns_full_tab) ) ) {
    
    opt <- options(error=NULL)
    on.exit(opt)
    stop(paste0("Error: the following 'argument' entry was mispelled: '",
                setdiff(select_columns,possibleargs),"' "))
  }
  
}


# Summarizing function
select_by_criteria <- function(x,criteria){
  
  if(!is.null(criteria)) {
    r <- which(eval(criteria, x, parent.frame()))
    subsetDat=tbl_df(x[r,,drop=F]) #tbl_df() to make object "work" with dplyr functions
  } else { subsetDat=tbl_df(x) }
  return(subsetDat)
  
}

# returns a full table or not
full_table <- function(x, full.table = FALSE){
  
  # Initial group_factors
  possibleargs <- tolower(c("lterid","title",
                            "datatype","studytype","duration_years",
                            "community","structured",
                            "lat_lter","lng_lter",
                            "species","kingdom","phylum","clss","ordr","family","genus"))
  
  if(full.table == FALSE) return(x[,possibleargs])
  if(full.table == TRUE)  return(x)

}