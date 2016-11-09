
# Converts factor columns into character format
factor_to_character <- function(x){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)
  
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
table_select <- function(x, full_table = FALSE){
  
  # Initial group_factors
  possibleargs <- tolower(c("lterid","title",
                            "datatype","studytype","duration_years",
                            "community","structured",
                            "lat_lter","lng_lter",
                            "species","kingdom","phylum","clss","ordr","family","genus"))
  
  if(full_table == FALSE) return(x[,possibleargs])
  if(full_table == TRUE)  return(x)

}


# produce a list of dictionary entries
dict_list <- function(x, select_columns){
    
  # select columns
  tmp   <- x[, select_columns, drop = FALSE]
  # list to contain dictionary of selected columns 
  out   <- list()
  
  # loop through selected columns
  for(i in 1:length(select_columns)){
    
    # if numeric
    if( is.numeric(tmp[,i]) ) {
      out[[i]]  <- paste("numeric field: from",min(tmp[,i],na.rm = TRUE),
                         "to", max(tmp[,i],na.rm = TRUE))
      # if not numeric, return unique values
    } else {
      out[[i]]  <- unique(tmp[,i])
    }
    
  }

  # name list elements by the column they refer to
  names(out)  <- select_columns
  return(out)
  
}