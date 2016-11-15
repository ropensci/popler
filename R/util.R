# expand table (to nest/unnest taxonomic info) 
elastic_tab <- function(x, shrink = TRUE, full_table){
  
  # select taxonomic information (based on full_ or standard_table)
  if( full_table == FALSE){
    taxas <- c("species","kingdom","phylum","clss","ordr","family","genus")
  } else {
    taxas <- c('sppcode','kingdom','subkingdom','infrakingdom',
               'superdivision','division','subdivision',
               'superphylum','phylum','subphylum','clss',
               'subclass','ordr','family','genus','species',
               'common_name','authority')
  }
  
  # nest(shrink)/Unnest(expand) data set
  if( shrink == TRUE) {
    out  <- x %>% 
      group_by_(.dots = setdiff(names(x),taxas) ) %>%
        nest(.key = taxonomy)
  } else {
    out  <- x %>% 
      group_by_( .dots = setdiff(names(x), "taxonomy") ) %>%
        unnest_(unnest_cols = "taxonomy")
  }
  
  return(out)
  
}


# Converts factor columns into character format
factor_to_character <- function(x, full_table = FALSE){
  
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


# trim the display of character values. Mostly for project "titles"
trim_display=function(x, trim){
  
  if(trim==T){
    tmp=as.data.frame(x)
    for(i in 1:ncol(tmp)){
      if(is.character(tmp[,i])){ tmp[,i]=strtrim(tmp[,i],50) }
    }
    tmp=as.tbl(tmp)
    return(tmp)
  } else{
    return(x)
  }
}


# Calculate tallies
tallies=function(browsed_data,tally_columns,group_factors,trim){
  
  df_list=list()
  for(i in 1:length(tally_columns)){
    
    #does 'tally_columns' refers to multiple columns
    multi_tally=popler:::multiple_columns(tally_columns[i])
    
    #columns referring to both group_factors and tallies
    group_tally_cols=c(group_factors,multi_tally)
    
    #Data of interest for the tally
    tally_data=select(browsed_data,one_of(group_tally_cols))
    
    #lower case of grouping factors (should get rid of doubles such as "yes" "Yes")
    tally_data=as.data.frame(tally_data)
    for(co in 1:ncol(tally_data)) { tally_data[,co]=tolower(tally_data[,co])}
    tally_data=as.tbl(tally_data)
    
    #only unique values
    tally_data=distinct(tally_data)
    
    #store tallies
    tally_name=paste0(tally_columns[i],"_tally") #name of column containing tallies
    #grouping factor present
    if(!is.null(group_factors)){ 
      df_list[[i]] <- tally_data %>%
        group_by_(.dots=group_factors) %>%
          summarise_(.dots=setNames(list(~n()), tally_name))
    #No grouping factor
    } else {
      df_list[[i]] <- tally_data %>% summarise_(.dots=setNames(list(~n()),tally_name))
    }
    
  }
  out=popler:::trim_display(Reduce(function(...) merge(...),df_list),trim)
  return(out)
  
}


# function converts entries to multiple columns - if need be
multiple_columns=function(x) {
  
  taxonomy            <-  c("kingdom","phylum","clss","ordr","family","genus","species")
  species             <-  c("genus","species")
  spatialLevels       <-  c("sp_rep1_ext","sp_rep2_ext","sp_rep3_ext","sp_rep4_ext")
  wrapperNames        <-  c("taxonomy","species","spatialLevels") #list of multiple columns entries (the above threee)
  
  if( any(x %in% wrapperNames) ){
    oldIds   <- which(x %in% wrapperNames)
    newIds   <- which(wrapperNames %in% x)
    eval(parse(n=1,text=paste0("newArg=",wrapperNames[newIds])))
    columnNames=c(x[-oldIds],newArg)
    return(columnNames)
  } else(return(x))
}
