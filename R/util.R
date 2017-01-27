# Converts factor columns into character format
factor_to_character <- function(x, full_tbl = FALSE){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)
  
}

# changes clss to class and ordr to order
class_order_names <- function(x){
  
  names(x)   <- gsub("clss","class",names(x))
  names(x)   <- gsub("ordr","order",names(x))
  
  return(x)
  
}


# implements the 'keyword' argument ANd operator in browse() 
key_arg <- function(x,keyword,criteria){
  
  # if both keyword and criteria are used ----------------------------------------------
  if( !is.null(keyword) & !is.null(criteria) ){
    stop("
         
         browse() cannot simultaneously subset based on both 
         a logical statement and the 'keyword' argument
         
         Pick one of the two methods, or... 
         refine your search using get_data()
         
         ")
  }
  
  # if only keyword is used --------------------------------------------------
  if( !is.null(keyword) & is.null(criteria) ){
    
    #function: index of keywords
    i_keyw <- function(x,keyword) {
      ind <- which( grepl(keyword,x,ignore.case = T) )
      return(ind)
    }
    
    # row numbers selected
    ind_list  <- lapply(x, i_keyw, keyword)
    proj_i    <- unique( unlist(ind_list) )
    
    # projects selected
    if(length(proj_i) > 0){
      proj_n      <- unique( x$proj_metadata_key[proj_i] )
      statements  <- paste0("proj_metadata_key == ", proj_n)
      src_arg     <- parse(text=paste0(statements, collapse = " | "))[[1]]
    } else { 
      src_arg     <- NULL
    }
    
    # return values
    out <- list(tab=x[proj_i,],s_arg=src_arg) 
    return(out)
    
  }
  
  
  # if only criteria is used --------------------------------------------------
  if( is.null(keyword) & !is.null(criteria) ){
    
    # if is %=% is used
    if( any(grepl("%=%",deparse(criteria))) ) {
      
      # you cannot use %=% and other operators simultaneously
      if( any(grepl("==|!=|>|<|<=|>=",deparse(criteria))) ){
        
        stop("
             
             You cannot use `%=%` and R's standard operators (<,>,<=,>=,==,!=)
             simultaneously. Sorry about that.
             
             ")
        
      } else{
        
        # convert 
        proj_i     <- which(eval(criteria, x, parent.frame()))
        
        # get project IDs
        if( length(proj_i) != 0 ) {
          proj_n <- unique( x[proj_i,,drop=F]$proj_metadata_key )
          statements  <- paste0("proj_metadata_key == ", proj_n)
          src_arg     <- parse(text=paste0(statements, collapse = " | "))[[1]]
        } else { src_arg <- NULL }
        
        # return values
        sbst_popler <<- src_arg # change the subset statement in parent environment
        return(list(tab=x))
      }
      
      # if neither keyword, nor %=% are used, return data frame as is
    } else { 
      return(list(tab=x))
    }
    
  }
 
  if( is.null(keyword) & is.null(criteria) ){ return(list(tab=x)) }
   
}


# Summarizing function
select_by_criteria <- function(x,criteria){
  
  if(!is.null(criteria)) {
    r <- which(eval(criteria, x, parent.frame()))
    if( length(r) != 0 ) {
      subsetDat <- tbl_df(x[r,,drop=F]) #tbl_df() to make object "work" with dplyr functions
    } 
    if( length(r) == 0 ) {
      stop( "No matches found. Either:
  1. the name of variable(s) you specified is/are incorrect or 
  2. the values you are looking for are not contained in the variable(s) you specified")
    }
    } else { 
      subsetDat <- tbl_df(x) 
    }
  return(subsetDat)
  
}


# Store possible arguments
possibleargs <- c("title","proj_metadata_key","lterid",
                  "datatype","studytype","duration_years",
                  "community",
                  "structured_type_1","structured_type_2","structured_type_3",
                  "treatment_type_1","treatment_type_2","treatment_type_3",
                  "lat_lter","lng_lter",
                  "species","kingdom","phylum","class","order","family","genus"
                  )


# returns a full table or not
table_select <- function(x, full_tbl = FALSE, possible_args){
  
  if(full_tbl == FALSE) return(x[,possible_args])
  if(full_tbl == TRUE)  return(x)
  
}


# check that at least proj_metadata_key is included in variables
vars_check <- function(x){
  
  if( !"proj_metadata_key" %in% x ) x = c("proj_metadata_key",x)
  return(x)
  
}

# Error for misspelled columns in full table
err_full_tab <- function(select_columns,columns_full_tab,possibleargs){
  
  #Check for spelling mistakes
  if( !all( is.element(select_columns,columns_full_tab) ) ) {
    opt <- options(error=NULL)
    on.exit(opt)
    stop(paste0("Error: the following 'argument' entry was mispelled: '",
                setdiff(select_columns,possibleargs),"' "))
  }
  
}


# expand table (to nest/unnest taxonomic info) 
elastic_tab <- function(x, shrink = TRUE, full_tbl){
  
  # select taxonomic information (based on full_ or standard_table)
  if( full_tbl == FALSE){
    taxas <- c("species","kingdom","phylum","class","order","family","genus")
  } else {
    taxas <- c('sppcode','kingdom','subkingdom','infrakingdom',
               'superdivision','division','subdivision',
               'superphylum','phylum','subphylum','class',
               'subclass','order','family','genus','species',
               'common_name','authority')
  }
  
  # nest(shrink)/Unnest(expand) data set
  if( shrink == TRUE) {
    out  <- x %>% 
      group_by_(.dots = setdiff(names(x),taxas) ) %>%
        nest(.key = taxonomy)
    # Names of taxonomic lists
    names(out$taxonomy)  <- paste0("taxa_project_#_",out$proj_metadata_key)
  } else {
    out  <- x %>% 
      group_by_( .dots = setdiff(names(x), "taxonomy") ) %>%
        unnest_( unnest_cols = "taxonomy" )
  }
  
  return(out)
  
}


# trim the display of character values. Mostly for project "titles"
trim_display=function(x, trim){
  
  if(trim==T){
    tmp=as.data.frame(x)
    for(i in 1:ncol(tmp)){
      if(is.character(tmp[,i])){ tmp[,i]=strtrim(tmp[,i],25) }
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
    tally_name=paste0(tally_columns[i],"_count") #name of column containing tallies
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

