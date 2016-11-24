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
        unnest_(unnest_cols = "taxonomy")
  }
  
  return(out)
  
}


# Converts factor columns into character format
factor_to_character <- function(x, full_tbl = FALSE){
  
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
table_select <- function(x, full_tbl = FALSE){
  
  # Initial group_factors
  possibleargs <- tolower(c("title","proj_metadata_key","lterid",
                            "datatype","studytype","duration_years",
                            "community","structured",
                            "lat_lter","lng_lter",
                            "species","kingdom","phylum","class","order","family","genus") )
  
  if(full_tbl == FALSE) return(x[,possibleargs])
  if(full_tbl == TRUE)  return(x)

}


# produce a list of dictionary entries
dict_list <- function(x, select_columns){
  
  # list to contain dictionary of selected columns 
  out       <- list()
  
  if( any( "species" == select_columns) ){
    
    select_columns = c(c("genus","species"),
                       setdiff(select_columns,c("genus","species")))
    
    tmp       <- unique(x[,c("genus","species")], drop = FALSE)
    out[[1]]  <- arrange_(tmp, .dots = c("genus","species") )
    names(out)[1] <- "species"
    
    if( length(select_columns) > 2 ){
      
      # select columns
      sub_cols  <- x[, select_columns, drop = FALSE]
      
      # loop through selected columns
      for(i in 3:length(select_columns) ){
        
        # if numeric
        if( is.numeric(sub_cols[,i]) ) {
          out[[i-1]]        <- paste("numeric field: from",min(sub_cols[,i],na.rm = TRUE),
                                      "to", max(sub_cols[,i],na.rm = TRUE))
          names(out)[i-1]   <- select_columns[i]
          
          # if not numeric, return unique values
        } else {
          out[[i-1]]        <- unique(sub_cols[,i])[ order(unique(sub_cols[,i])) ]
          names(out)[i-1]   <- select_columns[i]
          # remove NAs
          out[[i-1]]  <- out[[i-1]][out[[i-1]] != "NA"]
        }
      }
    }
    
  } else{
    
    # select columns
    sub_cols  <- x[, select_columns, drop = FALSE]
    
    # loop through selected columns
    for(i in 1:length(select_columns) ){
      # if numeric
      if( is.numeric(sub_cols[,i]) ) {
        out[[i]]        <- paste("numeric field: from",min(sub_cols[,i],na.rm = TRUE),
                                  "to", max(sub_cols[,i],na.rm = TRUE))
        names(out)[i]   <- select_columns[i]
        # if not numeric, return unique values
      } else {
        out[[i]]        <- unique(sub_cols[,i])[ order(unique(sub_cols[,i])) ]
        names(out)[i]   <- select_columns[i]
        # remove NAs
        out[[i]]  <- out[[i]][out[[i]] != "NA"]
      }
    }
   
  }
  
  # name list elements by the column they refer to
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

# changes clss to class and ordr to order
class_order <- function(x){
  
  r           <- which(names(x) %in% c("clss","ordr") ) 
  names(x)[r] <- c("class","order")
  return(x)
  
}

# explain meaning of dictionary variables 
dictionary_explain <- function(x){
  
  if( ncol(x) < 60){
    out = data.frame(variable = names(x), 
                     description=c("title of project","unique project id","lter name",
                                   "type of abundance data (count,biomass,cover,density,individual)",
                                   "experimental or observational study?","duration of project in years",
                                   "does data set contain multiple taxa?","is indidivual data structured?",
                                   "lter site latitude", "lter site longitude","specific epithet of a taxonomic unit",
                                   "kingdom","phylum","class","order","family","genus") )
  } else {
    out = data.frame(variable = names(x), 
                     description=c("project_id","title of project",
                                   "unit by which abundance was measured",
                                   "type of abundance data (count,biomass,cover,density,individual)",
                                   "is indidivual data structured? (e.g. by size, age, or sex)",
                                   "year of first observation","year of last observation",
                                   "frequency of sampling (e.g. yearly, seasonal, monthly)",
                                   "experimental or observational study?",
                                   "does data set contain multiple taxa?",
                                   
                                   "extent (in area or volume) of replication level 1 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 1 was measured",
                                   "label by which rep. level 1 is identified in the original data set",
                                   "replication of replication level 1",
                                   "extent (in area or volume) of replication level 2 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 2 was measured",
                                   "label by which rep. level 2 is identified in the original data set",
                                   "replication of replication level 2",
                                   "extent (in area or volume) of replication level 3 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 3 was measured",
                                   "label by which rep. level 3 is identified in the original data set",
                                   "replication of replication level 3",
                                   "extent (in area or volume) of replication level 4 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 4 was measured",
                                   "label by which rep. level 4 is identified in the original data set",
                                   "replication of replication level 4",
                                   
                                   "type of treatment, if project is experimental",
                                   "is abundance a derived quantity?", # make 100% sure this is correct
                                   "authors of the study",
                                   "email contact of study's authors",
                                   "web address of study's metadata",
                                   "identifier of the knb Catalog System", 
                                   
                                   "duration of project in years",
                                   "LTER site identification code",
                                   "full name of LTER site",
                                   "LTER site latitude", "LTER site longitude",
                                   "is LTER site currently_funded? (1 is funded, 0 is not funded)",
                                   "current principlal investigator of LTER",
                                   "email of LTER's current principlal investigator",
                                   "alternative email of LTER's current principlal investigator",
                                   "LTER homepage web address",
                                   
                                   "species code (generally refers to a binomial name (genus, species)",
                                   "kingdom","subkingdom","infrakingdom",
                                   "superdivision","division", 
                                   "subdivision","superphylum",
                                   "phylum","subphylum","class","subclass","order","family","genus",
                                   "specific epithet", "species' common name", 
                                   "scholar who first published the species name") )
  }
  
  return(out)
  
}

# query columsn 
query_cols <- function(){
 
  conn <- src_postgres(
    dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")
  
  #list columns
  proj_cols     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'project_table'")))[,1]
  lter_cols     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'lter_table'")))[,1]
  site_cols     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'study_site_table'")))[,1]
  s_i_p_cols    <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'site_in_project_table'")))[,1]
  taxa_cols     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'taxa_table'")))[,1]
  abund_cols    <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'taxa_table'")))[,1]
  
  all_cols      <- c(proj_cols,lter_cols,site_cols, s_i_p_cols, taxa_cols, abund_cols) 
  default_cols  <- c("year","day","month","genus","species","structure","datatype",         
                     "spatial_replication_level_1","spatial_replication_level_2",
                     "spatial_replication_level_3","spatial_replication_level_4",
                     "proj_metadata_key"
                     )

  return( list(all_cols = all_cols, default_cols = default_cols) )

}
