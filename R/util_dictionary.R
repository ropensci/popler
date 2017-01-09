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

# explain meaning of dictionary variables 
dictionary_explain <- function(x){
  
  if( ncol(x) < 60){
    out = data.frame(variable = names(x), 
                     description=c("title of project","unique project id","lter name",
                                   "type of abundance data (e.g. count,biomass)",
                                   "experimental or observational study?","duration of project in years",
                                   "does data set contain multiple taxa?",
                                   "1st type of indidivual structure (if any)","2nd type of indidivual structure (if any)","3rd type of indidivual structure (if any)",
                                   "type of 1st treatment (if any)","type of 2nd treatment (if any)","type of 3rd treatment (if any)",
                                   "lter site latitude", "lter site longitude","specific epithet of a taxonomic unit",
                                   "kingdom","phylum","class","order","family","genus") ,
                     stringsAsFactors = F)
  } else {
    out = data.frame(variable = names(x), 
                     description=c("project_id","foreign key of lter project",
                                   "title of project",
                                   "unit of measure for abundance",
                                   "type of abundance data (e.g. count,biomass)",
                                   "1st type of indidivual structure (if any)","unit of measure of 1st type of individual structure",
                                   "2nd type of indidivual structure (if any)","unit of measure of 2nd type of individual structure",
                                   "3rd type of indidivual structure (if any)","unit of measure of 3rd type of individual structure",
                                   "year of first observation","year of last observation",
                                   "frequency of sampling (e.g. yearly, seasonal, monthly)",
                                   "experimental or observational study?",
                                   "does data set contain multiple taxa?",
                                   
                                   "extent (in area or volume) of replication level 1 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 1 was measured",
                                   "label by which rep. level 1 is identified in the original data set",
                                   "number of replicates (level 1)",
                                   "extent (in area or volume) of replication level 2 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 2 was measured",
                                   "label by which rep. level 2 is identified in the original data set",
                                   "number of replicates (level 2)",
                                   "extent (in area or volume) of replication level 3 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 3 was measured",
                                   "label by which rep. level 3 is identified in the original data set",
                                   "number of replicates (level 3)",
                                   "extent (in area or volume) of replication level 4 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 4 was measured",
                                   "label by which rep. level 4 is identified in the original data set",
                                   "number of replicates (level 4)",
                                   "extent (in area or volume) of replication level 5 (highest rep. level: site)",
                                   "unit of measure by which extent of the replication level 5 was measured",
                                   "label by which rep. level 5 is identified in the original data set",
                                   "number of replicates (level 5)",
                                   "total number of spatial replicates",
                                   "number of nested spatial levels",
                                   
                                   "type of 1st treatment (if any)","type of 2nd treatment (if any)","type of 3rd treatment (if any)",
                                   "label of control treatment/group",
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
                                   "specific epithet", "common name of species", 
                                   "scholar who first published the species name",
                                   "foreign key of taxa table"),
                     stringsAsFactors = F)
  }
  
  return(out)
  
}
