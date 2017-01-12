# data frame of variable explanations
explanations = data.frame(variable = c('proj_metadata_key','lter_project_fkey','title','samplingunits','datatype',
                                       'structured_type_1','structured_type_1_units','structured_type_2','structured_type_2_units',
                                       'structured_type_3','structured_type_3_units',
                                       'studystartyr','studyendyr','samplefreq','studytype','community',
                                       'spatial_replication_level_1_extent','spatial_replication_level_1_extent_units',
                                       'spatial_replication_level_1_label','spatial_replication_level_1_number_of_unique_reps',
                                       'spatial_replication_level_2_extent','spatial_replication_level_2_extent_units','spatial_replication_level_2_label','spatial_replication_level_2_number_of_unique_reps',
                                       'spatial_replication_level_3_extent','spatial_replication_level_3_extent_units','spatial_replication_level_3_label','spatial_replication_level_3_number_of_unique_reps',
                                       'spatial_replication_level_4_extent','spatial_replication_level_4_extent_units','spatial_replication_level_4_label','spatial_replication_level_4_number_of_unique_reps',
                                       'spatial_replication_level_5_extent','spatial_replication_level_5_extent_units','spatial_replication_level_5_label','spatial_replication_level_5_number_of_unique_reps',
                                       'tot_spat_rep','n_spat_levs','treatment_type_1','treatment_type_2','treatment_type_3','control_group','derived','authors','authors_contact','metalink','knbid','duration_years','lterid','lter_name','lat_lter','lng_lter',
                                       'currently_funded','current_principle_investigator','current_contact_email','alt_contact_email','homepage',
                                       'sppcode','kingdom','subkingdom','infrakingdom','superdivision','division','subdivision','superphylum','phylum','subphylum','class','subclass','order','family','genus','species','common_name','authority','metadata_taxa_key'), 
                          
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


dict_list_1 <- function(x, select_columns){
  
  # index "special" and "normal"
  i_spec          <- which(select_columns %in% c("structure","species") )
  i_norm          <- setdiff(c(1:length(select_columns)), i_spec)
  spec_cols       <- select_columns[i_spec]
  norm_cols       <- select_columns[i_norm]
  
  # get unique values of "normal" variables -------------------------------------------
  if(length(norm_cols) > 1){
    out_norm <- lapply(x[,norm_cols], unique)
  } else {
    out_norm <- lapply(x[,norm_cols,drop=F], unique)
  }
  
  # get unique values of "special" variables ------------------------------------------
  out_spec        <- list(species=NULL,structure=NULL)
  
  if( any( "species" == select_columns) ){
    out_spec[[1]]   <- unique(x[,c("genus","species")])
  }
  if( any("structure" == select_columns) ){
    # stash all structure data in a single vector
    str_vec         <- unlist( c(x[,paste0("structured_type_",1:3)]) )
    out_spec[[2]]   <- unique( str_vec )
  }
  descr_spec      <- c("species (species name)","structure (type of indidivual structure)")
  if( identical(spec_cols,"species") )   { out_spec = out_spec[1] ; descr_spec=descr_spec[1] }
  if( identical(spec_cols,"structure") ) { out_spec = out_spec[2] ; descr_spec=descr_spec[2] }
  #keep_s             <- match(names(out_spec),spec_cols)
  #names(out_spec)    <- c("species (species name)","structure (type of indidivual structure)")
  #keep_s            <- keep_s[!is.na(keep_s)]
  #out_spec          <- out_spec[keep_s]
  
  # Final list (keeps order of inputs) -----------------------------------------------------------------------
  out               <- vector("list", length(select_columns))
  out[i_norm]       <- out_norm
  out[i_spec]       <- out_spec

  # Add variable explanations
  description     <- explanations$description[ match(names(out_norm),explanations$variable) ]
  descr_norm      <- paste0(names(out_norm), " (", description,")" )
  
  nam_out         <- character(length(out))
  nam_out[i_norm] <- descr_norm
  nam_out[i_spec] <- descr_spec
  
  names(out)      <- nam_out
  
  return(out) 
  
}


  

# produce a list of dictionary entries
dict_list <- function(x, select_columns){
  
  # list to contain dictionary of selected columns 
  out       <- list()
  
  # Entry "species" is treated differently
  if( any( "species" == select_columns) ){
    
    select_columns  <- c(c("genus","species"),
                       setdiff(select_columns,c("genus","species")))
    
    tmp             <- unique(x[,c("genus","species")], drop = FALSE)
    out[[1]]        <- arrange_(tmp, .dots = c("genus","species") )
    names(out)[1]   <- "species"
    
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
    
    # Add variable explanations, for everything not relative to "species"   
    description     <- explanations$description[ match(names(out)[-1],explanations$variable) ]
    names(out)[-1]  <- paste0(names(out)[-1], " (", description,")" )
    
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
    
    # Add variable explanations   
    description <- explanations$description[ match(names(out),explanations$variable) ]
    names(out)  <- paste0(names(out), " (", description,")" )
         
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
