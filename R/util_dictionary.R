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

explain_short = data.frame( variable = c('title','proj_metadata_key','lterid','datatype',
                                          'studytype','duration_years','community',
                                          'structure','treatment',
                                          'lat_lter','lng_lter',
                                          'species','kingdom','phylum','class','order','family','genus'),
                           
                            description=c("title of project","unique project id","lter name",
                                          "type of abundance data (e.g. count,biomass)",
                                          "experimental or observational study?","duration of project in years",
                                          "does data set contain multiple taxa?",
                                          "types of indidivual structure (if any)",
                                          "types of treatment (if any)",
                                          "lter site latitude", "lter site longitude","specific epithet of a taxonomic unit",
                                          "kingdom","phylum","class","order","family","genus"),
                            stringsAsFactors = F)
                          

# lazy evaluation in dictionary
vars_dict <- function(...){
  
  eval_that <- lazyeval::lazy_dots(...)
  out       <- sapply(eval_that, function(x) as.character(x$expr) )
  
  if(length(out) > 0 ) {
    return(out)
  } else { return(NULL) }
  
}


# verify whether provided variables match one of the potential variables
verify_vars <- function(sel_col){
  
  i <- which(sel_col %in% c(explanations$variable,"structure","treatment","species") )
  
  if( length(i) < length(sel_col) ){
    
    unmatched <- setdiff(seq_len(length(sel_col)),i) 
    stop(paste0("variable '",sel_col[unmatched],
                "' does not match any of the variables contained in popler"))
         
  }  
 
}

# produce the lists of unique dictionary values
dict_list <- function(x, select_columns){
  
  # first, verify user input matches with variables contained in popler
  popler:::verify_vars(select_columns)
  
  # index "special" and "normal"
  i_spec          <- which(select_columns %in% c("structure","treatment","species") )
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
  out_spc     <- list()
  
  if( any( "species" == select_columns) ){
    out_spc$species   <- unique(x[,c("genus","species")])
  }
  if( any("structure" == select_columns) ){
    # stash all structure data in a single vector
    str_vec           <- unlist( c(x[,paste0("structured_type_",1:3)]) )
    out_spc$structure <- unique( str_vec )
  }
  if( any("treatment" == select_columns) ){
    # stash all structure data in a single vector
    tr_vec            <- unlist( c(x[,paste0("treatment_type_",1:3)]) )
    out_spc$treatment <- unique( tr_vec )
  } 
  
  # Variable descriptions ----------------------------------------------------------------
  # Special variables
  descr_spec  <- c("species (species name)","structure (types of indidivual structure (if any))",
                   "treatment (type of treatment(if any))")
  if(length(out_spc) > 0 ){
    d_s_ind     <- sapply( names(out_spc), function(x) grep(x, descr_spec))
    descr_spc   <- descr_spec[d_s_ind]
  } else descr_spc <- NULL
  
  # Normal variables
  description <- explanations$description[ match(names(out_norm), explanations$variable) ]
  descr_norm  <- paste0(names(out_norm), " (", description,")" )
  
  # final descriptions
  names_out   <- rep(NA, length(select_columns))
  names_out[i_norm] <- descr_norm
  names_out[i_spec] <- descr_spc

  
  # description of output -----------------------------------------------------------------
  out         <- rep(list(NULL), length(select_columns))
  out[i_norm] <- out_norm
  out[i_spec] <- out_spc
  out         <- setNames(out, names_out)
 
  return(out) 
  
}


# explain meaning of dictionary variables 
dictionary_explain <- function(x){
  
  if( ncol(x) < 60){
    out = explain_short
  } else {
    out = explanations
  }
  
  return(out)
  
}
