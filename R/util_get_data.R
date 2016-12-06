
# query columns 
query_cols <- function(){
  
  #conn <- src_postgres(
  #  dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")
  conn <- src_postgres(dbname="popler_3", 
                       host="ec2-54-212-204-87.us-west-2.compute.amazonaws.com", 
                       port=5432, user="lter")
  
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
                                                table_name = 'count_table'")))[,1]
  
  all_cols      <- c(proj_cols,lter_cols,site_cols, s_i_p_cols, taxa_cols, abund_cols)
  default_cols  <- c("year","day","month","genus","species","datatype",         
                     "spatial_replication_level_1","spatial_replication_level_2",
                     "spatial_replication_level_3","spatial_replication_level_4",
                     "authors","authors_contact","proj_metadata_key",
                     "structure_type_1","structure_type_2","structure_type_3",
                     "treatment_type_1","treatment_type_2","treatment_type_3"
                     )
  
  rm(conn)
  return( list(all_cols = all_cols, default_cols = default_cols) )
  
}



# Identify which "search_arguments" belong to "all_columns"
inherit_search <- function(all_cols, inherit_logical){
  
  inherit_elem <- as.character(inherit_logical)
  
  inds = NULL
  for(i in 1:length(all_cols)){
    if( any( grepl(all_cols[i], inherit_elem) ) ){
      inds = c(inds,i)  
    }
  }
  return( unique(all_cols[inds]) )
}


# update columns selected in query
inherit_variables <- function(browsed_data, subset, all_columns){
  
  if( is.null(browsed_data) ){
    inherit_browse  <- NULL
  } else { 
    inherit_elem    <- as.character( attributes(browsed_data)$search_argument )
    inherit_elem    <- gsub("order", "ordr", inherit_elem)
    inherit_elem    <- gsub("class", "clss", inherit_elem)
    inherit_browse  <- popler:::inherit_search(all_columns, inherit_elem) 
  }
  
  if( any( deparse(subset) == "subset" ) ){
    inherit_subset  <- NULL
  } else { 
    inherit_elem    <- as.character( subset )
    inherit_elem    <- gsub("order", "ordr", inherit_elem)
    inherit_elem    <- gsub("class", "clss", inherit_elem)
    inherit_subset  <- popler:::inherit_search(all_columns, inherit_elem) 
  }
  
  return( unique( c(inherit_browse, inherit_subset) ) )
  
}


# produce sql statements for get_data
subset_arguments <- function(browsed_data = NULL, subset = NULL){

  # convert subset call to NULL in case subset is not specified
  if( any( deparse(subset) == "subset" ) ) subset <- NULL

  # place holders of subset statements
  subset_inherit  <- NULL
  subset_get_dat  <- NULL
  
  # translate statements to sql
  if( !is.null(browsed_data) )  subset_inherit <- parse_to_sql_search( attributes(browsed_data)$search_argument )
  if( !is.null(subset) )        subset_get_dat <- parse_to_sql_search( subset )
  
  # combine subset statements (if needed)
  if( !is.null(subset) ){
    if( !is.null(browsed_data) ){
      subset_arg <- paste(subset_inherit, subset_get_dat, sep = " AND ")
    } else{
      subset_arg <- paste0(subset_get_dat)
    }
  } else{
    subset_arg <- paste0(subset_inherit)
  }

  return(subset_arg)
  
}


# query popler
query_popler <- function(connection, select_vars, search_arg){
  
  if(length(search_arg) == 0) stop( "No logical argument specified. Please specify what data you wish to download from popler" )
  
  # table specific variables
  vars                     <- list()
  vars$count_table         <- gsub("treatment_type_","count_table.treatment_type_",select_vars)
  vars$biomass_table       <- gsub("treatment_type_","biomass_table.treatment_type_",select_vars)
  vars$percent_cover_table <- gsub("treatment_type_","percent_cover_table.treatment_type_",select_vars)
  vars$density_table       <- gsub("treatment_type_","density_table.treatment_type_",select_vars)
  vars$individual_table    <- gsub("treatment_type_","individual_table.treatment_type_",select_vars)
  
  table_all <- tbl(connection, sql(
    paste(
      # Count data
      "SELECT",vars$count_table,", count_observation",
      "FROM count_table",
      "JOIN taxa_table ON count_table.taxa_count_fkey = taxa_table.taxa_table_key",
      "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
      "site_in_project_table.site_in_project_key",
      "JOIN project_table ON site_in_project_table.project_table_fkey =",
      "project_table.proj_metadata_key",
      "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
      "study_site_table.study_site_key",
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
      "WHERE", search_arg,
      
      "UNION ALL",
      # Biomass data
      "SELECT",vars$biomass_table,", biomass_observation",
      "FROM biomass_table",
      "JOIN taxa_table ON biomass_table.taxa_biomass_fkey = taxa_table.taxa_table_key",
      "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
      "site_in_project_table.site_in_project_key",
      "JOIN project_table ON site_in_project_table.project_table_fkey =",
      "project_table.proj_metadata_key",
      "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
      "study_site_table.study_site_key",
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
      "WHERE", search_arg,
      
      "UNION ALL",
      # percent cover data
      "SELECT",vars$percent_cover_table,", percent_cover_observation",
      "FROM percent_cover_table",
      "JOIN taxa_table ON percent_cover_table.taxa_percent_cover_fkey = taxa_table.taxa_table_key",
      "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
      "site_in_project_table.site_in_project_key",
      "JOIN project_table ON site_in_project_table.project_table_fkey =",
      "project_table.proj_metadata_key",
      "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
      "study_site_table.study_site_key",
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
      "WHERE", search_arg,
      
      "UNION ALL",
      # individual data
      "SELECT",vars$individual_table,", individual_observation",
      "FROM individual_table",
      "JOIN taxa_table ON individual_table.taxa_individual_fkey = taxa_table.taxa_table_key",
      "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
      "site_in_project_table.site_in_project_key",
      "JOIN project_table ON site_in_project_table.project_table_fkey =",
      "project_table.proj_metadata_key",
      "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
      "study_site_table.study_site_key",
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
      "WHERE", search_arg,
      
      "UNION ALL",
      # density data
      "SELECT",vars$density_table,", density_observation",
      "FROM density_table",
      "JOIN taxa_table ON density_table.taxa_density_fkey = taxa_table.taxa_table_key",
      "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
      "site_in_project_table.site_in_project_key",
      "JOIN project_table ON site_in_project_table.project_table_fkey =",
      "project_table.proj_metadata_key",
      "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
      "study_site_table.study_site_key",
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
      "WHERE", search_arg
    )
  ))
  
  output_data <- as.data.frame(table_all)
  
}


# informational message at every download
data_message <- function(x){
  
  if( length(unique(x$proj_metadata_key)) == 1)
    message(paste0("You have downloaded data from ",length(unique(x$proj_metadata_key))," project. \nThe identification number of this project is: ",
           paste0(unique(x$proj_metadata_key),collapse=", "),"."),"\n
IMPORTANT NOTICE: 
If you are about to use this data in a formal publication, as courtesy, please:
1) Contact the investigators of each project. 
   Do this by using function authors() on this object. 
2) Acknowledge funding sources, if these are provided in the metadata.   
   Access metadata by using function metadata_url() on this object. \n")

  else {
    message("\n",paste0("You have downloaded data from ",length(unique(x$proj_metadata_key))," projects. \nThe identification numbers of these projects are: ",
               paste0(unique(x$proj_metadata_key),collapse=", "),"."),"\n
IMPORTANT NOTICE: 
If you are about to use this data in a formal publication, as courtesy, please:
1) Contact the investigators of each project. 
   Obtain contact information by using function authors() on this object. 
2) Acknowledge funding sources, if these are provided in the metadata.   
   Access project metadata by using function metadata_url() on this object. \n")
      
  }
  
}


# change subset class/order variables to clss/ordr
class_order_subset <- function(call_var) {
  
  for(i in 1:length(call_var)){
    
    if(call_var[[i]] == "order") call_var[[i]] = "ordr"
    if(call_var[[i]] == "class") call_var[[i]] = "clss"
    
  }
  
  return(call_var)
  
}
