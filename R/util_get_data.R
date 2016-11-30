
# query columns 
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
                     "authors","authors_contact","proj_metadata_key"
  )
  
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
  
  table_all <- tbl(connection, sql(
    paste(
      # Count data
      "SELECT",select_vars,", count_observation",
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
      "SELECT",select_vars,", biomass_observation",
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
      "SELECT",select_vars,", percent_cover_observation",
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
      "SELECT",select_vars,", individual_observation",
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
      "SELECT",select_vars,", density_observation",
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


# Function to open metadata webpage for each project whose data was downloaded
open_metadata <- function(metadata){
  
  if(metadata == T) {
    unique_projects = unique(output_data$proj_metadata_key)
    for(i in 1:length(unique_projects) ){
      
      metadata_webpage(unique_projects[i])
      
    }
  }
  
}

# informational message at every download
data_message <- function(x){
  
  mess_df <- unique(select(x,proj_metadata_key,authors,authors_contact))
  names(mess_df) <- c("project_id","authors","contacts")
  
  # Messages
  cat("\nBelow are the authors connected to the data you downloaded. \nPlease contact these individuals if you wish to carry out research using this data.\n\n")
  print(mess_df, row.names = FALSE)
  
}


# change subset class/order variables to clss/ordr
class_order_subset <- function(call_var) {
  
  for(i in 1:length(call_var)){
    
    if(call_var[[i]] == "order") call_var[[i]] = "ordr"
    if(call_var[[i]] == "class") call_var[[i]] = "clss"
    
  }
  
  return(call_var)
  
}
