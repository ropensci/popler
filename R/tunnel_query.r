#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database.
#' @param ... A logical expression using R synthax.
#' @return A data frame of the selected data.
#' @export
#' @examples
#' #returns ALL DATA referring to "Abietinaria"
#' get_fxn(genus=="Abietinaria")
#'
#' #returns data from the study number 7
#' get_fxn(medarecordid==7)


#Function that connects and gathers the information from the database
tunnel_query = function(...){

  # Database columns that will be stored as an attribute in the popler Class
  searchable_columns = c(
    # site_table
    'lterid', 'siteid', 'lat', 'long', 'decsript',

    # main_table
    'proj_metadata_key', 'datatype', 'structured', 
    'sitestartyr', 'siteendyr', 'samplefreq', 
    'studytype', 'community',

    # -- Spatial replication extent (numeric value)
    'spatial_replication_level_1_extent', 'spatial_replication_level_2_extent', 
    'spatial_replication_level_3_extent', 'spatial_replication_level_4_extent',
    # -- Spatial replication units (variable character)
    'spatial_replication_level_1_extent_units, spatial_replication_level_2_extent_units', 
    'spatial_replication_level_3_extent_units, spatial_replication_level_4_extent_units',
    # -- Spatial replication original label (variable character)
    'spatial_replication_level_1_label', 'spatial_replication_level_2_label', 
    'spatial_replication_level_3_label', 'spatial_replication_level_4_label',
    # -- Number of unique levels within each group of Spatial replication (integer)
    'spatial_replication_level_1_number_of_unique_reps', 'spatial_replication_level_2_number_of_unique_reps', 
    'spatial_replication_level_3_number_of_unique_reps', 'spatial_replication_level_4_number_of_unique_reps',
    'author', 'knbid',

    # taxa_table
    'sppcode', 'kingdom', 'phylum', 'clss', 'ordr', 'family', 'genus', 'species',

    # raw_table
    'year', 'month', 'day', 'covariates', 'spt_rep1', 'spt_rep2', 'spt_rep3',
    'spt_rep4', 'trt_label'
  )

  default_columns = paste(c(
    "year","day","month",                      
    "kingdom","phylum","clss","family","genus","species",                    
    "structure","datatype",         
    "spatial_replication_level_1","spatial_replication_level_2","spatial_replication_level_3",
    "spatial_replication_level_4","proj_metadata_key"
  ), collapse = ", ")
  
  
  search_arg=parse_to_sql_search(...)
  
  # Testing SQL syntax
  conn <- src_postgres(
    dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")

  #taxa <- as.data.frame(tbl(conn, "taxa_table"))
  #proj <- as.data.frame(tbl(conn, "project_table"))
  #stud <- as.data.frame(tbl(conn, "study_site_table"))
  # col names
  #col_nam <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
  #                                         table_name='count_table'")))[,1]

  table_all <- tbl(conn, sql(
    paste(
          # Count data
          "SELECT",default_columns,", count_observation",
          "FROM count_table",
          "JOIN taxa_table ON count_table.taxa_count_fkey = taxa_table.taxa_table_key",
          "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
          "site_in_project_table.site_in_project_key",
          "JOIN project_table ON site_in_project_table.project_table_fkey =",
          "project_table.proj_metadata_key",
          "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
          "study_site_table.study_site_key",
          "WHERE", search_arg,
          
          "UNION ALL",
          # Biomass data
          "SELECT",default_columns,", biomass_observation",
          "FROM biomass_table",
          "JOIN taxa_table ON biomass_table.taxa_biomass_fkey = taxa_table.taxa_table_key",
          "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
          "site_in_project_table.site_in_project_key",
          "JOIN project_table ON site_in_project_table.project_table_fkey =",
          "project_table.proj_metadata_key",
          "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
          "study_site_table.study_site_key",
          "WHERE", search_arg,
          
          "UNION ALL",
          # percent cover data
          "SELECT",default_columns,", percent_cover_observation",
          "FROM percent_cover_table",
          "JOIN taxa_table ON percent_cover_table.taxa_percent_cover_fkey = taxa_table.taxa_table_key",
          "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
          "site_in_project_table.site_in_project_key",
          "JOIN project_table ON site_in_project_table.project_table_fkey =",
          "project_table.proj_metadata_key",
          "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
          "study_site_table.study_site_key",
          "WHERE", search_arg,
          
          "UNION ALL",
          # individual data
          "SELECT",default_columns,", individual_observation",
          "FROM individual_table",
          "JOIN taxa_table ON individual_table.taxa_individual_fkey = taxa_table.taxa_table_key",
          "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
          "site_in_project_table.site_in_project_key",
          "JOIN project_table ON site_in_project_table.project_table_fkey =",
          "project_table.proj_metadata_key",
          "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
          "study_site_table.study_site_key",
          "WHERE", search_arg,
          
          "UNION ALL",
          # density data
          "SELECT",default_columns,", density_observation",
          "FROM density_table",
          "JOIN taxa_table ON density_table.taxa_density_fkey = taxa_table.taxa_table_key",
          "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
          "site_in_project_table.site_in_project_key",
          "JOIN project_table ON site_in_project_table.project_table_fkey =",
          "project_table.proj_metadata_key",
          "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
          "study_site_table.study_site_key",
          "WHERE", search_arg)))
  
  output_data <- as.data.frame(table_all)

  return(output_data)

}
