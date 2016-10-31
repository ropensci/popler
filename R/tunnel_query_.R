#' Browse the popler database, without lazy evaluation
#'
#' This function fetches LTER studies contained in the popler database without lazy evaluation. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database. 
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
tunnel_query_=function(...){

  # Database columns that will be stored as an attribute in the popler Class
  searchable_columns = c(
    # site_table
    'lterid', 'siteid', 'lat', 'long', 'decsript',

    # main_table
    'metarecordid', 'samplingprotocol', 'structured', 'studystartyr', 'studyendyr',
    'sitestartyr', 'siteendyr', 'samplefreq', 'totalobs', 'studytype', 'community',

    # -- Spatial replication extent (numeric value)
    'sp_rep1_ext', 'sp_rep2_ext', 'sp_rep3_ext', 'sp_rep4_ext',
    # -- Spatial replication units (variable character)
    'sp_rep1_ext_units', 'sp_rep2_ext_units', 'sp_rep3_ext_units', 'sp_rep4_ext_units',
    # -- Spatial replication original label (variable character)
    'sp_rep1_label', 'sp_rep2_label', 'sp_rep3_label', 'sp_rep4_label',
    # -- Number of unique levels within each group of Spatial replication (integer)
    'sp_rep1_uniquelevels', 'sp_rep2_uniquelevels', 'sp_rep3_uniquelevels', 'sp_rep4_uniquelevels',
    'author', 'knbid',

    # taxa_table
    'spp_code', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species',

    # raw_table
    'year', 'month', 'day', 'covariates', 'spt_rep1', 'spt_rep2', 'spt_rep3',
    'spt_rep4', 'trt_label'
  )
  
  search_arg=parse_to_sql_search(...,lazy=F)
  
  #start_SSH()                                         # start SSH tunneling
  #conn <- open_DB_connection()                        # open a db connection
  # Testing SQL syntax
  conn <- src_postgres(
    dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")
  
  #taxa <- as.data.frame(tbl(conn, "taxa_table"))
  #proj <- as.data.frame(tbl(conn, "project_table"))
  #stud <- as.data.frame(tbl(conn, "study_site_table"))
  #col names
  #col_taxa <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
  #                                     table_name='taxa_table'")))[,1]
  
  table_all <- tbl(conn, sql(
    paste(
      # Count data
      "SELECT year, day, month, kingdom, phylum, clss, family, genus, species,",
      "structure, datatype, count_observation,",
      "spatial_replication_level_1, spatial_replication_level_2,", 
      "spatial_replication_level_3, spatial_replication_level_4,",
      "proj_metadata_key", # lterid, lat, lng, 
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
      "SELECT year, day, month, kingdom, phylum, clss, family, genus, species,",
      "structure,  datatype, biomass_observation,",
      "spatial_replication_level_1, spatial_replication_level_2,", 
      "spatial_replication_level_3, spatial_replication_level_4,",
      "proj_metadata_key", #lterid, lat, lng, 
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
      "SELECT year, day, month, kingdom, phylum, clss, family, genus, species,",
      "structure,  datatype, percent_cover_observation,",
      "spatial_replication_level_1, spatial_replication_level_2,", 
      "spatial_replication_level_3, spatial_replication_level_4,",
      "proj_metadata_key", #lterid, lat, lng, 
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
      "SELECT year, day, month, kingdom, phylum, clss, family, genus, species,",
      "structure,  datatype, individual_observation,",
      "spatial_replication_level_1, spatial_replication_level_2,", 
      "spatial_replication_level_3, spatial_replication_level_4,",
      "proj_metadata_key", #lterid, lat, lng, 
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
      "SELECT year, day, month, kingdom, phylum, clss, family, genus, species,",
      "structure,  datatype, density_observation,",
      "spatial_replication_level_1, spatial_replication_level_2,", 
      "spatial_replication_level_3, spatial_replication_level_4,",
      "proj_metadata_key", #lterid, lat, lng, 
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
  #close_DB_connection(conn)                         # close the db connection
  #stop_SSH()                                        # stop SSH tunneling

  return(output_data)
  
}
