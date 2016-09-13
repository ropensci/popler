#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database.
#' @param A logical expression using R synthax.
#' @return A data frame of the selected data.
#' @export
#' @examples
#' #returns ALL DATA referring to "Abietinaria"
#' get_fxn(genus=="Abietinaria")
#'
#' #returns data from the study number 7
#' get_fxn(medarecordid==7)


#Function that connects and gathers the information from the database
tunnel_query=function(...){

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


  search_arg=parse_to_sql_search(...)

  #start_SSH()                                         # start SSH tunneling
  #conn <- open_DB_connection()                           # open a db connection
  # Testing SQL syntax
  conn <- src_postgres(
    dbname="popler", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")

  #First TRUE search
  table_search <- tbl(conn, sql(
    paste("SELECT year, day, month, kingdom, phylum, clss, family, genus, species,",
          "spt_rep1, spt_rep2, spt_rep3, spt_rep4,",
          "structure, individ, unitobs, samplingprotocol,",
          "lterid, lat, lng",
          "FROM raw_table",
          "JOIN taxa_table ON raw_table.raw_taxaid = taxa_table.taxaid",
          "JOIN main_table ON taxa_table.taxa_lter_proj_site =",
          "main_table.lter_proj_site",
          "JOIN site_table ON main_table.main_siteid = site_table.siteid",
          "WHERE", search_arg)))

  output_data <- as.data.frame(table_search,n=-1) #
  rm(conn)
  #close_DB_connection(conn)                         # close the db connection
  #stop_SSH()                                        # stop SSH tunneling

  return(output_data)

}
