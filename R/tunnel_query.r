#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database.
#' @param ... A logical expression using R synthax.
#' @param add_columns A string that to specify which columns to add in the default columns used in queries to the database. See Details for default columns.
#' @param subtract_columns A string to specify which columns, among the default columsn, the user wishes to discard in queries to the database 
#' @return A data frame of the selected data.
#' @export
#' @examples
#' #returns ALL DATA referring to "Abietinaria"
#' get_fxn(genus=="Abietinaria")
#'
#' #returns data from the study number 7
#' get_fxn(medarecordid==7)


#Function that connects and gathers the information from the database
tunnel_query = function(..., add_columns = NULL, subtract_columns = NULL,
                        metadata = T){

  default_columns = c(
    "year","day","month",                      
    "genus","species",                    
    "structure","datatype",         
    "spatial_replication_level_1","spatial_replication_level_2","spatial_replication_level_3",
    "spatial_replication_level_4",
    "proj_metadata_key"
  )
  
  # Add or subtract, or define columns
  default_columns = c(default_columns,add_columns)
  select_columns = paste( setdiff(default_columns,subtract_columns), collapse = ", ")
  
  # subset argument
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
          "SELECT",select_columns,", count_observation",
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
          "SELECT",select_columns,", biomass_observation",
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
          "SELECT",select_columns,", percent_cover_observation",
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
          "SELECT",select_columns,", individual_observation",
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
          "SELECT",select_columns,", density_observation",
          "FROM density_table",
          "JOIN taxa_table ON density_table.taxa_density_fkey = taxa_table.taxa_table_key",
          "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
          "site_in_project_table.site_in_project_key",
          "JOIN project_table ON site_in_project_table.project_table_fkey =",
          "project_table.proj_metadata_key",
          "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
          "study_site_table.study_site_key",
          "WHERE", search_arg
    )
  ))
  
  output_data <- as.data.frame(table_all)

  if(metadata == T) {
    unique_projects = unique(output_data$proj_metadata_key)
    for(i in 1:length(unique_projects)){
      
      metadata_webpage(unique_projects[i])
      
    }
  }
  
  #names(output_data)[grep("count_observation",names(output_data))] <- "abundance"
  
  return(output_data)

}
