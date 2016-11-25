#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database.
#' @param browsed_data A on object produced by the function browse() 
#' @param subset A logical argument A string that to specify which columns to add in the default columns used in queries to the database. See Details for default columns.
#' @param add_columns A string to specify which columns the user wants to add in queries to the database 
#' @param subtract_columns A string to specify which, among the default columns, the user wishes to discard in queries to the database 
#' @param metadata Should the function open the webpage containing the metadata for each one of the projects downloaded? 
#' @return A data frame of the selected data.
#' @export
#' @examples
#' 
#' # browse a study, then get the data associated with it
#' grasshop = browse(proj_metadata_key == 21)
#' gh_data = get_data(grasshop)
#' # further subset this data set, based on year
#' gh_data_96_99 = get_data(grasshop, year > 1995 & year < 2000)


# Function that connects and gathers the information from the database
get_data <- function(browsed_data = NULL, subset = NULL,
                     add_columns = NULL, subtract_columns = NULL, metadata = FALSE){
  
  # define possible columns ------------------------------------------------------------
  
  # possible columns 
  potential_vars  <- popler:::query_cols()
  all_columns     <- potential_vars$all_cols
  default_columns <- potential_vars$default_cols
  
  # Add/subtract, or define columns ----------------------------------------------------
  inherit_vars  <- NULL
  if( !is.null(browsed_data) ){
    inherit_elem    <- as.character( attributes(browsed_data)$search_argument )
    inherit_elem    <- gsub("order", "ordr", inherit_elem)
    inherit_elem    <- gsub("class", "clss", inherit_elem)
    inherit_vars    <- popler:::inherit_search(all_columns, inherit_elem) 
    inherit_browse  <- unique(inherit_vars)
  } 
  if( !is.null(subset) ){
    inherit_elem    <- as.character( substitute(subset) )
    inherit_elem    <- gsub("order", "ordr", inherit_elem)
    inherit_elem    <- gsub("class", "clss", inherit_elem)
    inherit_vars    <- popler:::inherit_search(all_columns, inherit_elem) 
    inherit_subset  <- unique(inherit_vars)
  } 
  default_columns   <- unique( c(inherit_browse, inherit_subset) )
  
  default_columns <- c(default_columns, add_columns)
  select_columns  <- paste( setdiff(default_columns, subtract_columns), collapse = ", ")
  default_columns <- unique(default_columns)
    
  # subset argument ------------------------------------------------------------------
  search_arg_1 = search_arg_2 = NULL 
  if( !is.null(substitute(subset)) ) search_arg_1 <- parse_to_sql_search( substitute(subset) )
  if( !is.null(browsed_data) )       search_arg_2 <- parse_to_sql_search( attributes(browsed_data)$search_argument )

  # combine search
  if( !is.null(substitute(subset)) & !is.null(browsed_data) ){
    
    search_arg = paste(search_arg_1, search_arg_2, sep = " AND ")
  } else{
    search_arg = paste0(search_arg_1, search_arg_2)
  }
  
  # Testing SQL syntax
  conn <- src_postgres(
    dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")
  
  
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
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
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
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
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
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
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
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
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
      "JOIN lter_table ON study_site_table.lter_table_fkey =",
      "lter_table.lterid",
      "WHERE", search_arg
    )
  ))
  
  output_data <- as.data.frame(table_all)
  
  if(metadata == T) {
    unique_projects = unique(output_data$proj_metadata_key)
    for(i in 1:length(unique_projects) ){
      
      metadata_webpage(unique_projects[i])
      
    }
  }
  
  # assign class
  output_data <- structure(output_data, 
                           class = c("popler", class(output_data)) 
                           )
  
  return(output_data)
  
}
