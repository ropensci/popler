#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database.
#' @param ... A list of one or two objects: an object produced by browse, a logical expression, or both.
#' @param add_vars A string to specify which variables the user wants to add to the default variables used in a query. 
#' @param subtract_vars A string to specify which, among the default variables, the user wishes to discard in queries to the database 
#' @return A data frame of the selected data.
#' @export
#' @examples
#' 
#' # browse a study, then get the data associated with it
#' parasite = browse(proj_metadata_key == 25)
#' gh_data = get_data(parasite)
#' 
#' # further subset this data set, based on year
#' gh_data_96_99 = get_data(parasite, year > 1995 & year < 2000)
#' 
#' # insect data sets from the SEV lter site
#' insect_sev = browse(class == "Insecta" & lterid == "SEV")
#' insect_25_yrs96_99 = get_data(insect_sev, year > 1995 & year < 2000 & proj_metadata_key == 25)
#' 
#' insect_21_25 = get_data((proj_metadata_key == 43 | proj_metadata_key == 25) & year < 1995 )


# Function that connects and gathers the information from the database
get_data <- function(..., #browsed_data = NULL, subset = NULL,
                     add_vars = NULL, subtract_vars = NULL){
  
  # open connection to database
  conn <- src_postgres(dbname="popler_3", password="bigdata",
                       host="ec2-54-214-212-101.us-west-2.compute.amazonaws.com", 
                       port=5432, user="other_user")
  
  
  # define possible variables ---------------------------------------------------------------

  # possible variables 
  potential_vars  <- query_vars(conn)
  # all potential variables in a query
  all_columns     <- potential_vars$all_vars
  # default variables 
  default_columns <- potential_vars$default_vars
  
  
  # selected variables --------------------------------------------------------------------
  
  # extract the variables contained in the logical expressions contained in '...'
  
  # concatenate logical expressions explicitly or implicitly (through browse()) declared in the '...' argument
  c_calls       <- concatenate_queries(...)
  # update the concatenated calls, in case c_calls contains "structure" or "treatment"
  all_calls     <- popler:::call_update( c_calls )
  # extract the variables specified in the calls
  # this is done to prevent querying variables database
  inherit_vars  <- inherit_search(all_columns, all_calls)
  
  
  # variables that appear either as default, added manually, or inherited from a logical operation
  actual_vars     <- unique( c(default_columns, add_vars, inherit_vars) )
  # 'actual_vars' minus variables subtracted manually via argument 'subtract_vars' 
  select_vars     <- paste( setdiff(actual_vars, subtract_vars), collapse = ", ")
  
  
  # translate R logical expressions in '...' into SQL --------------------------------------------------------------------
  search_arg      <- parse_to_sql_search( all_calls )
  
  
  # query ---------------------------------------------------------------------------------
  
  # query popler online
  output_data <- query_popler(conn, select_vars, search_arg)
  
  # Change "ordr" and "clss" to "order" and "class"
  output_data <- colname_change("clss", "class", output_data)
  output_data <- colname_change("ordr", "order", output_data)
  
  
  # outputs -------------------------------------------------------------------------------
  
  # assign class
  output_data <- structure(output_data, 
                           unique_projects = unique(output_data$proj_metadata_key),
                           unique_authors  = unique(output_data[,c("proj_metadata_key",
                                                                   "authors",
                                                                   "authors_contact")]),
                           class = c("popler", class(output_data)) 
  )
  
  # Informational message
  data_message(output_data)
  
  return(output_data)
  
}


#' @noRd
# obtain all potential columns 
query_vars <- function(conn){
  
  #list variables from the 6 tables relevant to standard popler queries  
  proj_vars     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE table_name = 'project_table'")))[,1]
  lter_vars     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE 
                                                table_name = 'lter_table'")))[,1]
  site_vars     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'study_site_table'")))[,1]
  s_i_p_vars    <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'site_in_project_table'")))[,1]
  taxa_vars     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'taxa_table'")))[,1]
  abund_vars    <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                                table_name = 'count_table'")))[,1]
  
  # a vector containing all variables
  all_vars      <- c(proj_vars,lter_vars,site_vars, s_i_p_vars, taxa_vars, abund_vars)
  
  # a vector of "default" variables
  default_vars  <- c("year","day","month","genus","species","datatype",         
                     "spatial_replication_level_1","spatial_replication_level_2",
                     "spatial_replication_level_3","spatial_replication_level_4","spatial_replication_level_5",
                     "authors","authors_contact","proj_metadata_key",
                     "structure_type_1","structure_type_2","structure_type_3","structure_type_4",
                     "treatment_type_1","treatment_type_2","treatment_type_3",
                     "covariates" 
  )
  
  return( list(all_vars = all_vars, default_vars = default_vars) )
  
}


# a function to concatenate browse() outputs and new arguments
concatenate_queries = function(...){
  
  # lazy_dots eval get_data query
  Q <- lazyeval::lazy_dots(...)
  
  # a list to store the outputs
  out <- list()
  
  # counters to check whether more than one browse() or new calls are used
  browse_calls <- 0
  new_calls    <- 0
  
  if(length(Q) > 2){
    stop("You cannot enter more than two arguments:\n1) an object returned by browse() and/or\n2) a logical statement\nPlease refer to the '...' argument in ?get_data.")
  }
  
  # loop over all inputs
  for(i in 1:length(Q)){
    if(class(Q[[i]]$expr) == "name"){
      
      # if class of object is "name" evaluate it to get original browse() query
      tmp <- eval(Q[[i]]$expr)
      
      # if this variable isn't a popler object, throw an error
      if(class(tmp)[1] != "popler"){
        stop(paste0("Error using the following argument:\n\n      ", 
                    Q[[i]]$expr,
                    "\n\n  Only logical expressions or outputs from the 'browse()' function may be used"))
      }
      
      # store search argument as output
      out[[i]] <- attributes(tmp)$search_argument
      
      # update counter
      browse_calls <- browse_calls + 1
    } else { 
      
      # if class of object is "call"...
      if(grepl("browse[(]",deparse(Q[[i]]$expr))) {
        
        # if the call is to browse(), evaluate is and then get the search arg
        out[[i]] <- attributes(eval(Q[[i]]$expr))$search_argument
        
        # update browse_calls counter
        browse_calls <- browse_calls + 1
      } else {
        # just save the expression
        out[[i]] <- Q[[i]]$expr
        
        # update calls counter
        new_calls <- new_calls + 1
      }
    }
  }
  
  # if either call counter is more than 1, call an error
  if(browse_calls > 1){
    stop("You cannot enter more than one browse() argument.\n  Please refer to the '...' argument in ?get_data.")
  }
  if(new_calls > 1){
    stop("You cannot enter more than one logical expression.\n  Please refer to the '...' argument in ?get_data.")
  }
  
  # return a single logical call
  return(eval(parse(text = paste0(unlist(out),    collapse="&") %>%
                      paste0("substitute(", . ,")", collapse="")))
  )
}

# Identify which "search_arguments" belong to "all_columns"
inherit_search <- function(all_cols, inherit_logical){
  
  inherit_elem <- as.character(inherit_logical)
  
  # change column names 
  inherit_elem <- colname_change("clss", "class", inherit_elem)
  inherit_elem <- colname_change("ordr", "order", inherit_elem)
  
  inds = NULL
  for(i in 1:length(all_cols)){
    if( any( grepl(all_cols[i], inherit_elem) ) ){
      inds = c(inds,i)  
    }
  }
  return( unique(all_cols[inds]) )
}


# query popler
query_popler <- function(connection, select_vars, search_arg){
  
  if(length(search_arg) == 0) stop( "No logical expression specified. Please specify what data you wish to download from popler" )
  
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



##### UNUSED FUNCTIONS #########################################################