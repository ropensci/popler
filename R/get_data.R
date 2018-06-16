#' Download data from the popler database
#'
#' This function downloads LTER studies contained in the popler database.
#' The user can download data directly, using a logical expression, or using 
#' objects created by `browse`.
#' @param ... A list of one or two objects: an object produced by browse,
#'  a logical expression, or both.
#' @param add_vars A string to specify which variables the user wants to 
#' add to the default variables used in a query. 
#' @param subtract_vars A string to specify which, among the default 
#' variables, the user wishes to discard in queries to the database 
#' @param cov_unpack Should covariates be unpacked? This argument uses
#'  function `cov_unpack` to extract the variables contained in the 
#'  variable `covariates`, and combine the new columns with the default output.
#' @return A data frame of the selected data. 
#' @return This data fame is of class "popler", "get_data", and "data.frame". 
#' @importFrom dplyr %>% select
#' @importFrom rlang .data
#' @export
#' @examples
#' 
#' \dontrun{
#' # browse a study, then get the data associated with it
#' parasite = pplr_browse(proj_metadata_key == 25)
#' gh_data = pplr_get_data(parasite)
#' 
#' # further subset this data set, based on year
#' gh_data_96_99 = pplr_get_data(parasite, year > 1995 & year < 2000)
#' 
#' # insect data sets from the SEV lter site
#' insect_sev = pplr_browse(class == "Insecta" & lterid == "SEV")
#' insect_25_yrs96_99 = pplr_get_data(insect_sev, year > 1995 & year < 2000)
#' 
#' insect_21_25 = pplr_get_data((proj_metadata_key == 43 | proj_metadata_key == 25) & year < 1995 )
#'}

# Function that connects and gathers the information from the database

pplr_get_data <- function(..., add_vars = NULL, subtract_vars = NULL,
                     cov_unpack = FALSE){
  # open connection to database
  conn <- db_open()
  # define possible variables ------------------------------------------
  
  # possible variables 
  possible_vars  <- vars_query(conn)
  # all potential variables in a query
  all_vars       <- possible_vars$all_vars
  # default variables 
  default_vars   <- possible_vars$default_vars
  # selected variables -------------------------------------------------
  # extract the variables contained in the logical expressions specified 
  # in '...'
  
  # concatenate logical expressions specified in the '...' argument
  # expressions can be specified explicitly, implicitly (through an 
  # object produced by browse()), or both 
  c_calls <- concatenate_queries(...)
  
  # update the concatenated calls, in case c_calls contains
  # "structure" or "treatment"
  updated_calls <- call_update(c_calls)
  
  # extract the variables specified in the calls' expressions
  # this is done to include `expr_vars` in the query, if some
  # of `expr_vars` do not match `default_vars`.
  expr_vars <- expr_vars_get(all_vars, updated_calls)
  
  # variables that appear either as default, added manually, or inherited
  # from a logical operation
  subset_vars <- unique(c(default_vars, add_vars, expr_vars))
  # 'subset_vars' minus variables subtracted manually via argument
  # 'subtract_vars' 
  vars_select <- paste( setdiff(subset_vars, subtract_vars), collapse = ", ")
  
  
  # translate R logical expressions in '...' into SQL ---------------------
  sql_condition <- parse_to_sql_search(updated_calls)
  
  
  # query -----------------------------------------------------------------
  
  # query popler online
  output_data <- pplr_query(conn, vars_select, sql_condition)
  
  if(dim(output_data)[1] < 1) {
    stop('No data found. Check to make sure query is correct',
         call. = FALSE)
  }
  
  # format output ---------------------------------------------------------
  
  # replace -99999, but only for numeric variables
  num_repl <- sapply(output_data, is.numeric) %>% as.vector()
  output_data[,num_repl] <- lapply(output_data[ ,num_repl],
                                    function(x) {
                                      replace(x, x == -99999, NA)
                                    }
                                   ) %>% as.data.frame()
  
  # remove variables that whose content is just "NA"
  output_data <- base::Filter(function(x) !all(x == "NA"), output_data)
  
  # Change "ordr" and "clss" to "order" and "class"
  output_data <- colname_change("clss", "class", output_data)
  output_data <- colname_change("ordr", "order", output_data)
  output_data <- colname_change("count_observation", "abundance_observation", 
                                output_data)
  
  # unpack the covariates?
  if(cov_unpack == TRUE) {
    
    output_data <- output_data %>%
      dplyr::select(-.data$covariates) %>%
      cbind(pplr_cov_unpack(output_data))
  }
  
  # outputs -----------------------------------------------------------------
  
  # assign class
  output_data <- structure(output_data, 
                unique_projects = unique(output_data$proj_metadata_key),
                unique_authors  = unique(output_data[ ,c("proj_metadata_key",
                                                         "authors",
                                                         "authors_contact")]),
                class = c("get_data", class(output_data)) 
  )
  
  # Informational message
  data_message(output_data)
  
  db_close(conn)
  
  return(output_data)
  
}


#' @noRd
# obtain all potential columns 
vars_query <- function(conn){
  
  proj_sql <- paste0("SELECT column_name ",
                     "FROM information_schema.columns ",
                     "WHERE table_name = 'project_table'")
  lter_sql <-  paste0("SELECT column_name ",
                      "FROM information_schema.columns ",
                      "WHERE table_name = 'lter_table'")
  site_sql <- paste0("SELECT column_name ",
                     "FROM information_schema.columns ",
                     "WHERE table_name = 'study_site_table'")
  s_i_p_sql <- paste0("SELECT column_name ",
                      "FROM information_schema.columns ",
                      "WHERE table_name = 'site_in_project_table'")
  taxa_sql <- paste0("SELECT column_name ",
                     "FROM information_schema.columns ",
                     "WHERE table_name = 'taxa_table'")
  abund_sql <- paste0("SELECT column_name ",
                      "FROM information_schema.columns ",
                      "WHERE table_name = 'count_table'")
  #list variables from the 6 tables relevant to standard popler queries  
  proj_vars <- query_get(conn,
                         proj_sql)[ ,1]
  lter_vars <- query_get(conn, 
                         lter_sql)[ ,1]
  site_vars <- query_get(conn, 
                         site_sql)[ ,1]
  s_i_p_vars <- query_get(conn, 
                          s_i_p_sql)[ ,1]
  taxa_vars <- query_get(conn, 
                         taxa_sql)[ ,1]
  abund_vars <- query_get(conn, 
                          abund_sql)[ ,1]
  
  # a vector containing all variables
  all_vars <- c(proj_vars, lter_vars,
                site_vars, s_i_p_vars,
                taxa_vars, abund_vars)
  
  # remove some variables that are in the database but we don't want to return
  # this is a temporary fix until we remove those columns from the database.
  all_vars <- all_vars[!all_vars %in% c("currently_funded",
                                        "homepage",
                                        "current_principle_investigator")]
  
  # a vector of "default" variables
  default_vars  <- c("authors", "authors_contact",
                     "year", "day", "month",
                     "sppcode", "genus", "species", "datatype",
                     "spatial_replication_level_1_label",
                     "spatial_replication_level_1",
                     "spatial_replication_level_2_label",
                     "spatial_replication_level_2",
                     "spatial_replication_level_3_label",
                     "spatial_replication_level_3",
                     "spatial_replication_level_4_label",
                     "spatial_replication_level_4",
                     "spatial_replication_level_5_label",
                     "spatial_replication_level_5",
                     "proj_metadata_key",
                     "structure_type_1", "structure_type_2",
                     "structure_type_3", "structure_type_4",
                     "treatment_type_1", "treatment_type_2",
                     "treatment_type_3",
                     "covariates" 
  )
  
  return(list(all_vars = all_vars, default_vars = default_vars))
  
}


#' @importFrom lazyeval lazy_dots
#' @noRd
# a function to concatenate browse() outputs and new arguments
concatenate_queries <- function(...){
  
  # lazy_dots eval get_data query
  Q <- lazyeval::lazy_dots(...)
  
  # a list to store the outputs
  out <- list()
  
  # counters to check whether more than one browse() or new calls are used
  browse_calls <- 0
  new_calls <- 0
  
  if(length(Q) > 2){
    stop("You cannot enter more than two arguments:\n1) ",
         "an object returned by browse() and/or\n2) a logical", 
         "statement\nPlease refer to the '...' argument in ?get_data.")
  }
  
  # loop over all inputs
  for(i in seq_len(length(Q))) {
    if(class(Q[[i]]$expr) == "name") {
      
      # if class of object is "name" evaluate it to get
      # original browse() query
      tmp <- eval(Q[[i]]$expr)
      
      # if this variable isn't a popler object, throw an error
      if(class(tmp)[1] != "popler"){
        stop(paste0("Error using the following argument:\n\n      ", 
                    Q[[i]]$expr,
                    "\n\n  Only logical expressions or outputs from the ",
                    "'browse()' function may be used"))
      }
      
      # store search argument as output
      out[[i]] <- attributes(tmp)$search_expr
      
      # update counter
      browse_calls <- browse_calls + 1
    } else { 
      
      # if class of object is "call"...
      if(grepl("browse[(]", deparse(Q[[i]]$expr))) {
        
        # if the call is to browse(), evaluate is and then get the search arg
        out[[i]] <- attributes(eval(Q[[i]]$expr))$search_expr
        
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
    stop("You cannot enter more than one browse() argument.\n",
         "Please refer to the '...' argument in ?get_data.")
  }
  if(new_calls > 1){
    stop("You cannot enter more than one logical expression.\n ",
         "Please refer to the '...' argument in ?get_data.")
  }

  LoopOut <- paste0(unlist(out), collapse = "&") 
  TextToParse <- paste0("substitute(", LoopOut, ")", collapse = "")
  
  # return a single logical call
  return(eval(parse(text = TextToParse)))
  
}

#' @noRd
# Identify which "search_expr" belong to "all_vars"
expr_vars_get <- function(all_cols, inherit_logical){
  inherit_elem <- as.character(inherit_logical)
  
  # change column names 
  inherit_elem <- colname_change("clss", "class", inherit_elem)
  inherit_elem <- colname_change("ordr", "order", inherit_elem)
  
  inds <- NULL
  for(i in seq_len(length(all_cols))){
    if(any(grepl(all_cols[i], inherit_elem))){
      inds <- c(inds, i)  
    }
  }
  return(unique(all_cols[inds]))
}


# query popler
#' @noRd
pplr_query <- function(conn, vars_select, sql_condition){
  
  if(length(sql_condition) == 0) {
    stop("No logical expression specified. Please specify what ",
         "data you wish to download from popler" )
  }
  # table specific variables
  vars <- list()
  vars$count_table <- gsub("treatment_type_", 
                           "count_table.treatment_type_",
                           vars_select)
  vars$biomass_table <- gsub("treatment_type_", 
                             "biomass_table.treatment_type_",
                             vars_select)
  vars$percent_cover_table <- gsub("treatment_type_", 
                                   "percent_cover_table.treatment_type_",
                                   vars_select)
  vars$density_table <- gsub("treatment_type_", 
                             "density_table.treatment_type_",
                             vars_select)
  vars$individual_table <- gsub("treatment_type_",
                                "individual_table.treatment_type_",
                                vars_select)
  
  output_data <- query_get(conn, paste(
    # Count data
    "SELECT",vars$count_table,", count_observation",
    "FROM count_table",
    "JOIN taxa_table ",
    "ON count_table.taxa_count_fkey = taxa_table.taxa_table_key",
    "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
    "site_in_project_table.site_in_project_key",
    "JOIN project_table ON site_in_project_table.project_table_fkey =",
    "project_table.proj_metadata_key",
    "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
    "study_site_table.study_site_key",
    "JOIN lter_table ON study_site_table.lter_table_fkey =",
    "lter_table.lterid",
    "WHERE", sql_condition,
    
    "UNION ALL",
    # Biomass data
    "SELECT",vars$biomass_table,", biomass_observation",
    "FROM biomass_table",
    "JOIN taxa_table ",
    "ON biomass_table.taxa_biomass_fkey = taxa_table.taxa_table_key",
    "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
    "site_in_project_table.site_in_project_key",
    "JOIN project_table ON site_in_project_table.project_table_fkey =",
    "project_table.proj_metadata_key",
    "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
    "study_site_table.study_site_key",
    "JOIN lter_table ON study_site_table.lter_table_fkey =",
    "lter_table.lterid",
    "WHERE", sql_condition,
    
    "UNION ALL",
    # percent cover data
    "SELECT",vars$percent_cover_table,", percent_cover_observation",
    "FROM percent_cover_table",
    "JOIN taxa_table ",
    "ON percent_cover_table.taxa_percent_cover_fkey ",
    "= taxa_table.taxa_table_key",
    "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
    "site_in_project_table.site_in_project_key",
    "JOIN project_table ON site_in_project_table.project_table_fkey =",
    "project_table.proj_metadata_key",
    "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
    "study_site_table.study_site_key",
    "JOIN lter_table ON study_site_table.lter_table_fkey =",
    "lter_table.lterid",
    "WHERE", sql_condition,
    
    "UNION ALL",
    # individual data
    "SELECT",vars$individual_table,", individual_observation",
    "FROM individual_table",
    "JOIN taxa_table ",
    "ON individual_table.taxa_individual_fkey = taxa_table.taxa_table_key",
    "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
    "site_in_project_table.site_in_project_key",
    "JOIN project_table ON site_in_project_table.project_table_fkey =",
    "project_table.proj_metadata_key",
    "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
    "study_site_table.study_site_key",
    "JOIN lter_table ON study_site_table.lter_table_fkey =",
    "lter_table.lterid",
    "WHERE", sql_condition,
    
    "UNION ALL",
    # density data
    "SELECT",vars$density_table,", density_observation",
    "FROM density_table",
    "JOIN taxa_table ",
    "ON density_table.taxa_density_fkey = taxa_table.taxa_table_key",
    "JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =",
    "site_in_project_table.site_in_project_key",
    "JOIN project_table ON site_in_project_table.project_table_fkey =",
    "project_table.proj_metadata_key",
    "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
    "study_site_table.study_site_key",
    "JOIN lter_table ON study_site_table.lter_table_fkey =",
    "lter_table.lterid",
    "WHERE", sql_condition
  ))
  
  return(output_data)
  
}


#' @noRd
# informational message at every download
data_message <- function(x){
  
  if( length(unique(x$proj_metadata_key)) == 1)
    message(paste0("You have downloaded data from ",
                   length(unique(x$proj_metadata_key)),
                   " project.\nThe identification number of this project is:",
                   paste0(" ", 
                          unique(x$proj_metadata_key),
                          collapse=", "),
                   "."),
            "\n
            IMPORTANT NOTICE:\n
            If you are about to use this data in a ",
            "formal publication as courtesy, please:
            1) Contact the investigators of each project. 
            2) Acknowledge funding sources, if these are provided ",
            "in the metadata.   
            Access metadata by using function metadata_url() on this ", 
            "object. \n")
  
  else {
    message("\n", paste0("You have downloaded data from ",
                         length(unique(x$proj_metadata_key)),
                         " projects. \nThe identification numbers of these projects are: ",
                         paste0(unique(x$proj_metadata_key),
                               collapse = ", "),
                         "."),
            "\n
            IMPORTANT NOTICE:\n
            If you are about to use this data in a ",
            "formal publication as courtesy, please:
            1) Contact the investigators of each project. 
            2) Acknowledge funding sources, if these are provided ",
            "in the metadata.   
            Access metadata by using function metadata_url() on this ", 
            "object. \n")
  }
  
}
