# function updates "treatment" and "structure" in a user's query to match the
# appropriate columns in the database.

#' @importFrom stringr str_extract_all 
#' @noRd

call_update = function(query){
  
  # query is some user's input (i.e. a query) to the pplr_browse() function
  
  # if the query is null, don't do anything; just return the query
  if(is.null(query)){ 
    return(query) 
  }
  
  # define some regex strings to:
  f_logic <- "[|]+[!]|[&]+[!]|[&|]+"  # ...find logical operators
  f_compn <- "![=]|[<=>]=|<|>|%in%"   # ...find comparison operators
  
  # defne replacement strings for treatment and structure
  r_trt <- "(treatment_type_1 @@ @ @@@ 
  treatment_type_2 @@ @ @@@ 
  treatment_type_3 @@ @)"
  
  r_str <- "(structured_type_1 @@ @ @@@ 
  structured_type_2 @@ @ @@@ 
  structured_type_3 @@ @ @@@ 
  structured_type_4 @@ @)"
  
  # convert query to character string and remove spaces
  query_str <- gsub(" ", "", paste0(deparse(query), collapse = ""))
  
  # split query_str on logical operators to get individual arguments
  query_arg <- unlist(strsplit(query_str, f_logic))
  
  # make a matrix where col1 is the LHS of each query_arg and col2 is the RHS
  LHS_RHS <- matrix(unlist(strsplit(query_arg, f_compn)),
                    ncol = 2,
                    byrow = TRUE)
  
  # find logical operators in the query by searching query_str
  logic <- c(stringr::str_extract_all(query_str, f_logic)[[1]],"")
  
  # find comparison operators in each argument by searching query_arg
  comps <- unlist(stringr::str_extract_all(query_arg, f_compn))
  
  # make a list where each element is an argument in the query, updating LHS of
  # an argument when necessary
  query_list = list()
  for(i in seq_len(nrow(LHS_RHS))){
    
    # determine operator in case treatment or structure strings need to change
    op <- ifelse(comps[i] == "!=", "&", "|")
    
    if(LHS_RHS[i, 1] == "treatment"){
      
      # update "treatment" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i,2]), 
                                  gsub("@@", comps[i], 
                                       gsub("@@@", op, r_trt))),
                             logic[i])
      
    } else if(LHS_RHS[i,1] == "structure"){
      
      # update "structure" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i, 2]),
                                  gsub("@@", comps[i],
                                       gsub("@@@", op, r_str))),
                             logic[i])
      
    } else{
      
      # for any other LHS, just put the query back together
      query_list[i] = paste(LHS_RHS[i,1], comps[i], 
                            string_eval_local(LHS_RHS[i,2]), logic[i])
      
    }
  }
  # slight rewriting to avoid using the "." placeholder from magrittr.
  # this should alleviate NOTES in check, even if it is slightly
  # more verbose
  QueryOut <- paste0(unlist(query_list), collapse = "") 
  TextToParse <- paste0("substitute(", QueryOut ,")", collapse = "")
  
  # collapse query_list to a single string, convert to expression, and return it
  return(eval(parse(text = TextToParse)))
}


#' @noRd
# given a browse() object or a get_data() object, returns an identical browse
# object with full_tbl=TRUE
rebrowse <- function(input, ...){
  UseMethod("rebrowse")
}


#' @noRd 
rebrowse.browse <- function(input, ...) {
  pmk <- paste0(input$proj_metadata_key, collapse=",")
  return(eval(parse(text = paste0("pplr_browse(proj_metadata_key %in% c(",
                                  pmk,
                                  "), full_tbl=TRUE)"))))
}

#' @noRd
rebrowse.get_data <- function(input, ...) {
  pmk <- paste0(attributes(input)$unique_projects, collapse=",")
  return(eval(parse(text = paste0("pplr_browse(proj_metadata_key %in% c(", 
                                  pmk,
                                  "), full_tbl=TRUE)"))))
}


#' @noRd
# Converts factor columns into character format
factor_to_character <- function(x, full_tbl = FALSE){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[ ,i] <- as.character(x[ ,i])
  }
  return(x)
  
}

# generate main data table summary ---------------------------------------------

#' @title Update \code{popler}'s summary table
#' 
#' @description Automatically retrieve most up to date version of \code{popler}
#' summary table
#' 
#' @return This function is called for its side effect and does not return 
#' anything
#' 
#' @note The \code{summary_table} is often called internally by popler functions,
#'  but can also be accessed directly by calling \code{pplr_summary_table_import()}. 
#' 
#' @export
#' 

pplr_summary_table_update <- function(){
  
  message("Please wait while popler updates its summary table... this may take several minutes.")
  
  # set database connection
  conn <- db_open()
  
  # list all columns
  proj_cols   <- query_get(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'project_table'")[,1]
  lter_cols   <- query_get(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'lter_table'")[,1]
  taxa_cols   <- query_get(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'taxa_table'")[,1]
  search_cols <- paste( c(proj_cols,lter_cols,taxa_cols), collapse = ", ")
  
  search_cols[!search_cols %in% c("currently_funded",
                                  "homepage",
                                  "current_principle_investigator")]
  # open database connection, query result
  out <- query_get(conn, 
                   paste("SELECT", search_cols,
                         "FROM project_table
                         JOIN site_in_project_table ON project_table.proj_metadata_key = site_in_project_table.project_table_fkey
                         JOIN taxa_table ON site_in_project_table.site_in_project_key = taxa_table.site_in_project_taxa_key
                         JOIN study_site_table ON site_in_project_table.study_site_table_fkey = study_site_table.study_site_key
                         JOIN lter_table ON study_site_table.lter_table_fkey = lter_table.lterid"))
  
  # Select project-specific information 
  proj_info             <- out[,c(proj_cols,lter_cols)]
  
  # Discard replicated information
  out_proj              <- unique(proj_info)
  
  # strings for spatial_replication_level_X_number_of_unique_reps
  sr_colnames <- paste0("spatial_replication_level_",1:5,"_number_of_unique_reps")
  
  # change any spatial replication -99999 to NA
  out_proj[,sr_colnames][out_proj[,sr_colnames] == -99999] <- NA
  
  # add column for total spatial replicates
  out_proj$tot_spat_rep <- apply(out_proj[sr_colnames], 1, prod, na.rm=TRUE)
  
  # add column for number of spatial levels 
  out_proj$n_spat_levs  <- apply(!is.na(out_proj[sr_colnames]), 1, sum) 
  
  # reorder out_proj columns
  # out_proj  <- out_proj[c(1:11,46:47,12:36,58:59,37:45,48:57)]
  out_proj  <- out_proj[c(1:11, 46:47, 12:36, 61:62, 37:45, 48, 52:60, 49:51)]
  
  # return unique taxonomic information for each proj_metadata_key
  taxas    <- taxa_cols[! taxa_cols %in% c("taxa_table_key", 
                                           "site_in_project_taxa_key",
                                           "metadata_taxa_key")]
  
  out_taxa <- unique(out[,c("proj_metadata_key",taxas)])
  
  # merge project and species data
  summary_table <- merge(out_proj, out_taxa)
  
  # convert factors to characters
  summary_table <- factor_to_character(summary_table)
  
  # Case insensitive matching ("lower" everything)
  names(summary_table) <- tolower( names(summary_table) )
  
  # convert columns "ordr" to "order" and "clss" to "class"
  summary_table <- colname_change("clss", "class", summary_table)
  summary_table <- colname_change("ordr", "order", summary_table)
  
  # store main data table--------------------------------------------------
  st_file <- paste0(system.file("extdata", package = "popler"),"/summary_table.rda")
  save(summary_table, file = st_file)
  
  # close database connection
  db_close(conn)
  
  message("Finished.")
}


#' @noRd

pplr_summary_table_check = function(){
  
  wks_passed <- floor(as.numeric(difftime(Sys.time(),
                                          file.mtime(system.file("extdata", 
                                                                 "summary_table.rda",
                                                                 package = "popler")), 
                                          units=c("weeks"))))
  # if summary_table.rda does exist, but was created more than 6 weeeks ago,
  # prompt user to update the table.
  if(wks_passed >= 6){
    message("It's been ", wks_passed, " weeks since popler's summary table has been updated.\nWe recommend running 'pplr_summary_table_update()' to make sure your summary table is up to date with the latest database changes.")
  }
  
}

#' @noRd
#' @importFrom RPostgreSQL PostgreSQL dbConnect
# open a connection to the popler database
db_open <- function() {
    
    if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
      stop("RPostgreSQL package required to connect to postgres db", call. = FALSE)
    }
    
    inputs <- int.data$db
    
    
  
    con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), 
                                  host     = inputs$server, 
                                  dbname   = inputs$dbname, 
                                  user     = inputs$uid, 
                                  password = inputs$pwd, 
                                  port     = inputs$port)
    
    #info <- RPostgreSQL::dbGetInfo(con)
    #dbplyr::src_sql("postgres", con, info=info, disco=popler:::popler_disconnector(con,"postgres",silent))
    #src_sql("postgres", con, info=info, disco=popler:::popler_disconnector(con,"postgres",silent))
}
  
  
  

# a wrapper function to (quietly) close popler database connections

#' @importFrom RPostgreSQL dbDisconnect
#' @noRd 
db_close <- function(connection){
    # RPostgreSQL::dbDisconnect(connection$con,quiet=T)
  RPostgreSQL::dbDisconnect(connection,quiet=TRUE)
}

#' @noRd
# evaluate a string using the local environment, return the evaluation as string
string_eval_local <- function(x){
  paste0(deparse(eval(parse(text = paste0("local(",x,")")))),collapse="")
}

#' @noRd
# changes a column name from one name to another
colname_change = function(from, to, x){
  names(x) <- gsub(from, to, names(x))
  return(x)
}

# a function to pull sql queries and return dataframes

#' @importFrom dplyr %>% tbl
#' @importFrom dbplyr sql
#' @noRd

query_get = function(connection, query){
  # accepts a connection and a string query input
  # outputs a dataframe
  return(dplyr::tbl(connection, dbplyr::sql(query)) %>% data.frame())
}

# Source for idea
# https://stackoverflow.com/questions/30357330/r-cmd-check-no-visible-binding-for-global-variable-mypkgdata

#' @noRd
pplr_summary_table_import <- function() {
  # create empty environment for loading
  pkgEnv <- new.env(parent = emptyenv())
  
  # if this has not been called
  if(!exists('summary_table', pkgEnv)) {
    load(system.file("extdata", 
                     "summary_table.rda",
                     package = "popler"), envir = pkgEnv)
  }
  
  # allow assignment so that R CMD check find global binding
  summary_table <- pkgEnv[['summary_table']]
  
  return(summary_table)
}

#' @noRd
# gets DOI or, if not present, URL.
links_get = function( sum_tab_df ){
  
  links   <- sum_tab_df$doi
  no_doi  <- which( links == 'NA' )
  links   <- replace( links, 
                      no_doi, 
                      sum_tab_df$metalink[no_doi] )
  
  return(links)
  
}
