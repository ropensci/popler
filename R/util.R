# function updates "treatment" and "structure" in a user's query to match the
# appropriate columns in the database.
call_update = function(query){
  
  # query is some user's input (i.e. a query) to the browse() function
  
  # if the query is null, don't do anything; just return the query
  if(is.null(query)){ return(query) }
  
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
  query_str <- gsub(" ", "",paste0(deparse(query),collapse=""))
  
  # split query_str on logical operators to get individual arguments
  query_arg <- unlist(strsplit(query_str,f_logic))
  
  # make a matrix where col1 is the LHS of each query_arg and col2 is the RHS
  LHS_RHS <- matrix(unlist(strsplit(query_arg,f_compn)),ncol=2,byrow=T)
  
  # find logical operators in the query by searching query_str
  logic <- c(str_extract_all(query_str,f_logic)[[1]],"")
  
  # find comparison operators in each argument by searching query_arg
  comps <- unlist(str_extract_all(query_arg,f_compn))
  
  # make a list where each element is an argument in the query, updating LHS of
  # an argument when necessary
  query_list = list()
  for(i in 1:nrow(LHS_RHS)){
    
    # determine operator in case treatment or structure strings need to change
    op <- ifelse(comps[i]=="!=", "&", "|")
    
    if(LHS_RHS[i,1] == "treatment"){
      
      # update "treatment" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i,2]), 
                                  gsub("@@", comps[i], 
                                       gsub("@@@", op, r_trt))),
                             logic[i])
      
    } else if(LHS_RHS[i,1] == "structure"){
      
      # update "structure" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i,2]),
                                  gsub("@@", comps[i],
                                       gsub("@@@", op, r_str))),
                             logic[i])
      
    } else{
      
      # for any other LHS, just put the query back together
      query_list[i] = paste(LHS_RHS[i,1], comps[i], string_eval_local(LHS_RHS[i,2]), logic[i])
      
    }
  }
  
  # collapse query_list to a single string, convert to expression, and return it
  return(eval(parse(text=
                      paste0(unlist(query_list),    collapse="") %>%
                      paste0("substitute(", . ,")", collapse="")))
  )
}

# given a browse() object or a get_data() object, returns an identical browse
# object with full_tbl=T and trim=F
rebrowse = function(input){
  
  # determine whether there's a get_data() object or a browse() object
  
  # if it's not a browse object...
  if(is.null(input$proj_metadata_key)){
    
    # and if it's not a get_data object
    if(is.null(attributes(input)$unique_projects)){
      
      stop("Input must be a browse() object or a get_data() object.")
      
    } else {
      
      # get unique proj_metadata_keys from get_data object
      pmk <- paste0(attributes(input)$unique_projects,collapse=",")
    }
    
  }  else {
    
    #get unique proj_metadata_keys from browse object
    pmk <- paste0(input$proj_metadata_key,collapse=",")
  }
  
  # re-run browse to get full table and untrimmed output
  return(eval(parse(text=paste0("browse(proj_metadata_key %in% c(", pmk,"), full_tbl=T, trim=F)"))))
}

# generate main data table summary ---------------------------------------------
summary_table_update = function(){
  
  message("Please wait while popler updates its summary table... this may take several minutes.")
  
  # set database connection
  conn <- db_open()
  
  # list all columns
  proj_cols   <- query_get(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'project_table'")[,1]
  lter_cols   <- query_get(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'lter_table'")[,1]
  taxa_cols   <- query_get(conn, "SELECT column_name FROM information_schema.columns WHERE table_name = 'taxa_table'")[,1]
  search_cols <- paste( c(proj_cols,lter_cols,taxa_cols), collapse = ", ")
  
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
  out_proj$tot_spat_rep <- apply(out_proj[sr_colnames], 1, prod, na.rm=T)
  
  # add column for number of spatial levels 
  out_proj$n_spat_levs  <- apply(!is.na(out_proj[sr_colnames]), 1, sum) 
  
  # reorder out_proj columns
  out_proj  <- out_proj[c(1:11,46:47,12:36,58:59,37:45,48:57)]
  
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
  save(summary_table, file=st_file)
  
  # close database connection
  db_close(conn)
  
  message("Finished.")
}

# check if the main table needs to be updated
summary_table_check = function(){
  # if summary_table.rda does not exist, add it
  if(system.file("extdata", "summary_table.rda", package = "popler")==""){
    summary_table_update()
  } else {
    
    wks_passed <- floor(as.numeric(difftime(Sys.time(),
                                            file.mtime(system.file("extdata", "summary_table.rda", package = "popler")), 
                                            units=c("weeks"))))
    # if summary_table.rda does exist, but was created more than 6 weeeks ago,
    # prompt user to update the table.
    if(wks_passed >= 6){
      message("It's been ", wks_passed, " weeks since popler's summary table has been updated.\nWe recommend running 'summary_table_update()' to make sure your summary table is up to date with the latest database changes.")
    }
  }
}

# open a connection to the popler database
db_open = function(){
  popler_connector(dbname="popler_3", 
                    host="ec2-54-214-212-101.us-west-2.compute.amazonaws.com",
                    port=5432,
                    user="other_user",
                    password="bigdata",
                    silent=TRUE)
}

# a wrapper function to (quietly) close popler database connections
db_close = function(connection){
    RPostgreSQL::dbDisconnect(connection$con,quiet=T)
}

# evaluate a string using the local environment, return the evaluation as string
string_eval_local = function(x){
  paste0(deparse(eval(parse(text=paste0("local(",x,")")))),collapse="")
}

# changes a column name from one name to another
colname_change = function(from, to, x){
  names(x) <- gsub(from,to,names(x))
  return(x)
}

# a function to pull sql queries and return dataframes
query_get = function(connection, query){
  # accepts a connection and a string query input
  # outputs a dataframe
  return(tbl(connection,sql(query)) %>% data.frame())
}

#' @noRd
## these functions should be masked from the user but available in popler

# a (very slightly) modified version of dplyr::src_postgres() to connect to the
# popler database and enable a silent disconnect
popler_connector = function (dbname=NULL, host=NULL, port=NULL, user=NULL, password=NULL, silent=TRUE) {
  
  if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
    stop("RPostgreSQL package required to connect to postgres db", call. = FALSE)
  }
  user <- if(is.null(user)){
    if(identical(Sys.getenv("TRAVIS"), "true")){"postgres"} else {""} 
    } else user
  con <- dbConnect(RPostgreSQL::PostgreSQL(), 
                   host     = if(is.null(host))     "" else host, 
                   dbname   = if(is.null(dbname))   "" else dbname, 
                   user     = user, 
                   password = if(is.null(password)) "" else password, 
                   port     = if(is.null(port))     "" else port)
  info <- dbGetInfo(con)
  src_sql("postgres", con, info=info, disco=popler_disconnector(con,"postgres",silent))
}

# a (very slightly) modified version of dplyr::db_disconnector() to enable
# silent disconnect
popler_disconnector = function (con, name, silent = TRUE) 
{
  reg.finalizer(environment(), function(...) {
    if (!silent) {
      message("Auto-disconnecting ", name, " connection to popler database ", 
              "(", paste(con@Id, collapse = ", "), ")")
    }
    dbDisconnect(con)
  })
  environment()
}

# Converts factor columns into character format
factor_to_character <- function(x, full_tbl = FALSE){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)
  
}