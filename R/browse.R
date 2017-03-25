#' Browse the project metadata contained in the popler database
#'
#' browse() reports the metadata of LTER studies contained in the popler database. 
#' The user can subset what data and what columns to visualize.  
#' @param ... A logical expression to subset popler's main table
#' @param full_tbl Should the function return the standard columns, or the full main table?
#' @param vars A vector of characters: which variables of popler's main table should be selected?
#' @param trim If TRUE, strings are truncated at the 50th character. Default is TRUE.
#' @param view If TRUE, opens up a spreadsheet-style data viewer.
#' @param keyword A string that selects 
#' @return A data frame combining the metadata of each project and the taxonomic units associated with each project.
#' @return This data frame is of class "popler", "data.frame", "tbl_df", and "tbl".  
#' @export
#' @examples
#' # No arguments return the standard 16 columns of popler's main table
#' default_vars = browse()
#' 
#' # full.table==T returns the full table
#' all_vars = browse(full_tbl = TRUE)
#' 
#' # subset only data from the sevilleta LTER 
#' sev_data = browse(lterid == "SEV")
#' 
#' # consider only plant data sets 
#' plant_data = browse(kingdom == "Plantae")
#' 
#' # Select only the data you need
#' three_columns = browse(vars = c("title","proj_metadata_key","genus","species"))
#' 
#' # Select only the data you need
#' study_21 = browse( proj_metadata_key == 25)
#' 
#' # Select studies that contain word "parasite"
#' parasite_studies = browse( keyword = "parasite")


# The browse popler function
browse <- function(..., full_tbl = FALSE, vars = NULL, trim = TRUE, view = FALSE, keyword = NULL){
  
  # stop if user supplies both criteria and a keyword
  if( !is.null(substitute(...)) & !is.null(keyword)){
    stop("
         browse() cannot simultaneously subset based on both a logical statement and the 'keyword' argument.
         Please use only one of the two methods, or refine your search using get_data().")
  }
  
  # error message if variable names are incorrect
  vars_spell( vars, c(names(summary_table),"taxonomy"), possible_vars() )
  
  # update user query to account for actual database variable names
  logic_expr   <- call_update(substitute(...))

  
  # subset rows: if keyword is not NULL ---------------------------
  if( !is.null(keyword) ){ 
    
    keyword_data   <- keyword_subset(summary_table, keyword)
    subset_data    <- keyword_data$tab
    keyword_expr   <- keyword_data$s_arg
  
  # subset rows: if logic_expr is not NULL
  } else {  
    
    subset_data   <- select_by_criteria(summary_table, logic_expr)
    keyword_expr  <- NULL 
    
  }
  
  
  # select variables (columns) -------------------------------------
  if( is.null(vars) ){ # if variables not declared explicitly 
    
    # select columns based on whether or not the full table should be returned
    out_vars <- if(full_tbl==T){subset_data} else {subset_data[,possible_vars()]}
  
  } else { # if variables are declared explicitly
      
    # select cols based on vars, including 'proj_metadata_key' if not in vars
    out_vars <- subset_data[,vars_check(vars)]
  }
  
  
  # collapse taxonomic information for each project into a list 
  nested_data  <- nest_taxa(out_vars, full_tbl)
  
  # trim output
  out_form <- trim_display(nested_data, trim)
  
  # write output
  if(view == TRUE) View(out_form)
  
  # attribute class "popler"
  out            <- structure(out_form, 
                              class = c("popler", class(out_form) ),
                              search_argument = c(logic_expr,keyword_expr)[[1]]
                              )
  
  return(out)
  
}



#' @noRd
# implements the 'keyword' argument ANd operator in browse() 
keyword_subset <- function(x, keyword){

  #function: index of keywords
  i_keyw <- function(x,keyword) {
    ind <- which( grepl(keyword,x,ignore.case = T) )
    return(ind)
  }
  
  # row numbers selected
  ind_list  <- lapply(x, i_keyw, keyword)
  proj_i    <- unique( unlist(ind_list) )
  
  # projects selected
  if(length(proj_i) > 0){
    proj_n      <- unique( x$proj_metadata_key[proj_i] )
    statements  <- paste0("proj_metadata_key == ", proj_n)
    src_arg     <- parse(text=paste0(statements, collapse = " | "))[[1]]
  } else { 
    src_arg     <- NULL
  }
  
  # return values
  out <- list(tab=x[proj_i,],s_arg=src_arg) 
  return(out)
  
}


# function to subset dataframe by criteria and do error checking
select_by_criteria <- function(x,criteria){
  
  if(!is.null(criteria)) {
    # if criteria are specified, subset the dataframe accordingly
    out <- subset(x,eval(criteria))
    
  } else { 
    # if no criteria are specified, do nothing
    out <- x
  }
  
  # if no results are returned, return an error
  if( nrow(out) == 0 ) {
    stop( "No matches found. Either:
          1. the name of variable(s) you specified is/are incorrect or 
          2. the values you are looking for are not contained in the variable(s) you specified")
  }
  
  return(tbl_df(out))
}


# Store possible variables
possible_vars = function(){ 
  return(c("title","proj_metadata_key","lterid",
           "datatype","studytype",
           "duration_years", "community", "studystartyr", "studyendyr",
           "structured_type_1","structured_type_2","structured_type_3","structured_type_4",
           "treatment_type_1","treatment_type_2","treatment_type_3",
           "lat_lter","lng_lter",
           "species","kingdom","phylum","class","order","family","genus"))
           #"taxonomy")
}


# check that at least proj_metadata_key is included in variables
vars_check <- function(x){
  
  if( !"proj_metadata_key" %in% x ) x = c("proj_metadata_key",x)
  return(x)
  
}

# Error for misspelled columns in full table
vars_spell <- function(select_columns,columns_full_tab,possibleargs){
  
  #Check for spelling mistakes
  if( !all( is.element(select_columns,columns_full_tab) ) ) {
    opt <- options(error=NULL)
    on.exit(opt)
    stop(paste0("Error: the following 'argument' entry was misspelled: '",
                setdiff(select_columns,possibleargs),"' "))
  }
  
}

# expand table (to nest/unnest taxonomic info) 
nest_taxa <- function(x, full_tbl){
  
  # select taxonomic information (based on full_ or standard_table)
  if( full_tbl == FALSE){
    taxas <- c("species","kingdom","phylum","class","order","family","genus")
  } else {
    taxas <- c('sppcode','kingdom','subkingdom','infrakingdom',
               'superdivision','division','subdivision',
               'superphylum','phylum','subphylum','class',
               'subclass','order','family','genus','species',
               'common_name','authority')
  }
  
  # nest data set
  out  <- x %>% 
    group_by_(.dots = setdiff(names(x),taxas) ) %>%
    nest(.key = taxas)
  # Names of taxonomic lists
  names(out$taxas)  <- paste0("taxa_project_#_",out$proj_metadata_key)

  return(out)
  
}


# trim the display of character values. Mostly for project "titles"
trim_display=function(x, trim){
  
  if(trim==T){
    tmp=as.data.frame(x)
    for(i in 1:ncol(tmp)){
      if(is.character(tmp[,i])){ tmp[,i]=strtrim(tmp[,i],25) }
    }
    tmp=as.tbl(tmp)
    return(tmp)
  } else{
    return(x)
  }
}



# Calculate tallies
tallies=function(browsed_data,tally_columns,group_factors,trim){
  
  df_list=list()
  for(i in 1:length(tally_columns)){
    
    #does 'tally_columns' refers to multiple columns
    multi_tally=multiple_columns(tally_columns[i])
    
    #columns referring to both group_factors and tallies
    group_tally_cols=c(group_factors,multi_tally)
    
    #Data of interest for the tally
    tally_data=select(browsed_data,one_of(group_tally_cols))
    
    #lower case of grouping factors (should get rid of doubles such as "yes" "Yes")
    tally_data=as.data.frame(tally_data)
    for(co in 1:ncol(tally_data)) { tally_data[,co]=tolower(tally_data[,co])}
    tally_data=as.tbl(tally_data)
    
    #only unique values
    tally_data=distinct(tally_data)
    
    #store tallies
    tally_name=paste0(tally_columns[i],"_count") #name of column containing tallies
    #grouping factor present
    if(!is.null(group_factors)){ 
      df_list[[i]] <- tally_data %>%
        group_by_(.dots=group_factors) %>%
        summarise_(.dots=setNames(list(~n()), tally_name))
      #No grouping factor
    } else {
      df_list[[i]] <- tally_data %>% summarise_(.dots=setNames(list(~n()),tally_name))
    }
    
  }
  out=trim_display(Reduce(function(...) merge(...),df_list),trim)
  return(out)
  
}


# function converts entries to multiple columns - if need be
multiple_columns=function(x) {
  
  taxonomy            <-  c("kingdom","phylum","clss","ordr","family","genus","species")
  species             <-  c("genus","species")
  spatialLevels       <-  c("sp_rep1_ext","sp_rep2_ext","sp_rep3_ext","sp_rep4_ext")
  wrapperNames        <-  c("taxonomy","species","spatialLevels") #list of multiple columns entries (the above threee)
  
  if( any(x %in% wrapperNames) ){
    oldIds   <- which(x %in% wrapperNames)
    newIds   <- which(wrapperNames %in% x)
    eval(parse(n=1,text=paste0("newArg=",wrapperNames[newIds])))
    columnNames=c(x[-oldIds],newArg)
    return(columnNames)
  } else(return(x))
}


##### UNUSED FUNCTIONS #########################################################