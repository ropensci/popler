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
#' study_21 = browse( proj_metadata_key == 21)
#' 
#' # More straighforward using keyword operator, %=%
#' parasite_npp = browse( title %=% "parasit", trim = FALSE)


# The browse popler function
browse <- function(..., full_tbl = FALSE, vars = NULL, trim = TRUE, view = FALSE, keyword = NULL){
  
  # stop if user supplies both criteria and a keyword
  if( !is.null(substitute(...)) & !is.null(keyword)){
    stop("
         browse() cannot simultaneously subset based on both a logical statement and the 'keyword' argument.
         Please use only one of the two methods, or refine your search using get_data().")
  }
  
  # error message if column names are incorrect
  err_full_tab( vars, names(main_table()), possible_args() )
  
  # Select by subset 
  sbst_popler   <- call_update(substitute(...))
  key_subset    <- key_arg(main_table(), keyword, sbst_popler) # if keyword argument/%=% != NULL 
  subset_data   <- select_by_criteria(key_subset$tab, sbst_popler)
  
  # select data based on whether or not the full table should be returned
  subset_data <- if(full_tbl){subset_data} else {subset_data[,possible_args()]}
  
  # If no column specified, return all columns
  if( is.null(vars) ){
    out_cols <- subset_data
  } else{
    # include proj_metadata_key if in vars
    vars      <- vars_check(vars)
    # If not, select said columns
    out_cols  <- subset_data[,vars]
  }
  
  out_form <- elastic_tab(out_cols, full_tbl)
  out_form <- trim_display(out_form, trim)
  
  
  # write output
  if(view == TRUE) View(out_form)
  
  # attribute class "popler"
  out            <- structure(out_form, 
                              class = c("popler", class(out_form) ),
                              search_argument = c(sbst_popler,key_subset$s_arg)[[1]]
  )
  
  return(out)
  
}



#' @noRd
# Converts factor columns into character format
factor_to_character <- function(x, full_tbl = FALSE){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)
  
}

main_table = function(){

  # Data table; convert factors to characters
  main_t       <- factor_to_character(main_popler)

  # Case insensitive matching ("lower" everything)
  names(main_t) <- tolower( names(main_t) )
  
  # convert columns "ordr" to "order" and "clss" to "class"
  main_t        <- colname_change("clss", "class", main_t)
  main_t        <- colname_change("ordr", "order", main_t)
  
  return(main_t)
}

# changes a column name from one name to another
colname_change = function(from, to, x){
  names(x) <- gsub(from,to,names(x))
  return(x)
}

# implements the 'keyword' argument ANd operator in browse() 
key_arg <- function(x,keyword,criteria){
  
  # if only keyword is used --------------------------------------------------
  if( !is.null(keyword) & is.null(criteria) ){
    
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
  
  # if only criteria is used --------------------------------------------------
  if( is.null(keyword) & !is.null(criteria) ){
    
    # if is %=% is used
    if( any(grepl("%=%",deparse(criteria))) ) {
      
      # you cannot use %=% and other operators simultaneously
      if( any(grepl("==|!=|>|<|<=|>=",deparse(criteria))) ){
        
        stop("
             
             You currently cannot use `%=%` and R's standard operators (<,>,<=,>=,==,!=)
             simultaneously. We are currently working to improve this - sorry about that.
             
             ")
        
      } else{
        
        # convert 
        proj_i     <- which(eval(criteria, x, parent.frame()))
        
        # get project IDs
        if( length(proj_i) != 0 ) {
          proj_n <- unique( x[proj_i,,drop=F]$proj_metadata_key )
          statements  <- paste0("proj_metadata_key == ", proj_n)
          src_arg     <- parse(text=paste0(statements, collapse = " | "))[[1]]
        } else { src_arg <- NULL }
        
        # return values
        sbst_popler <<- src_arg # change the subset statement in parent environment
        return(list(tab=x))
      }
      
      # if neither keyword, nor %=% are used, return data frame as is
    } else { 
      return(list(tab=x))
    }
    
  }
  
  if( is.null(keyword) & is.null(criteria) ){ return(list(tab=x)) }
  
}


# Summarizing function
select_by_criteria <- function(x,criteria){
  
  if(!is.null(criteria)) {
    r <- which(eval(criteria, x, parent.frame()))
    if( length(r) != 0 ) {
      subsetDat <- tbl_df(x[r,,drop=F]) #tbl_df() to make object "work" with dplyr functions
    } 
    if( length(r) == 0 ) {
      stop( "No matches found. Either:
            1. the name of variable(s) you specified is/are incorrect or 
            2. the values you are looking for are not contained in the variable(s) you specified")
    }
  } else { 
    subsetDat <- tbl_df(x) 
  }
  return(subsetDat)
  
}


# Store possible arguments
possible_args = function(){ 
  return(c("title","proj_metadata_key","lterid",
           "datatype","studytype",
           "duration_years", "community",
           "structured_type_1","structured_type_2","structured_type_3","structured_type_4",
           "treatment_type_1","treatment_type_2","treatment_type_3",
           "lat_lter","lng_lter",
           "species","kingdom","phylum","class","order","family","genus"))
}


# check that at least proj_metadata_key is included in variables
vars_check <- function(x){
  
  if( !"proj_metadata_key" %in% x ) x = c("proj_metadata_key",x)
  return(x)
  
}

# Error for misspelled columns in full table
err_full_tab <- function(select_columns,columns_full_tab,possibleargs){
  
  #Check for spelling mistakes
  if( !all( is.element(select_columns,columns_full_tab) ) ) {
    opt <- options(error=NULL)
    on.exit(opt)
    stop(paste0("Error: the following 'argument' entry was misspelled: '",
                setdiff(select_columns,possibleargs),"' "))
  }
  
}


# expand table (to nest/unnest taxonomic info) 
elastic_tab <- function(x, full_tbl){
  
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
  
  # nest(shrink)/Unnest(expand) data set
  out  <- x %>% 
    group_by_(.dots = setdiff(names(x),taxas) ) %>%
    nest(.key = taxonomy)
  # Names of taxonomic lists
  names(out$taxonomy)  <- paste0("taxa_project_#_",out$proj_metadata_key)

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
    multi_tally=popler:::multiple_columns(tally_columns[i])
    
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
  out=popler:::trim_display(Reduce(function(...) merge(...),df_list),trim)
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
      query_list[i] <- paste(gsub("@", LHS_RHS[i,2], 
                                  gsub("@@", comps[i], 
                                       gsub("@@@", op, r_trt))),
                             logic[i])
      
    } else if(LHS_RHS[i,1] == "structure"){
      
      # update "structure" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", LHS_RHS[i,2], 
                                  gsub("@@", comps[i], 
                                       gsub("@@@", op, r_str))),
                             logic[i])
      
    } else{
      
      # for any other LHS, just put the query back together
      query_list[i] = paste(LHS_RHS[i,1], comps[i], LHS_RHS[i,2], logic[i])
      
    }
  }
  
  # collapse query_list to a single string, convert to expression, and return it
  return(eval(parse(text=
                      paste0(unlist(query_list),    collapse="") %>%
                      paste0("substitute(", . ,")", collapse="")))
  )
}


##### UNUSED FUNCTIONS #########################################################
# returns a full table or not
table_select <- function(x, full_tbl = FALSE, possible_args){
  
  if(full_tbl == FALSE) return(x[,possible_args])
  if(full_tbl == TRUE)  return(x)
  
}


# changes clss to class and ordr to order
class_order_names <- function(x){
  
  names(x)   <- gsub("clss","class",names(x))
  names(x)   <- gsub("ordr","order",names(x))
  
  return(x)
  
}