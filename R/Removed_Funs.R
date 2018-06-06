# Functions/methods removed from prior development versions

#' Get author information from a data object
#'
#' Obtain author name and email from a data object downloaded from popler.
#' @param data_object An object produced by the function get_data().
#' @return A data frame with project IDs, names of the authors of each data set, and their email.
#' @export
#' @importFrom dplyr as.tbl %>%
#' @examples
#' 
#' \dontrun{
#' # get author names and email from studies containing data from the genus Poa
#' poa_data = get_data(subset = genus == "Poa")
#' authors( poa_data )
#' }
# function definition
authors <- function(data_object){
  out <- attributes(data_object)$unique_authors %>%  as.tbl
  return(out)
}



#' Count the number of observations in one or more of the popler database variables 
#'
#' summary_popler() counts the number of observations (e.g. number of species)
#'  contained in one or more of the popler database variables.
#' "Counts" refer to the number of unique occurrences in each field.  
#' Observations can be counted one by one, or by grouping them 
#' based on other variables (e.g. number of species, grouped by study) 
#' 
#' @param group_vars Variables used to group observation counts.
#' @param count_vars Names of variables whose observations will be counted.
#' If no group variables defined, the function counts unique fields 
#' contained in the specified variable(s)
#' @param trim If TRUE, strings are truncated at the 25th character.
#' 
#' @return An object of class "tbl_df", "tbl", and "data.frame". 
#' 
#' @export
#' @examples
#' \dontrun{
#' # Tallies without grouping factor
#' pplr_summary(count_vars = "title", trim = TRUE)
#'          
#' # Number of species by study
#' pplr_summary(group_vars = "title", 
#'                count_vars = "species", trim = TRUE)
#'          
#' # Number of studies by LTER site
#' pplr_summary(group_vars = "lterid", 
#'                count_vars = "title", trim = TRUE)
#' }       
# The summary_popler function
pplr_summary <- function(group_vars = NULL, count_vars = "title", trim = TRUE){
  
  summary_table <- summary_table_import()
  # tally cases, if tally_by is not NULL
  out <- tallies(summary_table, count_vars, group_vars, trim)
  
  return(out) # return output
  
}

# Calculate tallies
#' @importFrom dplyr select one_of as.tbl group_by_ summarise_ n %>% distinct
#' @importFrom stats setNames
#' @noRd
tallies <- function(browsed_data, tally_columns, group_factors, trim) {
  
  df_list <- list()
  for(i in seq_len(length(tally_columns))) {
    
    #does 'tally_columns' refers to multiple columns
    multi_tally <- multiple_columns(tally_columns[i])
    
    #columns referring to both group_factors and tallies
    group_tally_cols <- c(group_factors, multi_tally)
    
    #Data of interest for the tally
    tally_data <- dplyr::select(browsed_data,
                                dplyr::one_of(group_tally_cols))
    
    #lower case of grouping factors (should get rid of doubles such as "yes" "Yes")
    tally_data <- as.data.frame(tally_data)
    for(co in seq_len(ncol(tally_data))) { 
      tally_data[ ,co] <- tolower(tally_data[ ,co])
    }
    tally_data <- dplyr::as.tbl(tally_data)
    
    #only unique values
    tally_data <- dplyr::distinct(tally_data)
    
    #store tallies
    tally_name <- paste0(tally_columns[i], "_count") #name of column containing tallies
    #grouping factor present
    if(!is.null(group_factors)){ 
      df_list[[i]] <- tally_data %>%
        group_by_(.dots = !!!group_factors) %>%
        summarise_(.dots = setNames(list(~n()),
                                    tally_name))
      #No grouping factor
    } else {
      df_list[[i]] <- tally_data %>% 
        summarise_(.dots=setNames(list(~n()),
                                  tally_name))
    }
    
  }
  out <- trim_display(Reduce(function(...) merge(...), df_list), trim)
  return(out)
  
}


# function converts entries to multiple columns - if need be
#' @noRd
multiple_columns <- function(x) {
  
  taxonomy            <-  c("kingdom","phylum","clss","ordr",
                            "family","genus","species")
  species             <-  c("genus","species")
  spatialLevels       <-  c("sp_rep1_ext","sp_rep2_ext",
                            "sp_rep3_ext","sp_rep4_ext")
  wrapperNames        <-  c("taxonomy","species","spatialLevels") #list of multiple columns entries (the above threee)
  
  if( any(x %in% wrapperNames) ){
    oldIds   <- which(x %in% wrapperNames)
    newIds   <- which(wrapperNames %in% x)
    newArg <- eval(parse(n = 1, text = paste0(wrapperNames[newIds])))
    columnNames <- c(x[-oldIds], newArg)
    return(columnNames)
  } else {
    return(x)
  }
}

# summary method for popler objects
summary.popler <- function(x) {
  out <- length(unique(x$title))
  names(out) <- "number of projects"
  return(out)
}



#' Print popler study names in a legible form.
#'
#' Prints the names of popler studies as paragraphs of known width.
#' This is a wrapper of the strwrap function in base. 
#' 
#' @param browsed An object created by the function browse()
#' @param width A positive integer giving the target column 
#' for wrapping lines in the output (from base function strwrap)
#'  
#' @export
#' @examples
#' \dontrun{
#' # Look up names
#' #tmp = browse()
#' #study_names(tmp)
#' vcr_dat = browse(lterid == "VCR")
#' study_names(vcr_dat)
#' #study_names(vcr_dat, width = 30)
#' }

study_names <- function(browsed, width = 60) {
  
  summary_table <- summary_table_import()
  # re-browse data, with trim OFF
  if( is.null(attributes(browsed)$search_expr) ){
    new_browsed <- browse(trim = FALSE)
  } else {
    call        <- attributes(browsed)$search_expr
    new_browsed <- subset(summary_table, eval(call) )
  }
  
  # extract titles from object
  titles  <- unique( new_browsed$title )
  # Store 'wrapped' titles in a list 
  title_l <- lapply(titles, strwrap, width = width)
  
  for(i in 1:length(title_l)){
    
    title_l[[i]] <- c(title_l[[i]],"\n")
    writeLines(title_l[[i]])
    
  }

}




# a (very slightly) modified version of dplyr::db_disconnector() to enable
# silent disconnect

#' @importFrom RPostgreSQL dbDisconnect
popler_disconnector = function (con, name, silent = TRUE) 
{
  reg.finalizer(environment(), function(...) {
    if (!silent) {
      message("Auto-disconnecting ", name, " connection to popler database ", 
              "(", paste(con@Id, collapse = ", "), ")")
    }
    RPostgreSQL::dbDisconnect(con)
  })
  environment()
}

# Converts factor columns into character format
factor_to_character <- function(x, full_tbl = FALSE){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[ ,i] <- as.character(x[ ,i])
  }
  return(x)
  
}

