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
