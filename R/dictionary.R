#' Dictionary of the popler metadata
#'
#' Provides information on the metadata variables contained
#' in the popler database, and the kind of data contained in 
#' those variables.
#' 
#' @param ... A sequence of (unquoted) variables specifying one
#' or more variables of popler's main table for which dictionary 
#' information is needed
#' @param full_tbl Should the function return the standard columns,
#' or the full main table?
#' 
#' @export
#' @examples
#' \dontrun{
#' # Column names
#' column_names <- pplr_dictionary(full_tbl = FALSE)
#' 
#' # Dictionary information
#' dictionary_lter <- pplr_dictionary(lterid, full_tbl = FALSE)
#' 
#' # multiple columns
#' dictionary_lter_lat <- pplr_dictionary(lterid,lat_lter, full_tbl = FALSE)
#' }

pplr_dictionary <- function(..., full_tbl = FALSE){
  # summary table ------------------------------------------------------------
  # load summary table
  summary_table <- pplr_summary_table_import()
  # variables ------------------------------------------------

  # variables of which user defined wishes to know the content
  vars <- vars_dict(...)
  
  # produce output -------------------------------------------
  
  # if no column specified, return ALL column names
  if(is.null(vars)){
    # select data based on 
    tmp <- if(full_tbl){
      summary_table
    } else {
      
      # variables of default (full_tbl=FALSE) main table
      summary_table[ ,default_vars()]
    }
    out <- dictionary_explain(tmp)
  # if colums specified.
  } else {
    out <- dict_list(summary_table, vars)
  }
  
  return(out)
  
}

# lazy evaluation in dictionary

#' @importFrom lazyeval lazy_dots
#' @noRd

vars_dict <- function(...){
  
  eval_that <- lazyeval::lazy_dots(...)
  out <- sapply(eval_that, function(x) as.character(x$expr))
  
  if(length(out) > 0) {
    return(out)
  } else { 
    return(NULL) 
  }
  
}


# verify whether provided variables match one of the potential variables
#' @noRd
verify_vars <- function(sel_col){
  
  i <- which(sel_col %in% c(int.data$explanations$variable,
                            "structure", "treatment", "species") )
  
  if( length(i) < length(sel_col) ){
    
    unmatched <- setdiff(seq_len(length(sel_col)),i) 
    stop(paste0("variable '", sel_col[unmatched],
                "' does not match any of the variables contained in popler"))
    
  }  
  
}


unique_or_summary <- function(col) {
  if(is.numeric(col) | 
     is.integer(col)) {
    
    summary(col)
    
  } else {
    
    unique(col)
  }
}
# produce the lists of unique dictionary values
#' @importFrom stats setNames
#' @noRd

dict_list <- function(x, select_columns){
  
  # first, verify user input matches with variables contained in popler
  verify_vars(select_columns)
  
  # index "special" and "normal"
  i_spec <- which(select_columns %in% c("structure",
                                        "treatment",
                                        "species",
                                        "proj_metadata_key"))
  i_norm <- setdiff(c(1:length(select_columns)), i_spec)
  norm_cols <- select_columns[i_norm]
  
  # get unique values of "normal" variables -------------------------------------------
  if(length(norm_cols) > 1){
    out_norm <- lapply(x[ ,norm_cols, drop = FALSE], 
                       function(y) unique_or_summary(y))
  } else {
    out_norm <- lapply(x[ ,norm_cols, drop = FALSE],
                       function(y) unique_or_summary(y))
  }
  
  # get unique values of "special" variables ------------------------------------------
  out_spc <- list()
  
  if(any("species" == select_columns)){
    out_spc$species <- unique(x[ ,c("genus", "species")])
  }
  if(any("proj_metadata_key" == select_columns)) {
    out_spc$proj_metadata_key <- unique(x[ ,'proj_metadata_key'])
  }
  if( any("structure" == select_columns) ){
    # stash all structure data in a single vector
    str_vec <- unlist(c(x[ ,paste0("structured_type_", 1:4)]))
    out_spc$structure <- unique(str_vec)
  }
  if(any("treatment" == select_columns)){
    # stash all structure data in a single vector
    tr_vec <- unlist(c(x[ ,paste0("treatment_type_", 1:3)]))
    out_spc$treatment <- unique(tr_vec)
  } 
  
  # Variable descriptions ----------------------------------------------------------------
  # Special variables
  descr_spec  <- c("species (species name)",
                   "structure (types of indidivual structure)",
                   "treatment (type of treatment)",
                   "proj_metadata_key")
  if(length(out_spc) > 0){
    d_s_ind <- sapply(names(out_spc), function(x) grep(x, descr_spec))
    descr_spc <- descr_spec[d_s_ind]
  } else {
    descr_spc <- NULL
  }
  
  # Normal variables
  description <- int.data$explanations$description[match(names(out_norm),
                                      int.data$explanations$description)]
  
  descr_norm <- paste0(names(out_norm), " (", description,")" )
  
  # final descriptions
  names_out <- rep(NA, length(select_columns))
  names_out[i_norm] <- descr_norm
  names_out[i_spec] <- descr_spc
  
  
  # description of output -----------------------------------------------------------------
  out <- rep(list(NULL), length(select_columns))
  out[i_norm] <- out_norm
  out[i_spec] <- out_spc
  out <- setNames(out, names_out)
  
  # remove NAs or "NA"
  out <- lapply(out, function(x) x <- x[!is.na(x)])
  out <- lapply(out, function(x) x <- x[x != "NA"])
  
  return(out) 
  
}

#' @noRd
# explain meaning of dictionary variables 
dictionary_explain <- function(x){
  
  if(ncol(x) < 60){
    out <- int.data$explain_short
  } else {
    out <- int.data$explanations
  }
  
  return(out)
  
}
