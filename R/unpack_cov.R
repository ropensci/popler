#' Unpack the covariates for a data set
#'
#' Obtain author name and email from a data object downloaded from popler.
#' @param An object produced by the function get_data(). NOTE: temporarily, data can come from 1 study only. 
#' @return A data frame whose columns represent the covariates of the data set.
#' @export
#' @examples
#' 
#' # get author names and email from studies containing data from the genus Poa
#' demo_d <- get_data(proj_metadata_key == 8)
#' as.tbl( unpack_cov( demo_d ) )


# function to unpack covariates
unpack_cov <- function(x){
  
  spl_flds <- lapply(lapply(x$covariates, strsplit, ","), unlist)
  no_brack <- lapply(lapply(spl_flds, strsplit, "\\{|\\}"), unlist)
  keep_dat <- lapply(no_brack, function(x) return(x[grep(":",x)]))
  fields   <- lapply(lapply(keep_dat, strsplit, ":"),unlist)
  
  # test: whether 
  n_elem   <- sapply(fields, length)
  # identify error
  err_id   <- which(unlist(lapply(n_elem, `%%`, 2)) == 1)
  n_cols   <- unique(lapply(n_elem, `%%`, 2))
  if(length(n_cols) == 1 & n_cols == 0){
  
    keep_even <- grep(0,c(1:unique(n_elem)) %% 2)
    keep_nams <- grep(1,c(1:unique(n_elem)) %% 2)
    
    kept_fields <- lapply(fields, function(x) return(x[keep_even]))
    kept_nams   <- lapply(fields, function(x) return(x[keep_nams]))
    field_nams  <- unique(kept_nams)
    
    field_mat   <- as.data.frame( do.call(rbind,kept_fields) )
    
    if( length(field_nams) > 1 ) stop("Error: cannot unpack covariates.")
    
    if( length(field_nams) == 1 ){
      
      # "clean" names
      
      field_nams  <- unlist( strsplit(unlist(field_nams), "'") )
      field_nams  <- field_nams[-grep(" |  |$^",field_nams)]
      
      field_mat <- setNames(field_mat, field_nams )
      return(field_mat)
    }  
    
  } else {
    stop("Error: cannot unpack covariates.
          Error occured at line(s) ", paste0(err_id,collapse=",") )
  }
  
}
