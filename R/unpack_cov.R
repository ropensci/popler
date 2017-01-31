# function to unpack covariates
unpack_cov <- function(x){
  
  spl_flds <- lapply(lapply(x$covariates, strsplit, ","), unlist)
  no_brack <- lapply(lapply(spl_flds, strsplit, "\\{|\\}"), unlist)
  keep_dat <- lapply(no_brack, function(x) return(x[grep(":",x)]))
  fields   <- lapply(lapply(keep_dat, strsplit, ":"),unlist)
  
  # test: whether 
  n_elem   <- sapply(fields, length)
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
    stop("Error: cannot unpack covariates.")
  }
  
}
