
# implements the 'keyword' argument in browse() 
new_key_arg <- function(x,keyword,criteria){
  
  # if both keyword and criteria are used ----------------------------------------------
  if( !is.null(keyword) & !is.null(criteria) ){
    stop("
         
         browse() cannot simultaneously subset based on both 
         a logical statement and the 'keyword' argument
         
         Pick one of the two methods, or... 
         refine your search using get_data()
         
         ")
  }
  
  # if only keyword is used --------------------------------------------------
  if( !is.null(keyword) & is.null(criteria) ){
      
    #function: index of keywords
    i_keyw <- function(x,keyword) {
      ind <- which( grepl(keyword,x,ignore.case = T) )
      return(ind)
    }
    
    # row numbers selected
    ind_list    <- lapply(x, i_keyw, keyword)
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
             
             You cannot use `%=%` and R's standard operators (<,>,<=,>=,==,!=)
             simulataneously. Sorry about that.

             ")
        
      } else{
      
        # convert 
        updtd_crit <- popler:::update_call( criteria )
        proj_i     <- which(eval(updtd_crit, x, parent.frame()))
        
        # get project IDs
        if( length(r) != 0 ) {
          proj_n <- unique( x[proj_i,,drop=F]$proj_metadata_key )
          statements  <- paste0("proj_metadata_key == ", proj_n)
          src_arg     <- parse(text=paste0(statements, collapse = " | "))[[1]]
        } else { src_arg <- NULL }
      
        # return values
        out         <- list(tab=x[proj_i,],s_arg=src_arg) 
   
      }
      
    # if neither keyword, nor %=% are used, return data frame as is
    } else { 
      return(list(tab=x,s_arg=NULL))
    }
  
  }
  
}


s_arg <- function(x){
  
  any(grepl("%=%",deparse(criteria)))
  
}