foo <- function(...){
  
  # calls
  call_list <- lazyeval::lazy_dots(...)
  # sql translations
  subset_inherit <- subset_get_dat <- NULL
  
  # if user provides 3 objects
  if( length(call_list) > 2 ){ 
    stop("Error: you cannot enter more than 2 arguments in the '...' field.\n  After the second comma, please refer to the arguments described in the documentation.")
  }
  
  # if user provides 2 objects
  if( length(call_list) == 2 ){
    
    # test what class expressions belong to   
    arg_class_vec <- c(class( call_list[[1]]$expr ),class( call_list[[2]]$expr ))
                       
    if( all( c("name", "call") %in% arg_class_vec)){
      
      # expression "name" should be the "popler" object
      i = which(arg_class_vec %in% "name") 
      if(is.element("popler", class(eval(call_list[[i]]$expr,call_list[[i]]$env)))){
        
        # logical statement from popler object
        subset_inherit <- parse_to_sql_search( 
                          attributes(eval(call_list[[i]]$expr,call_list[[i]]$env))$search_argument )
        # Directly provided logical statement
        i2 = setdiff(c(1,2),i)
        subset_get_dat <- parse_to_sql_search( call_list[[i2]]$expr )
        
      }else{ #the nonlogical argument MUST be a popler object
        stop("Error: The object you provide must be produced by browse()")
      }
      
    }else{
      if( all( c("name", "name") %in% arg_class_vec) ){
        stop("Error: You apparently provided two separate objects. \n  If you provide 2 inputs in the '...' field, these necessarily need to be \n1.  An object produced by browse() and \n2.  A logical statement.")
      }else{ 
        if( all( c("call", "call") %in% arg_class_vec)){
          stop("Error: You apparently provided two separate logical statements. \n  If you provide 2 inputs in the '...' field, these necessarily need to be \n1.  An object produced by browse() and \n2.  A logical statement.")
        }else {
          stop("Error: if you provide 2 inputs in the '...' field, these can only be \n1. An object produced by browse() and \n2.A logical statement.")
        }
      }
    }
    subset_arg <- paste(subset_inherit, subset_get_dat, sep = " AND ")
  }
  
  # if user provides 1 object
  if( length(call_list) == 1 ){
  
    if(class(call_list[[1]]$expr) == "name"){
      if( "popler" %in% class(eval(call_list[[1]]$expr,call_list[[1]]$env)) ){
        subset_inherit <- parse_to_sql_search( 
                          attributes(eval(call_list[[1]]$expr,call_list[[1]]$env))$search_argument )
      } else{
        stop("Error: The object you provide must be produced by browse()")
      }
    }else{
      subset_get_dat <- parse_to_sql_search( call_list[[1]]$expr )
    }
    
    subset_arg <- paste0(subset_inherit, subset_get_dat)

  }
   
  return(subset_arg)
}
   


inherit_variables <- function(..., all_columns){

  # calls
  call_list <- lazyeval::lazy_dots(...)
  # sql translations
  subset_inherit <- subset_get_dat <- NULL
  
  
  # if more than 2 arguments provided in '...'
  if( length(call_list) > 2 ){
    stop("Error: you cannot enter more than 2 arguments in the '...' field.\n  After the second comma, please refer to the arguments described in the documentation.")
  }
  
  # if two arguments provided in '...'
  if( length(call_list) == 2 ){
  
    # test what class expressions belong to   
    arg_class_vec <- c(class( call_list[[1]]$expr ),class( call_list[[2]]$expr ))
    
    if( all( c("name", "call") %in% arg_class_vec)){
      
      # expression "name" should be the "popler" object
      i = which(arg_class_vec %in% "name" )
      if(is.element("popler", class(eval(call_list[[i]]$expr,call_list[[i]]$env)))){
     
        inherit_elem    <- as.character( attributes(eval(call_list[[1]]$expr,call_list[[1]]$env))$search_argument )
        inherit_elem    <- gsub("order", "ordr", inherit_elem)
        inherit_elem    <- gsub("class", "clss", inherit_elem)
        inherit_browse  <- popler:::inherit_search(all_columns, inherit_elem) 
        
      } else{ #the nonlogical argument MUST be a popler object
        stop("Error: The object you provide must be produced by browse()")
      }
      
      # the "call" is a logical expression
      i2 = setdiff(c(1,2), i)
      inherit_elem    <- as.character( call_list[[i2]]$expr )
      inherit_elem    <- gsub("order", "ordr", inherit_elem)
      inherit_elem    <- gsub("class", "clss", inherit_elem)
      inherit_subset  <- popler:::inherit_search(all_columns, inherit_elem)
      
    }else{
      if( all( c("name", "name") %in% arg_class_vec) ){
        stop("Error: You apparently provided two separate objects. \n  If you provide 2 inputs in the '...' field, these necessarily need to be \n1.  An object produced by browse() and \n2.  A logical statement.")
      }else{ 
        if( all( c("call", "call") %in% arg_class_vec)){
          stop("Error: You apparently provided two separate logical statements. \n  If you provide 2 inputs in the '...' field, these necessarily need to be \n1.  An object produced by browse() and \n2.  A logical statement.")
        }else {
          stop("Error: if you provide 2 inputs in the '...' field, these can only be \n1. An object produced by browse() and \n2.A logical statement.")
        }
      }
    }
    
  }
  
  # if one argument provided in '...'
  if( length(call_list) == 1 ){
    
    if( class(call_list[[1]]$expr) == "popler" ){
      inherit_elem    <- as.character( attributes(browsed_data)$search_argument )
      inherit_elem    <- gsub("order", "ordr", inherit_elem)
      inherit_elem    <- gsub("class", "clss", inherit_elem)
      inherit_browse  <- popler:::inherit_search(all_columns, inherit_elem) 
    } else {
      inherit_browse  <- NULL
    }
    
    if( class(call_list[[1]]$expr) == "call" ){
      inherit_elem    <- as.character( call_list[[1]]$expr )
      inherit_elem    <- gsub("order", "ordr", inherit_elem)
      inherit_elem    <- gsub("class", "clss", inherit_elem)
      inherit_subset  <- popler:::inherit_search(all_columns, inherit_elem) 
    } else {
      inherit_subset  <- NULL
    }
    
  }
 
  return( unique( c(inherit_browse, inherit_subset) ) )
   
}  
  