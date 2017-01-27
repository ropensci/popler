# update call function. 
update_call <- function(orig_call){
  
  # does call contain "structure"? --------------------------------------------------------------
  if( any(grepl("structure", deparse(orig_call))) & 
      any(!grepl("structured", deparse(orig_call))) ){
    
    tmp   <- orig_call
    path  <- NULL
    
    # vet the abstract syntax tree (AST) as long as you find expressions "calls" 
    while( class(tmp[[2]]) == "call" | class(tmp[[2]]) == "(" ){
      
      # typical "call" composed of 3 arguments
      if( length(tmp) == 3 ){ 
        second  <- paste0(deparse(tmp[[2]]), collapse="")
        third   <- paste0(deparse(tmp[[3]]), collapse="")
        test_v  <- c(grepl("structure", second), grepl("structure", third) )
        
        i       <- which( test_v == TRUE )
        tmp     <- tmp[[i+1]]
        path    <- c(path,i+1)
        
      } else { #call with 2 arguments
        tmp     <- tmp[[2]]
        path    <- c(path,2)
      }
      
    }
    
    # locate the call to change 
    if( is.null(path) ){
      locate_call   <- "orig_call"
    } else {
      locate_call   <- paste0("orig_call",paste0("[[",path,"]]",collapse=""))
    }
    
    # modify the call
    old_call      <- eval(parse(text=locate_call))
    operation     <- deparse(old_call[[1]])
    match_field   <- old_call[[3]]
    
    # expand boolean operation to structured_type 1 through 3
    new_cols      <- paste0("structured_type_", c(1:4))
    new_state     <- paste0(paste0( new_cols, operation,"'", match_field,"'"), collapse = " | ")
    new_call      <- parse(text = paste0("(",new_state,")"))[[1]]
    
    eval(parse(text = paste0(locate_call, " = new_call ")))
    
  }
  
  # check for "treatment" --------------------------------------------------------------
  if( any(grepl("treatment", deparse(orig_call))) & 
      any(!grepl("treatment_type", deparse(orig_call))) ){
    
    tmp   <- orig_call
    path  <- NULL
    
    # vet the abstract syntax tree (AST) as long as you find expressions "calls" 
    while( class(tmp[[2]]) == "call" | class(tmp[[2]]) == "(" ){
      
      # typical "call" composed of 3 arguments
      if( length(tmp) == 3 ){ 
        
        second  <- paste0(deparse(tmp[[2]]),collapse="")
        third   <- paste0(deparse(tmp[[3]]),collapse="")
        test_v  <- c(grepl("treatment", second), grepl("treatment", third) )
        
        i       <- which( test_v == TRUE )
        tmp     <- tmp[[i+1]]
        path    <- c(path,i+1)
        
      } else { #call with 2 arguments
        tmp     <- tmp[[2]]
        path    <- c(path,2)
      }
      
    }
    
    # locate the call to change 
    if( is.null(path) ){
      locate_call   <- "orig_call"
    } else {
      locate_call   <- paste0("orig_call",paste0("[[",path,"]]",collapse=""))
    }
    
    # modify the call
    old_call      <- eval(parse(text=locate_call))
    operation     <- deparse(old_call[[1]])
    match_field   <- old_call[[3]]
    
    # expand boolean operation to structured_type 1 through 3
    new_cols      <- paste0("treatment_type_", c(1:4))
    new_state     <- paste0(paste0( new_cols, operation,"'", match_field,"'"), collapse = " | ")
    new_call      <- parse(text = paste0("(",new_state,")"))[[1]]
    
    eval(parse(text = paste0(locate_call, " = new_call ")))
    
  }
  
  return(orig_call)
  
}

# single calls
#popler:::update_call( substitute(structure == "size"))
#popler:::update_call( substitute(treatment == "N"))

#popler:::update_call( substitute(structure == "size"))
#popler:::update_call( substitute(treatment == "N"))

#popler:::update_call( substitute( (treatment == 'N') ))

# combine other unrelated variables
#popler:::update_call( substitute(structure == "size" & lterid == "SEV"))
#popler:::update_call( substitute(structure == "size" & (lterid == "SEV" | lterid == "CDR")) )

# combine structure and treatment
#popler:::update_call( substitute(structure == "size" & lterid == "SEV"))
#popler:::update_call( substitute(structure == "size" & (lterid == "SEV" | lterid == "CDR")) )

# single quotation
#popler:::update_call( substitute(structure == 'size' & lterid == 'SEV'))
#popler:::update_call( substitute(structure == 'size' & (lterid == 'SEV' | lterid == 'CDR')) )

# keyword argument 
#popler:::update_call( substitute(structure %=% "size" & treatment %=% "N")  )

