# changes a column name from one name to another
colname_change = function(from, to, x){
  names(x) <- gsub(from,to,names(x))
  return(x)
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
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i,2]), 
                                  gsub("@@", comps[i], 
                                       gsub("@@@", op, r_trt))),
                             logic[i])
      
    } else if(LHS_RHS[i,1] == "structure"){
      
      # update "structure" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i,2]),
                                  gsub("@@", comps[i],
                                       gsub("@@@", op, r_str))),
                             logic[i])
      
    } else{
      
      # for any other LHS, just put the query back together
      query_list[i] = paste(LHS_RHS[i,1], comps[i], string_eval_local(LHS_RHS[i,2]), logic[i])
      
    }
  }
  
  # collapse query_list to a single string, convert to expression, and return it
  return(eval(parse(text=
                      paste0(unlist(query_list),    collapse="") %>%
                      paste0("substitute(", . ,")", collapse="")))
  )
}

# evaluate a string using the local environment, return the evaluation as string
string_eval_local = function(x){
  deparse(eval(parse(text=paste0("local(",x,")"))))
}

# function to (quietly) close database connections
db_close = function(connection, quiet=T){
  if(quiet){
    # silent disconnect from db
    invisible(RPostgreSQL::dbDisconnect(connection$con,quiet=TRUE))
    
    # silent garbage collect
    invisible(suppressMessages(gc()))
    
  } else {
    # silent disconnect from db
    invisible(RPostgreSQL::dbDisconnect(connection$con,quiet=TRUE))
    
    # silent garbage collect
    invisible(gc())
  }
}