# Exmaple to show how to search multiple columns simultaneously
main_tab <- popler:::main_popler
df = main_tab[,paste0("structured_type_",1:3)]

# inverts grep's arguments to use with apply()
grep_wrap <- function(x,text){ return(grep(text,x)) }

# example: calculate indexes
indexes <- unique( unlist( apply(df,2,grep_wrap,"sex|size") ) )
r       <- indexes[order(indexes)]


# test a "call" object 
test_call   <- function(x){ test_call2( substitute(x) ) }
test_call2  <- function(x){
  
  if( grepl("structured_type",deparse(x)) ) print("YEYA") 
  
}
test_call(structured_type == "size")
