.onLoad <- function(libname = find.package("popler"), pkgname = "popler") {
  
  # just a goofy test message
  message("Welcome to popler, y'all!")
  
  # check if the summary table exists or needs to be updated
  #summary_table_check()
  
  # load the summary table into the namespace
  devtools::load_data()
}