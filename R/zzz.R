.onLoad <- function(libname = find.package("popler"), pkgname = "popler") {
  
  # check if the summary table exists or needs to be updated
  summary_table_check()
  
  message("summary table done")
  
  message(system.file("extdata", "summary_table.rda", package = "popler"))
  
  # load the summary table into the namespace
  load(system.file("extdata", "summary_table.rda", package = "popler"), envir=parent.env(environment()), verbose=FALSE)
  
  # just a goofy test message
  message("Welcome to popler, y'all!")
  
}