.onAttach <- function(libname = find.package("popler"), pkgname = "popler") {
  # just a goofy test message
  packageStartupMessage("Welcome to popler, y'all!")
  # check if the summary table exists or needs to be updated
  pplr_summary_table_check()

}