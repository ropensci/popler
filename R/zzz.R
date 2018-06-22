.onAttach <- function(libname = find.package("popler"), pkgname = "popler") {
  # check if the summary table exists or needs to be updated
  pplr_summary_table_check()

}