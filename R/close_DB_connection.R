#' Closes a connection with popler
#'
#' Close the connection with the popler database
#' @param there is actually no parameter in this function!
#' @export

close_DB_connection = function(connection){
  # close the database connection (doesn't work?)
  invisible(RPostgreSQL::dbDisconnect(connection$con))

  # remove the connection from the workspace
  a <- deparse(substitute(connection))
  invisible(rm(list=a,envir=sys.frame(-1)))

  # run garbage collection to make sure these connections are closed.
  # (and keep it quiet)
  capture.output(suppressMessages(gc(verbose=F)),file='NULL')
}
