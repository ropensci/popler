#' Opens a connection with popler
#'
#' Opens a connection with the popler database
#' @param there is actually no parameter in this function!
#' @export

open_DB_connection = function(){
  # login to the database
  src_postgres(dbname="popler", host="localhost", user="postgres", password="demography")
}
