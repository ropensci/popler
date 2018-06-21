#' @name pplr_save
#' @rdname save-load
#' 
#' @title Save and load \code{pplr_get_data()} outputs
#' 
#' @description Utility functions to save and load data downloaded from 
#' \code{popler}.
#' 
#' @param input An object of class \code{get_data} that you wish to save for
#' later use.
#' @param file The name of the file. Do not specify a file type, only the 
#' path and the name of the object.
#' 
#' @details These functions are thin wrappers around \code{saveRDS}
#' and \code{readRDS}. \code{.rds} file formats are highly compressed
#' so they do not take up a lot of storage, but can be read and written to
#' faster than \code{.csv} files (with the downside of being unreadable unless
#' opened in R). These are exported so that those who are unfamiliar
#' with these formats can don't need to re-download data with each new session.
#' For more control over these functions, use \code{saveRDS} and \code{readRDS}
#' directly.
#' 
#' @return For \code{pplr_save}: \code{input} (invisibly).
#' 
#' For \code{pplr_load}: A previously saved \code{get_data} object.
#' 
#' @examples 
#' \dontrun{
#' 
#' data <- pplr_get_data(lterid == "SEV")
#' 
#' pplr_save(data, file = "some/path/downloaded_data")
#' 
#' 
#' data_reloaded <- pplr_load(file = "some/path/downloaded_data")
#' 
#' 
#' stopifnot(identical(data, data_reloaded))
#' 
#' } 
#'
#' @seealso \code{\link[base]{saveRDS}}, \code{\link[base]{readRDS}}
#' 
#' @export
pplr_save <- function(object, file) {
  
  if(!inherits(object, 'get_data')){
    stop('"pplr_save" is only intended for use with objects of "get_data" class')
  }
  
  filepath <- paste0(file, '.rds')
  
  
  saveRDS(object, file = filepath)
  
  invisible(object)
  
}

#' @name pplr_load
#' @rdname save-load
#' 
#' @inheritParams pplr_save
#' @export

pplr_load <- function(file) {
  
  filepath = paste0(file, '.rds')
  
  out <- readRDS(file = filepath)
  
  return(out)
}

