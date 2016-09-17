##' Open web browser with the metadata webpage 
#'
#' Opens the metadata page of study, using its id
#' @param study_id The id of the study. Works with one id at a time.
#' @export
#' @return
#' @examples
#' look up metadata link for study number 21
#' metadata_webpage(study_id=21)

# The actual popler function=============================================================================
metadata_webpage <- function(study_id=NULL){
  
  x   <- popler:::dataPoplerFunction 
  x   <- subset(x,metarecordid == study_id)
  out <- as.character(unique(x$metalink))
  
  if(length(out)==1){
    browseURL(out)
  } else stop("Error: ID corresponds to multiple studies")
  
}