#' Get metadata information from a data object
#'
#' Load the webpage containing the metadata of the data sets downloaded through get_data(). If you downloaded data from multiple projects, this function opens multiple webpages. 
#' This is a wrapper of function browseURL in base. 
#' @param data_object An object produced by the function get_data() 
#' @export
#' @examples
#' 
#' # Load the metadata webpages of the projects that contain data from the Poa genus.
#' poa_data = get_data(subset = genus == "Poa")
#' metadata_url( poa_data )

# function definition 
metadata_url <- function(x){
  # study id(s)
  ids       <- attributes(x)$unique_projects
  # main table
  main_t    <- popler:::factor_to_character(popler:::main_popler)
  main_t    <- select(main_t, proj_metadata_key, metalink)
  
  for(i in 1:length(ids)){
    link <- unique(subset(main_t, proj_metadata_key == ids[i])$metalink)
    browseURL(link)
  }
}
