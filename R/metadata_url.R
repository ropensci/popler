#' Get metadata information from a data object
#'
#' Load the webpage containing the metadata of the data sets downloaded through 
#' get_data(). If you downloaded data from multiple projects, this function opens multiple webpages. 
#' This is a wrapper of function browseURL in base. 
#' @param data_object An object produced by the function get_data()
#' @examples
#' 
#' \dontrun{
#' # Load the metadata webpages of the projects that contain data from the Poa genus.
#' fes_d <- browse(genus == "Festuca")
#' metadata_url( fes_d )
#' }
#' 
#' @importFrom dplyr select filter
#' @importFrom rlang .data
#' @export
# function definition 

pplr_metadata_url <- function(data_object){
  # study id(s)
  proj_ids  <- attributes(data_object)$unique_projects
  # load summary_table
  summary_table <- summary_table_import()
  # main table
  main_t <- dplyr::select(summary_table, 
                          .data$proj_metadata_key, 
                          .data$metalink)
  
  
  # test whether object is produced by `browse` or `get_data` ----
  if(is.null(proj_ids)){
    
    ids <- unique(data_object$proj_metadata_key)
    
  } else {
    ids <- proj_ids
  }
  
  # if length(ids) > 5, ask readers if they want to open all ## browsers --  
  if(length(ids) > 5)  {
    n <- readline(prompt=paste0("These are ",
                                length(ids),
                                " different projects. Do you want 
to open a browser for each one of them? 
Print 'N' if you want to refine the search(Y/N):") )
    n <- tolower(n)
  } else {
    n <- "y"
  }
  
  # open browsers --------------------------------------------------------
  if(n == "y"){
    for(i in 1:length(ids)){
      link <- unique(dplyr::filter(main_t,
                                   .data$proj_metadata_key == ids[i])$metalink)
      browseURL(link)
    }
  } 

}
