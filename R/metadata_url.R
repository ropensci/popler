#' @title Get metadata information from a data object
#'
#' @description Load the webpage containing the metadata of the data sets 
#' contained in objects produced by \code{pplr_browse} or downloaded through 
#' \code{pplr_get_data()}. If you downloaded data from multiple projects, 
#' this function will open multiple webpages. This is a wrapper of function 
#' \code{browseURL} in \code{base}.
#' @param input An object produced by the function \code{pplr_get_data()}.
#' @examples
#' 
#' \dontrun{
#' # Load the metadata webpages of the projects that contain data from the Poa genus.
#' fes_d <- pplr_browse(genus == "Festuca")
#' pplr_metadata_url( fes_d )
#' }
#' 
#' @importFrom dplyr select filter
#' @importFrom rlang .data
#' @export

pplr_metadata_url <- function(input){
  
  # study id(s)
  proj_ids  <- attributes(input)$unique_projects
  # load summary_table
  summary_table <- pplr_summary_table_import()
  # main table
  main_t <- dplyr::select(summary_table, 
                          .data$proj_metadata_key, 
                          .data$metalink,
                          .data$doi) %>% 
              unique
  
  
  # test whether object is produced by `browse` or `get_data` ----
  if(is.null(proj_ids)){
    
    ids <- unique(input$proj_metadata_key)
    
  } else {
    ids <- proj_ids
  }
  
  # if length(ids) > 5, ask readers if they want to open all ## browsers --  
  if(length(ids) > 5)  {
    n <- readline(prompt=paste0("These are ",
                                length(ids),
                                " different projects. Do you want 
to open a browser for each one of them? 
Return 'N' if you want to refine the search(Y/N):") )
    n <- tolower(n)
  } else {
    n <- "y"
  }
  
  # open browsers --------------------------------------------------------
  if(n == "y"){
    for(i in seq_len(length(ids))){
      
      # store doi link (if present)
      link <- dplyr::filter(main_t, 
                            .data$proj_metadata_key == ids[i]) %>% 
                # grab DOI - or url if DOI not present 
                links_get
      
      # apply recusrively in case of multiple links
      sapply(link, browseURL)
      
      # # use url only if you don't have doi
      # if( doi_link == 'NA'){
      #   link <- unique(dplyr::filter(main_t,
      #                                .data$proj_metadata_key == ids[i])$metalink)
      #   browseURL(link)
      # }else{
      #   browseURL(doi_link)
      # }
      
    }
  } 

}
