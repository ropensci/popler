#' @title  Spatial-temporal replication of data sets 
#' 
#' @description Create figures for temporal replication of sites in a given 
#' data set.
#' 
#' @param input The output from \code{pplr_get_data()}. This will not work
#' with output from \code{pplr_browse} as the raw data is required to calculate
#' the amount of replication.
#' @param return_plot A logical indicating whether to return a copy of the 
#' \code{input} data or the \code{ggplot} object created by the function. Use
#' \code{TRUE} to return the \code{ggplot} object for subsequent modification.
#' Use \code{FALSE} to return an invisible copy of the \code{input} object (
#' useful for piping). Default is \code{FALSE}.
#' 
#' @return The \code{input} object (invisibly) or a \code{ggplot2} object.
#' 
#' @examples 
#' \dontrun{
#' 
#' library(ggplot2)
#' library(dplyr)
#' 
#' # create an unmodified figure
#' BNZ <- pplr_get_data(lterid == 'BNZ')
#' 
#' pplr_site_rep(BNZ)
#' 
#' # Return the figure instead of the data for subsequent modification
#' Antarctica <- pplr_get_data(lterid == 'PAL')
#' 
#' pplr_site_rep(Antarctica,
#'               return_plot = TRUE) + 
#'    ggtitle("Penguins Rock!")
#'    
#' # Use within pipes. Cannot return and modify the figure this way.
#' pplr_get_data(lterid == 'SEV') %>% 
#'   pplr_site_rep(return_plot = FALSE) %>%
#'   pplr_report_metadata()
#' }
#' 
#' @importFrom ggplot2 ggplot aes geom_point theme_bw xlab ylab
#' @importFrom rlang quo quo !! sym
#' 
#' @export

pplr_site_rep <- function(input, 
                          return_plot = FALSE){
  
  
  if(inherits(input, 'browse')) {
    stop("'pplr_site_rep()' only works with outputs from 'pplr_get_data()'.",
         call. = FALSE)
  }
    
  
  x <- rlang::quo(input$year)
  y <- rlang::quo(input$spatial_replication_level_1)

  rep_plot <- ggplot2::ggplot(data = input,
                              ggplot2::aes(x = !! x,
                                           y = !! y)) +
    ggplot2::geom_point(size = 5) +
    ggplot2::theme_bw() + 
    ggplot2::xlab("Year with available data") + 
    ggplot2::ylab("Site")
  
  print(rep_plot)
  
  if(return_plot) {
    return(rep_plot)
  } else {
    invisible(input)
  }
}
