#' @title  Spatial-temporal replication of data sets 
#' 
#' @description Create figures for temporal replication of a given data set at a given
#' spatial grain.
#' 
#' @param input The output from \code{pplr_get_data()}. This will not work
#' with output from \code{pplr_browse} as the raw data is required to calculate
#' the amount of replication.
#' @param rep_level An integer corresponding to the desired spatial replication
#' level. Values of 1-4 are possible, with 1 being the default. 
#' @param return_plot A logical indicating whether to return a copy of the 
#' \code{input} data or the \code{ggplot} object created by the function. Use
#' \code{TRUE} to return the \code{ggplot} object for subsequent modification.
#' Use \code{FALSE} to return an invisible copy of the \code{input} object (
#' useful for piping). Default is \code{FALSE}.
#' 
#' @details Higher values of \code{rep_level} indicate larger spatial grains. 
#' Be sure to check the values of \code{spatial_replication_level_X_label} in 
#' the \code{pplr_get_data()} object to ensure that the results make sense.
#' 
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
#' pplr_site_rep(BNZ, rep_level = 3)
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
#' @importFrom rlang enquo quo !! sym
#' 
#' @export

pplr_site_rep <- function(input, 
                          rep_level = 1,
                          return_plot = FALSE){
  
  
  if(inherits(input, 'browse')) {
    stop("'pplr_site_rep()' only works with outputs from 'pplr_get_data()'.",
         call. = FALSE)
  }
    
  replication <- paste0('spatial_replication_level_',
                        rep_level,
                        collapse = "")
  
  if(all(is.na(input[ ,replication]))) {
    stop("No spatial replication data available for this spatial grain.",
         "\nTry a lower replication level (e.g. set 'rep_level' to smaller value).",
         call. = FALSE)
  }
  
  rep_level <- rlang::sym(replication)
  
  x <- rlang::quo(input$year)
  y <- rlang::enquo(rep_level)

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
