#' @name pplr_site_rep
#' @rdname pplr_site_rep
#' 
#' @title  Spatial-temporal replication of data sets 
#' 
#' @description Functions to examine the number of temporal replicates contained in each 
#' spatial replication level of a dataset. 
#' \code{pplr_site_rep_plot} plots the temporal replicates available for each site.
#' \code{pplr_site_rep} produces logical vectors that identify the spatial replicates 
#' with enough temporal replication, or summary tables. 

#' 
#' @param input An object of class \code{get_data}. Note that this is not an 
#' output from \code{browse}, as the raw data is required to calculate
#' the amount of replication.
#' @param freq A number corresponding to the desired annual frequency of 
#' temporal replication. Studies that are 
#' replicated more frequently will be included in the counts and those that 
#' replicated less frequently will be excluded. If \code{return_logical = TRUE},
#' rows included in sites that are replicated at the desired frequency will have
#' a \code{TRUE} value, and rows that are not will have \code{FALSE} value. Values
#' greater than 1 correspond to sampling multiple times per year. For example, 
#' \code{freq = 2} indicates a desired sampling frequency of every 6 months. 
#' Values less than 1 indicate sampling intervals of greater than 1 year. For
#' example, \code{freq = 0.5} indicates a desired sampling frequency of once
#' every 2 years.
#' @param duration An integer corresponding to the desired duration of temporal
#' replication. Rows corresponding to sites with more replication will be 
#' included, while those with less will be excluded.
#' @param rep_level An integer corresponding to the level of spatial replication
#' desired. Values between 1 and 5 are possible (though higher levels may not be
#' present for some datasets). Higher values correspond to higher levels of 
#' spatial nestedness. The default value of \code{rep_level = 1} corresponds to 
#' entire sites.
#' @param return_logical Indicates whether to return a summary table of class
#' \code{tbl} or a logical vector. The default is \code{TRUE}.
#' 
#'
#' @return \code{pplr_site_rep_plot}: \code{input} object (invisibly) or a
#' \code{ggplot2} object. Use \code{return_plot} to control.
#' 
#' \code{pplr_site_rep}: A \code{tbl} or a logical vector of length 
#' \code{dim(input)[1]}. Use \code{return_logical} to control.
#' 
#' @details \code{pplr_site_rep_plot} produces a scatterplot showing the sites 
#' (\code{spatial_replication_level_1}) and years for which data is available.
#' 
#' \code{pplr_site_rep} works with any level of spatial replication and produces
#' either a summary table of temporal replication or a logical vector that can be used 
#' to subset a data set based on the desired frequency and length of time.
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' library(ggplot2)
#' library(dplyr)
#' 
#' # produce logical vector and subset using it. This can also be piped into a 
#' # the plotting function for visiualization
#' 
#' good_studies <- pplr_get_data(lterid == 'SEV') %>%
#'                    .[pplr_site_rep(input = .,
#'                                    duration = 12, 
#'                                    rep_level = 3), ] %>%
#'                    pplr_site_rep_plot()
#'                                        
#' 
#' # Or, make a neat summary table and decide where to go from there
#' SEV <- pplr_get_data(lterid == 'SEV')
#' 
#' rep_table <- pplr_site_rep(input = SEV,
#'                            freq = 0.5,
#'                            duration = 12,
#'                            return_logical = FALSE)
#'  
#' # pplr_site_rep_plot ---------------
#'                                                      
#' # create an unmodified figure
#' BNZ <- pplr_get_data(lterid == 'BNZ')
#' 
#' pplr_site_rep_plot(BNZ)
#' 
#' # Return the figure instead of the data for subsequent modification
#' Antarctica <- pplr_get_data(lterid == 'PAL')
#' 
#' pplr_site_rep_plot(Antarctica,
#'               return_plot = TRUE) + 
#'    ggtitle("Penguins Rock!")
#'    
#' # Use within pipes. Cannot return and modify the figure this way.
#' pplr_get_data(lterid == 'SEV') %>% 
#'   pplr_site_rep_plot(return_plot = FALSE) %>%
#'   pplr_report_metadata()
#' }
#' 
#' @importFrom dplyr bind_cols group_by summarise n ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang quo syms .data !! !!! 
#' @importFrom lubridate ydm 
#' @importFrom stringr str_split
#' 
#' @export

pplr_site_rep <- function(input, 
                          freq = 1, 
                          duration = 10,
                          rep_level = 1,
                          return_logical = TRUE) {
  
  if(!inherits(input, 'get_data')) {
    stop('"input" must be an object of class "get_data".')
  }
  
  if(rep_level < 1 & rep_level > 5) {
    stop('"rep_level" must be an integer between 1 and 5.')
  }

  # create symbols for rep_levels
  levels <- seq_len(rep_level)
  
  levels <- paste0('spatial_replication_level_', levels, sep = "")
  
  if(!all(levels %in% names(input))) {
    stop('requested "rep_level" is not available for this data set.\n',
         'Please try again with a lower value for "rep_level".',
         call. = FALSE)
  }
  
  group_col <- rlang::quo(group_col)
  
  # New data frame for filtering with single spatial rep group_column
  new_data <- dplyr::bind_cols(input, paste2(input, levels, group_col))
  

  group <- group_col
  year <- rlang::quo(year)
  
  # Remove years based on frequency
  
  # first, transform data to correct format and then figure out
  # what the user actually wants back
  
  
  # if there is daily data, capture it
  
  dates <- c('year', 'day', 'month')
  
  if(!'day' %in% names(new_data)){
    # otherwise, create dummy day variable
    new_data$day <- 1
  }
  
  # Same for months. If available, use it
  if(!'month' %in% names(new_data)) {
    new_data$month <- 6
  }
  
  # Some of the data sets will have some days entered, but will be missing others.
  # Consequently, they generate NAs when lubridate tries to parse them and are dropped
  # from calculations
  
  NA_months <- NA_days <- integer(0)
  
  if(any(is.na(new_data$month))) {
    warning('Some, but not all, "month" entries are missing and ',
            'will not be included in the calculations of replication ',
            '"duration" or "frequency".',
            call. = FALSE)
    NA_months <- which(is.na(new_data$month))
  }
  
  if(any(is.na(new_data$day))) {
    warning('Some, but not all, "day" entries are missing and ',
            'will not be included in the calculations of replication ',
            '"duration" or "frequency".',
            call. = FALSE)
    NA_days <- which(is.na(new_data$day))
    
  }
  
  sample_date <- rlang::quo(sample_date)
  
  # create formatted sample date
  new_data <- dplyr::bind_cols(new_data, paste2(new_data,
                                                dates,
                                                sample_date))
  
  new_data$sample_date <- suppressWarnings(lubridate::ydm(new_data$sample_date))
  
  sample_date <- rlang::quo(sample_date)
  
  
  summary <- new_data %>%
    dplyr::group_by(!! group, !! year) %>%
    dplyr::summarise(n_dates = length(unique(!! sample_date)))
  
  
  # perform frequency based filtering 
  if(freq <= 1) {
    
    # if the frequency is greater than 1 year, then it gets a little tricky.
    # first, compute the differences between each year at each site, and then 
    # create an index of sites meeting that criteria for frequency based 
    # filtering
    
    year_diffs <- rle(diff(summary$year))
    
    freq_idx <- which(year_diffs$values <= 1/freq & 
                      year_diffs$values >= 0)
    
    sites <- character(length(freq_idx))
    
    for(i in seq_len(length(freq_idx))) {
      idx <- sum(year_diffs$lengths[seq_len(freq_idx[i])])
      sites[i] <- summary$group_col[idx]
    }
    sites <- unique(sites)
    
    summary <- dplyr::filter(summary, .data$group_col %in% sites)
    
  } else {
    summary <- dplyr::filter(summary, .data$n_dates >= freq)
  }
  
  if(dim(summary)[1] < 1) {
    stop('No sites meet the frequency requirements for this combination of\n',
         '"freq" and "rep_level". Please use different settings.')
  }
  
  
  # next, compute which years are sampled consecutively. rle computes the number
  # times in a row that a value appears. Thus, if a difference of 1 appears more
  # than the requested duration, then that is what we want
  consecutive_years <- rle(diff(summary$year))
  
  if(freq >= 1){
    duration_idx <- which(consecutive_years$lengths >= duration &
                          consecutive_years$values == 1)
  } else {
    duration_idx <- which(consecutive_years$lengths >= duration &
                          consecutive_years$values <= 1/freq & 
                          consecutive_years$values > 0)
    
  }
  if(length(duration_idx) < 1) {
    stop('Chosen "duration" is too long for this data set.\n',
         'Please choose a shorter "duration".')
  }
  
  # rle doesn't really return what we want though. The actual group_col index
  # we need are the row numbers for each site replicate. Examine the output of 
  # rle to see why (length(consecutive_years[[1]] != length(summary$year)))
  # so, quick for loop to find the appropriate indices. 
  
  true_idx <- integer(length(duration_idx))
  site_ids <- character(length(duration_idx))
  
  for(i in seq_len(length(duration_idx))) {
    idx <- duration_idx[i]
    
    true_idx[i] <- sum(consecutive_years$lengths[seq_len(idx)])
    
    site_ids[i] <- summary$group_col[true_idx[i]]
    
  }
  
  
  
  
  # Now, depending on user selected output, create logical vector for subsetting,
  # or just return the correctly formatted table
  
  if(return_logical) {
    out <- new_data$group_col %in% site_ids
    
    out[c(NA_days, NA_months)] <- FALSE
    
  } else {
    
    summary <- summary %>%
      dplyr::filter(!! group %in% site_ids) %>%
      dplyr::group_by(!! group, !! year) %>%
      dplyr::summarise(number_of_samples = sum(.data$n_dates,
                                               na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    
    # re-build spatial_replication_level_x columns
    for(i in seq_len(length(levels))) {
      summary[ ,levels[i]] <- stringr::str_split(summary$group_col,
                                                 '-',
                                                 simplify = TRUE)[ ,i]
      
      
    }
    
    out <- dplyr::select(summary,
                         !!! rlang::syms(levels), 
                         .data$year,
                         .data$number_of_samples)
    
    
  }
  
  return(out)
}


#' @noRd
#' @importFrom rlang quo_name := !!
#' @importFrom purrr pmap_chr
#' @importFrom dplyr tibble
# helper to work in bind_cols
paste2 <- function(data, cols, name) {
  name <- rlang::quo_name(name)
  data <- data[ ,cols, drop = FALSE]
  listdf <- list()
  
  for(i in seq_len(dim(data)[2])) {
    listdf[[i]] <- data[ ,i]
  }
  
  out <- purrr::pmap_chr(listdf, paste, sep = '-')
  
  out <- dplyr::tibble(!! name := out)

  return(out)
}


#' @name pplr_site_rep_plot
#' @rdname pplr_site_rep
#' 
#' @inheritParams pplr_site_rep
#' @param return_plot A logical indicating whether to return a copy of the 
#' \code{input} data or the \code{ggplot} object created by the function. Use
#' \code{TRUE} to return the \code{ggplot} object for subsequent modification.
#' Use \code{FALSE} to return an invisible copy of the \code{input} object 
#' (useful for piping). Default is \code{FALSE}.
#' 
#' @importFrom ggplot2 ggplot aes geom_point theme_bw xlab ylab
#' @importFrom rlang quo !! 
#' 
#' @export

pplr_site_rep_plot <- function(input, 
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
