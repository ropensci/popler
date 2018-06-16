# re-shape the data into appropriate shape for lter_maps. returns a list 
# of formatted data and breaks for legend
#' @noRd
prep_map_data <- function(input) { 
  input <- rebrowse(input)
  
  # get lat-long counts for each site
  B <- table(input$lng_lter, input$lat_lter)
  latlon_count <- data.frame(lat = NA,
                             long = NA,
                             count = -0.1)
  # convert to long format
  for(i in 1:nrow(B)){
    for(j in 1:ncol(B)){
      if(B[i, j] > 0){
        latlon_count <- rbind(latlon_count,
                              data.frame(lat = as.numeric(colnames(B)[j]),
                                         long = as.numeric(rownames(B)[i]),
                                         count=B[i, j]))
      }
      
    }
  }
  
  # circle sizes for the legend
  s1 <- 1
  s3 <- max(latlon_count$count, na.rm = TRUE)
  if(s3 %% 2){
    s2 <- (s3 - 1) / 2
  } else {
    s2 <- s3 / 2
  }
  sbreaks <- c(s1, s2, s3)
  if(s3 == 1){ 
    sbreaks <- 1
  }
  if(s3 == 2){
    sbreaks <- c(1,2)
  }
  if(s3 == 3){ 
    sbreaks <- c(1, 2, 3)
  }
  
  plot_data <- list(data = latlon_count,
                    breaks = sbreaks)
  
  return(plot_data)
  
}

#' Generate maps of LTER sites
#' 
#' @description Generates maps of LTER sites in a given \code{input} object.
#' Sizes of site markers correspond to the number of studies at a given site.
#' 
#' @param input An object created by either \code{pplr_browse()} or
#' \code{pplr_get_data()}
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
#' library(dplyr)
#' # Return the invisible object and keep piping
#' 
#' browse_object <- pplr_browse(proj_metadata_key == 11)
#' 
#' browse_object %>%
#'   pplr_maps() %>% 
#'   pplr_get_data()
#'   
#'   
#' 
#' }
#' 
#' @importFrom rlang quo
#' 
#' @export
# wraps the others to print the plots UNFINISHED----------------
pplr_maps <- function(input, return_plot = FALSE) { 
  plot_pars <- prep_map_data(input)
  
  counts <- plot_pars$data
  sizes <- plot_pars$breaks
  
  long <- rlang::quo(long)
  lat <- rlang::quo(lat)
  count <- rlang::quo(count)
  group <- rlang::quo(group)
  
  plots <- list()
  
  # Next part identifies which plots should be drawn and who gets a legend
  # based on who is drawn. Rules are as follows:
  # All 3 - US plot, legend on right,
  # US + AK - US plot, legend on right
  # US + AN - US plot, legend on right
  # AK + AN - AK plot, legend on right
  # US/AK only - legend on right
  # AN only - legend on bottom
  
  if(any(counts$lat > 50, na.rm = TRUE)) {
    ak_plot <- ak_map(count_data = counts, 
                      x = long,
                      y = lat,
                      polygon_group = group,
                      count_group = count,
                      size_breaks = sizes)
    plots$ak <- ak_plot
  }
  
  if(any(counts$lat < 50 & counts$lat > 0, na.rm = TRUE)) {
    us_plot <- us_map(count_data = counts, 
                      x = long,
                      y = lat,
                      polygon_group = group,
                      count_group = count,
                      size_breaks = sizes)
    plots$us <- us_plot
  }
  
  if(any(counts$lat < -40, na.rm = TRUE)) {
    an_plot <- an_map(count_data = counts, 
                      x = long,
                      y = lat,
                      polygon_group = group,
                      count_group = count,
                      size_breaks = sizes)
    plots$an <- an_plot
  }
  
  if(any(counts$lat > -40 & counts$lat < 0, na.rm = TRUE)) { 
    mc_plot <- mc_map(count_data = counts,
                      x = long,
                      y = lat,
                      polygon_group = group,
                      count_group = count,
                      size_breaks = sizes)
    
    plots$mc <- mc_plot
    
  }
  
  if(length(plots) == 4) {
    plot_all(plots)
  } # all plots
  
  if(length(plots) == 3){
    if(!'mc' %in% names(plots)) plot_3_no_mc(plots)
    if(!'an' %in% names(plots)) plot_3_no_an(plots)
    if(!'us' %in% names(plots)) plot_3_no_us(plots)
    if(!'ak' %in% names(plots)) plot_3_no_ak(plots)
  } 
  
  if(length(plots) == 2) {
    if(all(c('an', 'ak') %in% names(plots))) plot_2_ak_an(plots)
    if(all(c('an', 'mc') %in% names(plots))) plot_2_mc_an(plots)
    if(all(c('an', 'us') %in% names(plots))) plot_2_us_an(plots)
    if(all(c('us', 'mc') %in% names(plots))) plot_2_us_mc(plots)
    if(all(c('us', 'ak') %in% names(plots))) plot_2_us_ak(plots)
    if(all(c('mc', 'ak') %in% names(plots))) plot_2_ak_mc(plots)
  } # 2 plots
  
  if(length(plots) == 1) {
    if("an" %in% names(plots)) {
      print(plots[[1]] + ggplot2::theme(legend.position = "bottom"))
    } else {
      print(plots[[1]] + ggplot2::theme(legend.position = "right"))
    }
  }
  
  if(return_plot) {
    
    return(plots)
    
  } else { 
    
    invisible(input)
    
  }
}



#' @param count_data a data frame with counts of number of studies from each
#' lat-long pair of coordinates
#' @param x Usually longitude, but could be any value for an x-axis on a map
#' @param y Usually latitude, but could be any value for a y-axis on a map
#' @param polygon_group the name of the grouping variable to use to shade
#' polygons. Almost always \code{group}
#' @param count_group the name of the grouping variable to make size counts
#' on. This is almost always \code{count}
#'
#' @importFrom ggplot2 ggplot theme_bw aes ggtitle scale_x_continuous
#' scale_y_continuous geom_polygon geom_point theme map_data
#' xlab ylab coord_map scale_size_area element_blank margin
#' @importFrom rlang quo !!
#' 
#' @noRd

ak_map <- function(count_data,
                   x, y,
                   polygon_group,
                   count_group,
                   size_breaks) {
  
  # Quosured in lter_maps(), so no need to quo again here
  # x <- rlang::quo(x)
  # y <- rlang::quo(y)
  # polygon_group <- rlang::quo(polygon_group)
  # count_group <- rlang::quo(count_group)
  

  ak <- ggplot2::map_data('world', region='USA')
  ak <- ak[which(ak$subregion == 'Alaska'), ]
  p_ak <- ggplot2::ggplot() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Alaska") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   legend.position = "right",
                   plot.margin = ggplot2::margin(t = 5,
                                                 l = 2.5,
                                                 r = 2.5,
                                                 b = 0)) +
    ggplot2::geom_polygon(data = ak,
                          ggplot2::aes(x = !! x, 
                                        y = !! y, 
                                        group = !! polygon_group),
                          fill="#FF9D60") + 
    ggplot2::geom_point(data = count_data,
                        ggplot2::aes(x = !! x, 
                                     y = !! y, 
                                     size = !! count_group),
                        alpha = 0.5) + 
    ggplot2::scale_x_continuous(limits = c(-180, -129),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(50, 72),
                                expand = c(0, 0)) +
    ggplot2::ylab("") +
    ggplot2::xlab("") + 
    ggplot2::scale_size_area(breaks = size_breaks, 
                             guide = 
                               ggplot2::guide_legend(title = 
                                                       "Number of\nprojects")) + 
    ggplot2::coord_map()
  
  return(p_ak)
  
}



#' @param count_data a data frame with counts of number of studies from each
#' lat-long pair of coordinates
#' @param x Usually longitude, but could be any value for an x-axis on a map
#' @param y Usually latitude, but could be any value for a y-axis on a map
#' @param polygon_group the name of the grouping variable to use to shade
#' polygons. Almost always \code{group}
#' @param count_group the name of the grouping variable to make size counts
#' on. This is almost always \code{count}
#' @param size_breaks The break points for \code{count_group}
#'
#' @importFrom ggplot2 ggplot theme_bw aes ggtitle scale_x_continuous
#' scale_y_continuous geom_polygon geom_point theme map_data
#' xlab ylab coord_map scale_size_area element_blank margin
#' @importFrom rlang quo !!
#' 
#' @noRd

us_map <- function(count_data,
                   x, y, 
                   polygon_group, 
                   count_group, 
                   size_breaks) {
  
  # Quosured in lter_maps(), so no need to quo again here
  # x <- rlang::quo(x)
  # y <- rlang::quo(y)
  # polygon_group <- rlang::quo(polygon_group)
  # count_group <- rlang::quo(count_group)
  
  us <- ggplot2::map_data('usa')
  p_us <- ggplot2::ggplot() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("United States and Puerto Rico") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "right",
                   plot.margin = ggplot2::margin(t = 5,
                                                 l = 2.5,
                                                 r = 2.5,
                                                 b = 0)) +
    ggplot2::geom_polygon(data = us,
                          ggplot2::aes(x = !! x, 
                                       y = !! y, 
                                       group = !! polygon_group),
                          fill="#FF9D60") + 
    ggplot2::geom_point(data = count_data,
                        ggplot2::aes(x = !! x, 
                                     y = !! y, 
                                     size = !! count_group),
                        alpha = 0.5) + 
    ggplot2::xlab("") + 
    ggplot2::ylab("") + 
    ggplot2::scale_x_continuous(limits = c(-126, -62.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(15, 50), expand = c(0, 0)) +
    ggplot2::scale_size_area(breaks = size_breaks, 
                             guide = 
                               ggplot2::guide_legend(title = 
                                                       "Number of\nprojects")) + 
    ggplot2::coord_map()
  
  
  return(p_us)
  
}

#' @param count_data a data frame with counts of number of studies from each
#' lat-long pair of coordinates
#' @param x Usually longitude, but could be any value for an x-axis on a map
#' @param y Usually latitude, but could be any value for a y-axis on a map
#' @param polygon_group the name of the grouping variable to use to shade
#' polygons. Almost always \code{group}
#' @param count_group the name of the grouping variable to make size counts
#' on. This is almost always \code{count}
#'
#' @return A class \code{ggplot} object
#' @importFrom ggplot2 ggplot theme_bw aes ggtitle scale_x_continuous
#' scale_y_continuous geom_polygon geom_point theme map_data
#' xlab ylab coord_map scale_size_area element_blank margin
#' @importFrom rlang  !!
#' 
#' @noRd

an_map <- function(count_data, 
                   x, y,
                   polygon_group, 
                   count_group, 
                   size_breaks) {
  
  # Quosured in lter_maps(), so no need to quo again here
  # x <- rlang::quo(x)
  # y <- rlang::quo(y)
  # polygon_group <- rlang::quo(polygon_group)
  # count_group <- rlang::quo(count_group)
  
  
  
  # draw Antarctica-based locations
  an <- ggplot2::map_data("world")
  an <- an[an$region == "Antarctica", ]
  p_an <- ggplot2::ggplot() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Antarctica") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   legend.position = "bottom",
                   plot.margin = ggplot2::margin(t = 5,
                                                 l = 2.5,
                                                 r = 2.5,
                                                 b = 0)) +
    ggplot2::geom_polygon(data = an,
                          ggplot2::aes(x = !! x, 
                                       y = !! y, 
                                       group = !! polygon_group),
                          fill="#FF9D60") + 
    ggplot2::geom_point(data = count_data,
                        ggplot2::aes(x = !! x, 
                                     y = !! y, 
                                     size = !! count_group),
                        alpha = 0.5) + 
    ggplot2::scale_x_continuous(limits = c(-180, 180), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-85, -60), expand = c(0, 0)) +
    ggplot2::xlab("") +
    ggplot2::ylab("") + 
    ggplot2::scale_size_area(breaks = size_breaks, 
                             guide = 
                               ggplot2::guide_legend(title = 
                                                       "Number of projects")) + 
    ggplot2::coord_map()
  
  return(p_an)
  
}

mc_map <- function(count_data, 
                    x, y,
                    polygon_group, 
                    count_group,
                    size_breaks) {
  
  mcr <- ggplot2::map_data("world")
  mcr <- mcr[mcr$region == 'French Polynesia', ]
  
  p_mc <- ggplot2::ggplot() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("French Polynesia") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   legend.position = "right",
                   plot.margin = ggplot2::margin(t = 5, 
                                                 l = 2.5, 
                                                 r = 2.5, 
                                                 b = 0)) +
    ggplot2::geom_polygon(data = mcr,
                          ggplot2::aes(x = !! x, 
                                       y = !! y, 
                                       group = !! polygon_group),
                          fill="#FF9D60") + 
    ggplot2::geom_point(data = count_data,
                        ggplot2::aes(x = !! x,
                                     y = !! y,
                                     size = !! count_group),
                        alpha = 0.5) +
    ggplot2::scale_x_continuous(limits = c(-150, -148.5)) +
    ggplot2::scale_y_continuous(limits = c(-18, -17)) +
    ggplot2::xlab("") +
    ggplot2::ylab("") + 
    ggplot2::scale_size_area(breaks = size_breaks, 
                             guide = 
                               ggplot2::guide_legend(title = 
                                                       "Number of \nprojects")) + 
    ggplot2::coord_map()
  
  return(p_mc)
}


#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#' @noRd
# Function to create multiple plots on same page
multiplot <-  multiplot <- function(..., layout) {
  # function source: 
  # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...))
  
  numplots <- length(plots)

  
  if (numplots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = 
                                        grid::grid.layout(nrow(layout),
                                                          ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in seq_len(numplots)) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], 
            vp = grid::viewport(layout.pos.row = matchidx$row,
                                layout.pos.col = matchidx$col))
    }
  }
}



#' @importFrom ggplot2 theme
#' @noRd
plot_all <- function(plots) {
  
  layout_mat <- matrix(c(1, 2, 2,
                         3, 4, 4),
                       nrow = 2,
                       byrow = TRUE) 
  
  multiplot(plots$ak + ggplot2::theme(legend.position = 'none'),
            plots$us + ggplot2::theme(legend.position = 'none'),
            plots$mc + ggplot2::theme(legend.position = 'none'),
            plots$an + ggplot2::theme(legend.position = 'bottom'),
            layout = layout_mat)
}

#' @importFrom ggplot2 theme
#' @noRd
plot_3_no_mc <- function(plots) {
  layout_mat <- matrix(c(1, 2, 2,
                         3, 3, 3),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$ak + ggplot2::theme(legend.position = 'none'),
            plots$us,
            plots$an + ggplot2::theme(legend.position = 'none'),
            layout = layout_mat)
  
}

#' @importFrom ggplot2 theme
#' @noRd
plot_3_no_an <- function(plots) {
  layout_mat <- matrix(c(1, 2, 2,
                         3, 3, 3,
                         3, 3, 3),
                       nrow = 3,
                       byrow = TRUE)
  
  multiplot(plots$mc + ggplot2::theme(legend.position = 'none'),
            plots$ak + ggplot2::theme(legend.position = 'none'),
            plots$us,
            layout = layout_mat)
  
}

#' @importFrom ggplot2 theme
#' @noRd
plot_3_no_ak <- function(plots) {
  layout_mat <- matrix(c(1, 2, 2,
                         3, 3, 3),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$mc + ggplot2::theme(legend.position = 'none'),
            plots$us,
            plots$an + ggplot2::theme(legend.position = 'none'),
            layout = layout_mat)
}

#' @importFrom ggplot2 theme
#' @noRd
plot_3_no_us <- function(plots) {
  layout_mat <- matrix(c(1, 2, 2,
                         3, 3, 3),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$mc + ggplot2::theme(legend.position = 'none'),
            plots$ak,
            plots$an + ggplot2::theme(legend.position = 'none'),
            layout = layout_mat)
}

#' @importFrom ggplot2 theme
#' @noRd
plot_2_us_ak <- function(plots) {
  layout_mat <- matrix(c(1, 2, 2,
                         1, 2, 2),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$ak + ggplot2::theme(legend.position = 'none'),
            plots$us,
            layout = layout_mat)
  
}

#' @importFrom ggplot2 theme
#' @noRd
plot_2_us_mc <- function(plots) {
  
  layout_mat <- matrix(c(1, 2, 2,
                         1, 2, 2),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$mc + ggplot2::theme(legend.position = 'none'),
            plots$us,
            layout = layout_mat)
}

#' @importFrom ggplot2 theme
#' @noRd
plot_2_us_an <- function(plots) {
  
  layout_mat <- matrix(c(1, 1, 1,
                         2, 2, 2),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$us,
            plots$an + ggplot2::theme(legend.position = 'none'),
            layout = layout_mat)
}

#' @importFrom ggplot2 theme
#' @noRd
plot_2_ak_mc <- function(plots) {
  
  layout_mat <- matrix(c(1, 2, 2,
                         1, 2, 2),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$mc + ggplot2::theme(legend.position = 'none'),
            plots$ak,
            layout = layout_mat)
}

#' @importFrom ggplot2 theme
#' @noRd
plot_2_ak_an <- function(plots) {
  
  layout_mat <- matrix(c(1, 1, 1,
                         2, 2, 2),
                       nrow = 2,
                       byrow = TRUE)
  
  multiplot(plots$ak,
            plots$an + ggplot2::theme(legend.position = 'none'),
            layout = layout_mat)
}

#' @importFrom ggplot2 theme
#' @noRd
plot_2_mc_an <- function(plots) {
  
  layout_mat <- matrix(c(1, 2, 2),
                       nrow = 1,
                       byrow = TRUE)
  
  multiplot(plots$mc + ggplot2::theme(legend.position = 'none'),
            plots$an,
            layout = layout_mat)
}



