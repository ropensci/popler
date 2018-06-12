# re-shape the data into appropriate shape for lter_maps. returns a list 
# of formatted data and breaks for 
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

#' @importFrom rlang quo
#' 
#' @noRd
# wraps the others to print the plots
lter_maps <- function(input) { 
  plot_pars <- prep_map_data(input)
  
  counts <- plot_pars$data
  sizes <- plot_pars$breaks
  
  long <- rlang::quo(long)
  lat <- rlang::quo(lat)
  count <- rlang::quo(count)
  group <- rlang::quo(group)
  
  ak_plot <- ak_map(count_data = counts, 
                    x = long,
                    y = lat,
                    polygon_group = group,
                    count_group = count)
  us_plot <- us_map(count_data = counts, 
                    x = long,
                    y = lat,
                    polygon_group = group,
                    count_group = count,
                    size_breaks = sizes)
  an_plot <- an_map(count_data = counts, 
                    x = long,
                    y = lat,
                    polygon_group = group,
                    count_group = count)
  
  layout_mat <- matrix(c(1, 2, 2,
                         3, 3, 3),
                       nrow = 2, 
                       byrow = TRUE)

  multiplot(ak_plot, us_plot, an_plot, layout = layout_mat)
  
  
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
#' xlab ylab coord_map scale_size_area element_blank
#' @importFrom rlang quo !!
#' 
#' @noRd

ak_map <- function(count_data, x, y, polygon_group, count_group) {
  
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
                   legend.position = "none") +
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
    ggplot2::ylab("Latitude") +
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
#' xlab ylab coord_map scale_size_area element_blank
#' @importFrom rlang quo !!
#' 
#' @noRd

us_map <- function(count_data, x, y, polygon_group, count_group, size_breaks) {
  
  # Quosured in lter_maps(), so no need to quo again here
  # x <- rlang::quo(x)
  # y <- rlang::quo(y)
  # polygon_group <- rlang::quo(polygon_group)
  # count_group <- rlang::quo(count_group)
  
  us <- ggplot2::map_data('usa')
  p_us <- ggplot2::ggplot() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Contiguous United States") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "right") +
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
    ggplot2::scale_x_continuous(limits = c(-126, -66.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(24.5, 50), expand = c(0, 0)) +
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
#' xlab ylab coord_map scale_size_area element_blank
#' @importFrom rlang  !!
#' 
#' @noRd

an_map <- function(count_data, x, y, polygon_group, count_group) {
  
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
                   legend.position = "none") +
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
    ggplot2::xlab("Longitude") +
    ggplot2::coord_map()
  
  return(p_an)
  
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
