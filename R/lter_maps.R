#' @import grid
#' @import ggplot2

lter_maps = function(input){
  
  
  # a function for making panel plots
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    
    # function source: 
    # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout),
                                                                   ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  # re-run browse to get full table and untrimmed output
  input <- rebrowse(input)
  
  # get lat-long counts for each site
  B <- table(input$lng_lter,input$lat_lter)
  latlon_count <- data.frame(lat = NA,
                             lon = NA,
                             count = -0.1)
  for(i in 1:nrow(B)){
    for(j in 1:ncol(B)){
      if(B[i, j] > 0){
        latlon_count <- rbind(latlon_count,
                              data.frame(lat = as.numeric(colnames(B)[j]),
                                         lon = as.numeric(rownames(B)[i]),
                                         count=B[i, j]))}
      
    }
  }
  
  # circle sizes for scale_size_area()
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
    sbreaks <- c(1,2,3)
  }
  
  # draw Alaska-based locations
  ak = ggplot2::map_data('world', region='USA')
  ak = ak[which(ak$subregion == 'Alaska'), ]
  p_ak <- ggplot2::ggplot() + ggplot2::theme_bw() + 
    ggplot2::ggtitle("Alaska") +
    ggplot2::theme(axis.title.x = element_blank(),
          legend.position = "none") +
    ggplot2::geom_polygon(data = ak,
                          ggplot2::aes_(x = quote(long), 
                                        y = quote(lat), 
                                        group = quote(group)),
                          fill="#FF9D60") + 
    ggplot2::geom_point(data = latlon_count,
                        ggplot2::aes_(x = quote(lon), 
                                      y = quote(lat), 
                                      size = quote(count)),
                        alpha=0.5) + 
    ggplot2::scale_x_continuous(limits = c(-180,-129),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(50,72),
                                expand = c(0, 0)) +
    ggplot2::ylab("Latitude") +
    ggplot2::coord_map()
  
  # draw US-based locations
  us <- ggplot2::map_data("usa")
  p_us <- ggplot2::ggplot() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Contiguous United States") +
    ggplot2::theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right") +
    ggplot2::geom_polygon(data = us, 
                          ggplot2::aes_(x = quote(long), 
                                        y = quote(lat), 
                                        group = quote(group)),
                          fill="#FF9D60") + 
    ggplot2::geom_point(data = latlon_count,
                        ggplot2::aes_(x = quote(lon), 
                                      y = quote(lat), 
                                      size = quote(count)),
                        alpha=0.5) + 
    ggplot2::scale_x_continuous(limits = c(-126,-66.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(24.5,50), expand = c(0, 0)) +
    ggplot2::scale_size_area(breaks = sbreaks, 
                             guide = guide_legend(title = "Number of\nprojects")) + 
    ggplot2::coord_map()
  
  # draw Antarctica-based locations
  an <- ggplot2::map_data("world")
  an <- an[an$region=="Antarctica",]
  p_an <- ggplot2::ggplot() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Antarctica") +
    ggplot2::theme(axis.title.y = element_blank(),
          legend.position = "none") +
    ggplot2::geom_polygon(data = an,
                          ggplot2::aes_(x = quote(long), 
                                        y = quote(lat), 
                                        group = quote(group)),
                          fill="#FF9D60") + 
    ggplot2::geom_point(data = latlon_count,
                        ggplot2::aes_(x = quote(lon), 
                                      y = quote(lat), 
                                      size = quote(count)),
                        alpha=0.5) + 
    ggplot2::scale_x_continuous(limits = c(-180,180), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-85,-60), expand = c(0, 0)) +
    ggplot2::xlab("Longitude") +
    ggplot2::coord_map()
  
  # print it all on a panel
  multiplot(p_ak,  p_us, p_an, 
            layout = matrix(c(1,2,2,3,3,3),
                            nrow = 2, byrow = TRUE))
  
  return(latlon_count)
}
