lter_maps = function(input){
  
  # a function for making panel plots
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
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
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  # re-run browse to get full table and untrimmed output
  pmk <- paste0(input$proj_metadata_key,collapse=",")
  input <- eval(parse(text=paste0("browse(proj_metadata_key %in% c(", pmk,"), full_tbl=T, trim=F)")))
  
  B <- table(input$lng_lter,input$lat_lter)
  latlon_count <- data.frame(lat=NA,lon=NA,count=-0.1)
  for(i in 1:nrow(B)){
    for(j in 1:ncol(B)){
      if(B[i,j]>0){
        latlon_count <- rbind(latlon_count,
                              data.frame(lat=as.numeric(colnames(B)[j]),
                                         lon=as.numeric(rownames(B)[i]),
                                         count=B[i,j]))}
      
    }
  }
  
  # circle sizes for scale_size_area()
  s1 <- 1
  s3 <- max(latlon_count$count,na.rm=T)
  if(s3 %% 2){
    s2 <- (s3 - 1) / 2
  } else {
    s2 <- s3 / 2
  }
  sbreaks <- c(s1, s2, s3)
  if(s3==1){ sbreaks <- 1}
  
  # draw Alaska-based locations
  ak = map_data('world', region='USA')
  ak = ak[which(ak$subregion == 'Alaska'),]
  p_ak <- ggplot() + theme_bw() + ggtitle("Alaska") +
    theme(axis.title.x=element_blank(),
          legend.position="none") +
    geom_polygon(data = ak, aes(x=long, y = lat, group = group), fill="#fec44f") + 
    geom_point(data = latlon_count, aes(x=lon, y=lat, size=count), alpha=0.5) + 
    scale_x_continuous(limits = c(-180,-129), expand = c(0, 0)) +
    scale_y_continuous(limits = c(50,72), expand = c(0, 0)) +
    ylab("Latitude") +
    coord_map()
  
  # draw US-based locations
  us <- map_data("usa")
  p_us <- ggplot() + theme_bw() + ggtitle("Contiguous United States") +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="right") +
    geom_polygon(data = us, aes(x=long, y = lat, group = group), fill="#fec44f") + 
    geom_point(data = latlon_count, aes(x=lon, y=lat, size=count), alpha=0.5) + 
    scale_x_continuous(limits = c(-126,-66.6), expand = c(0, 0)) +
    scale_y_continuous(limits = c(24.5,50), expand = c(0, 0)) +
    scale_size_area(breaks=sbreaks, guide=guide_legend(title = "Number of\nprojects")) + 
    coord_map()
  
  # draw Antarctica-based locations
  an <- map_data("world")
  an <- an[an$region=="Antarctica",]
  p_an <- ggplot() + theme_bw() + ggtitle("Antarctica") +
    theme(axis.title.y=element_blank(),
          legend.position="none") +
    geom_polygon(data = an, aes(x=long, y = lat, group = group), fill="#fec44f") + 
    geom_point(data = latlon_count, aes(x=lon, y=lat, size=count), alpha=0.5) + 
    scale_x_continuous(limits = c(-180,180), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-85,-60), expand = c(0, 0)) +
    xlab("Longitude") +
    coord_map()
  
  # print it all on a panel
  multiplot(p_ak,p_us,p_an, layout=matrix(c(1,2,2,3,3,3), nrow = 2, byrow = TRUE))
  
  return(latlon_count)
}