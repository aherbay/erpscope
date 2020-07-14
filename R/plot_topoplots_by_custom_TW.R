plot_topoplots_by_custom_TW <-  function (data_diff,
                                          tw_array ,
                                          plotname) {

  #electrodeLocs <- readRDS("electrodeLocs_51elec.RDS")
  electrodeLocs <- locations_51_electrodes

  electrodeLocs <- rename(electrodeLocs,   Electrode = electrode)
  electrodeLocs <- subset(electrodeLocs, !(Electrode %in% c("AFz","PO5","PO6") ))

  print(head(electrodeLocs))

  # base elements for topo

  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100) {
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  maskRing <- circleFun(diameter = 1.42) #create a circle round the outside of the plotting area to mask the jagged edges of the interpolation
  headShape <- circleFun(c(0, 0), round(max(electrodeLocs$x)), npoints = 100) # 0
  nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))


  theme_topo <- function(base_size = 12)
  {
    theme_bw(base_size = base_size) %+replace%
      theme(
        rect             = element_blank(),
        line             = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
  }

  # for each time window

  topo_ggplots <- list()
  electro_ggplots <- list()




  for( tw_index in 1:length(tw_array)) {



    lowBound <- tw_array[[tw_index]][1]
    upperBound <- tw_array[[tw_index]][2]

    print(paste('Processing',lowBound,"to",upperBound))


    # compute mean diff voltage for each electrode for the given time window
    means_by_electrodes <- data_diff %>%
      filter ( Time > lowBound  & Time < upperBound ) %>%
      drop_na() %>%
      group_by(Electrode)%>%
      summarise(Voltage= mean(Voltage) )

    # preprocess the means voltage

    #print(head(means_by_electrodes))

    # to solve : 1: Column `Electrode` joining factor and character vector, coercing into character vector
    #electrodeLocs$Electrode <- as.factor(electrodeLocs$Electrode)
    means_by_electrodes <- means_by_electrodes %>% right_join(electrodeLocs, by = "Electrode")
    print(head(means_by_electrodes))

    gridRes <- 250 # Specify the number of points for each grid dimension i.e. the resolution/smoothness of the interpolation

    tmpTopo <- with(means_by_electrodes,
                    akima::interp(x = x, y = y, z = Voltage,
                                  xo = seq(min(x)*2,
                                           max(x)*2,
                                           length = gridRes),
                                  yo = seq(min(y)*2,
                                           max(y)*2,
                                           length = gridRes),
                                  linear = FALSE,
                                  extrap = TRUE)
    )

    interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

    names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

    interpTopo <- gather(interpTopo,
                         key = y,
                         value = Voltage,
                         -x,
                         convert = TRUE)

    interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

    interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

    # plot it


    # electrodes plots
    #electro_ggplots[[length(topo_ggplots) + 1]] <-  ggplot(headShape,aes(x,y))+
    # geom_path(size = 1.5)+
    # geom_point(data = means_by_electrodes,aes(x,y,colour = Voltage),size = 3)+
    # scale_colour_gradientn(colours = jet.colors(10),guide = "colourbar",oob = squish,limits = c(-2,2))+ #note: oob = squish forces everything outside the colour limits to equal nearest colour boundary (i.e. below min colours = min colour)
    # geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
    # theme_topo()+
    # coord_equal()+
    # labs(title = paste(lowBound,'-',upperBound))


    topo_ggplots[[length(topo_ggplots) + 1]] <-   ggplot2::ggplot(interpTopo,
                                                         aes(x = x, y = y, fill = Voltage)
    ) +
      geom_raster(show.legend=F) +
      stat_contour(aes(z = Voltage),
                   colour = "black",
                   binwidth = 0.5) +
      theme_topo()+
      scale_fill_gradientn(colours = jet.colors(10),
                           limits = c(-2,2),
                           guide = FALSE, #"colourbar"
                           oob = scales::squish) +
      geom_path(data = maskRing,
                aes(x, y, z = NULL, fill =NULL),
                colour = "white",
                size = 15)+
      geom_point(data = means_by_electrodes,
                 aes(x, y),
                 size = 1)+
      geom_path(data = headShape,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5)+
      geom_path(data = nose,
                aes(x, y, z = NULL, fill = NULL),
                size = 1.5)+
      coord_equal()+theme(plot.title = element_text(hjust = 0.5))+
      labs(title = paste(lowBound,'ms to',upperBound, 'ms'))

  }





  #ggarrange(plotlist=topo_ggplots, nrow = 1, ncol = 6)

  # ggsave(filename=paste(plotname, "topo",'_',min_time,'-',max_time,'.pdf', sep=''), width = 20, height = 8)


  #ggarrange(plotlist=electro_ggplots, nrow = 4, ncol = 10)
  #ggsave(filename=paste("electro",'_',min_time,'-',max_time,'.pdf', sep=''))
  return(topo_ggplots)

}
