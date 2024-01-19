plot_topoplots_by_custom_TW <-  function (data_for_map,
                                          tw_array ,
                                          plotname,
                                          topoplots_scale = 'auto',
                                          data_to_display = "voltage_difference",
                                          levelA,
                                          levelB,
                                          t_test_threshold,
                                          maps_color_palette = 'auto') {

  #electrodeLocs <- readRDS("electrodeLocs_51elec.RDS")
  electrodeLocs <- locations_51_electrodes

  electrodeLocs <- rename(electrodeLocs,   Electrode = electrode)
  electrodeLocs <- subset(electrodeLocs, !(Electrode %in% c("AFz","PO5","PO6") ))

  #print(head(electrodeLocs))

  # base elements for topo



  if (maps_color_palette == 'auto') {
      jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
      maps_color_palette <- jet.colors(10)
  } else if (maps_color_palette == 'grey_gradient') {
      grey.colors <- colorRampPalette(c("grey", "black"))
      maps_color_palette <- grey.colors(10)
  }

  if(length(topoplots_scale) == 2) {
    # topoplots_scale is a custom vector
  } else if(topoplots_scale == 'auto') {

        topoplots_scale <- c() # transforming topoplots_scale into a vector


        means_by_electrodes_for_eachTime <- data_for_map %>%
          group_by(Electrode,Time) %>%
          summarise(Voltage= mean(Voltage) )

        min_value <- abs( min(means_by_electrodes_for_eachTime$Voltage) )
        max_value <- abs( max(means_by_electrodes_for_eachTime$Voltage) )
        #print(min_value)
        #print(max_value)

         higher_value <- round(max(min_value,max_value))
         topoplots_scale[1] <- -higher_value
         topoplots_scale[2] <- higher_value
         #print( topoplots_scale[1] )
         #print( topoplots_scale[2] )

      }




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

  if (data_to_display %in% c("t_test_t_value", "t_test_p_value")) {
    data_for_map_summarized <- data_for_map %>% group_by(Subject,Electrode,Time,Condition) %>% summarise(Voltage= mean(Voltage) )
    #print(head(data_for_map_summarized))
  }


  # if(topoplots_scale == 'auto') {
  #   topoplots_scale[1] <- 0
  #   topoplots_scale[2] <- 0
  # }

  for( tw_index in 1:length(tw_array)) {



    lowBound <- tw_array[[tw_index]][1]
    upperBound <- tw_array[[tw_index]][2]

    message(paste('--> Generating Voltage Map for Time Window: ',lowBound,"ms to",upperBound, "ms"))


      if(data_to_display == "voltage_difference") {

        title_legend <- "Voltage Difference"

        # compute mean diff voltage for each electrode for the given time window
        means_by_electrodes <- data_for_map %>%
          filter ( Time > lowBound  & Time < upperBound ) %>%
          drop_na() %>%
          group_by(Electrode)%>%
          summarise(Voltage= mean(Voltage) )

        # preprocess the means voltage

        #print(head(means_by_electrodes))

        # to solve : 1: Column `Electrode` joining factor and character vector, coercing into character vector
        #electrodeLocs$Electrode <- as.factor(electrodeLocs$Electrode)
        #print(unique(data$Electrode))
        means_by_electrodes <- means_by_electrodes %>% right_join(electrodeLocs, by = "Electrode") %>% filter(Electrode  %in% unique(data_for_map$Electrode))
        #print(head(means_by_electrodes))

        # if(topoplots_scale == 'auto') {
        #     if(topoplots_scale[1] >  min(means_by_electrodes$Voltage) ) topoplots_scale[1] <- min(means_by_electrodes$Voltage)
        #     if(topoplots_scale[1] <  max(means_by_electrodes$Voltage) ) topoplots_scale[1] <- max(means_by_electrodes$Voltage)
        # }



    } else if (data_to_display == "t_test_t_value") {

        title_legend <- "t-test t-value"

        means_by_electrodes <- data_for_map_summarized %>%
          filter ( Time > lowBound  & Time < upperBound ) %>%
          drop_na() %>%
          group_by(Subject,Electrode,Condition) %>%
          summarise(
            Voltage= mean(Voltage)
          ) %>%
          group_by(Electrode) %>%
          summarize(
            `Voltage` = t.test(   # Should change that for p value and replace voltage below by y_value_to_plot
              Voltage[Condition == rlang::quo_text(levelA)],
              Voltage[Condition == rlang::quo_text(levelB)], paired = TRUE
            )$statistic
          )

        means_by_electrodes <- means_by_electrodes %>% right_join(electrodeLocs, by = "Electrode") %>% filter(Electrode  %in% unique(data_for_map$Electrode))


    } else if (data_to_display == "t_test_p_value") {

      title_legend <- "t-test p-value"

        means_by_electrodes <- data_for_map_summarized %>%
          filter ( Time > lowBound  & Time < upperBound ) %>%
          drop_na() %>%
          group_by(Subject,Electrode,Condition) %>%
          summarise(
            Voltage= mean(Voltage)
          ) %>%
          group_by(Electrode)%>%
          summarize(
            `Voltage` = t.test(     # Should change that for p value and replace voltage below by y_value_to_plot
              Voltage[Condition == rlang::quo_text(levelA)],
              Voltage[Condition == rlang::quo_text(levelB)], paired = TRUE
            )$p.value
          )

        means_by_electrodes$Voltage <- means_by_electrodes$Voltage +1
        means_by_electrodes <- means_by_electrodes %>% right_join(electrodeLocs, by = "Electrode") %>% filter(Electrode  %in% unique(data_for_map$Electrode))


    } else { stop("Invalid data_to_display in the function plot_topoplots_by_custom_TW")}



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
    # geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
    # theme_topo()+
    # coord_equal()+
    # labs(title = paste(lowBound,'-',upperBound))
    if (data_to_display == "t_test_p_value") {
          interpTopo$Voltage <- interpTopo$Voltage - 1
          topo_ggplots[[length(topo_ggplots) + 1]] <-   ggplot2::ggplot(interpTopo,
                                                               aes(x = x, y = y, fill = Voltage)
          ) +
            geom_raster(show.legend=F) +
            stat_contour(aes(z = Voltage),
                         colour = "black",
                         binwidth = 0.5) +
            theme_topo()+

            # version 1 with more gradient approach
             # scale_fill_steps2(midpoint = t_test_threshold, limits = c(0,0.10), oob=scales::squish, mid = scales::muted("red"), high = "white", low = scales::muted("green", guide = FALSE) +
            # version 2 with more stats threshold approach

            #scale_fill_steps2(midpoint = t_test_threshold, limits = c(0,0.10), oob=scales::squish,mid = "#99cc33", high = "#339900", low = ," #ffcc00",  breaks= c(0.01,0.05))+ #, breaks= c(0.01,0.05,0.05)guide = FALSE,

            # version 3 essai"#297D00","#3BB300","#3390FF"
            scale_fill_stepsn( limits = c(0,0.07),colors=c("#28A500","#35DA24","#00D1FF" ) ,  breaks= c(0.01,0.05) , values = scales::rescale(c(0.01,0.05), c(0,1)) )+



            #scale_fill_stepsn(  oob=scales::squish, breaks= c(0,0.001,0.01,0.05,0.05,0.10,1), colours =  c("#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B"),guide = FALSE)+ #guide = FALSE,, limits = c(0,1)
            #scale_fill_stepsn(colors = c("#D73027", "#1A9850") , breaks= c(t_test_threshold)   )+ # pb it changes from on VM to another
            #scale_fill_steps2(midpoint = t_test_threshold, limits = c(0,0.10), breaks = c(0.001, 0.01, 0.05), mid = scales::muted("red"), high = "white", low = "red" , guide = FALSE)+ #, breaks= c(0,0.001,0.01,0.05,0.05,0.10,1)

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
            coord_equal()+theme(plot.title = element_text(hjust = 0.5,vjust = -4))+
            labs(title = paste(lowBound,'ms to',upperBound, 'ms'))

    }else {

      topo_ggplots[[length(topo_ggplots) + 1]] <-   ggplot2::ggplot(interpTopo,
                                                                    aes(x = x, y = y, fill = Voltage)
      ) +
        geom_raster(show.legend=F) +
        stat_contour(aes(z = Voltage),
                     colour = "black",
                     binwidth = 0.5) +
        theme_topo()+
        scale_fill_gradientn(colours = maps_color_palette,
                             limits = c(topoplots_scale[1],topoplots_scale[2]),
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
        coord_equal()+theme(plot.title = element_text(hjust = 0.5,vjust = -4))+
        labs(title = paste(lowBound,'ms to',upperBound, 'ms'))

      }


     # last plot to get legend
     if(tw_index == length(tw_array) ){


       if(data_to_display == "t_test_p_value" ) {

         #print(t_test_threshold)
         topo_forlegend <-   ggplot2::ggplot(interpTopo,
                                                                       aes(x = x, y = y, fill = Voltage)
         ) +
           geom_raster(show.legend=F) +
           stat_contour(aes(z = Voltage),
                        colour = "black",
                        binwidth = 0.5) +
           theme_topo()+
           theme(legend.position="bottom",
                 legend.background = element_rect(fill = "transparent", colour = "transparent"))+
           #scale_fill_steps2(midpoint = t_test_threshold, limits = c(0,0.10), mid = scales::muted("red"), high = "white", low = "red")+ #, breaks= c(0,0.001,0.01,0.05,0.05,0.10,1)
           #scale_fill_stepsn( breaks= c(0,0.001,0.01,0.05,0.05,0.10,1), colours =  c("#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B"),guide = FALSE)+ #guide = FALSE,, limits = c(0,1)
           #scale_fill_steps2(midpoint = t_test_threshold, limits = c(0,0.10),oob=scales::squish, mid = scales::muted("red"), high = "white", low = scales::muted("green") , guide = FALSE)+ #, breaks= c(0,0.001,0.01,0.05,0.05,0.10,1)
           scale_fill_stepsn( limits = c(0,0.07), oob=scales::squish,colors=c("#28A500","#35DA24","#00D1FF") ,  breaks= c(0.01,0.05) , values = scales::rescale(c(0.01,0.05), c(0,1)),
                              guide = guide_coloursteps(even.steps = TRUE,show.limits = TRUE )  )+ # ffcc00 c("#339900","#99cc33","#3390FF") FFE033 FAE7D2 purple-blue 28A500

           #scale_fill_stepsn(colors = c("#FC8D59", "#FEE08B","#D9EF8B", "#91CF60", "#1A9850") ,
            #                 breaks = c(0.001, 0.01, 0.05, 0.10))+
           guides(fill = guide_colourbar(barwidth = 10))+
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
           coord_equal()+theme(plot.title = element_text(hjust = 0.5,vjust = -4))+
           labs(title = paste(lowBound,'ms to',upperBound, 'ms'), fill = paste(title_legend,"    "))



       } else {

         #highest_abs_viltage <-  max(abs(topoplots_scale[1]),abs(topoplots_scale[2]))
         #topoplots_scale[1] <-

         topo_forlegend <-   ggplot2::ggplot(interpTopo,
                                             aes(x = x, y = y, fill = Voltage)
         ) +
           geom_raster(show.legend=F) +
           stat_contour(aes(z = Voltage),
                        colour = "black",
                        binwidth = 0.5) +
           theme_topo()+
           theme(legend.position="bottom",
                 legend.background = element_rect(fill = "transparent", colour = "transparent"))+
           scale_fill_gradientn(colours = maps_color_palette,
                                limits = c(topoplots_scale[1],topoplots_scale[2]),
                                oob = scales::squish,
                                breaks= seq(round(topoplots_scale[1]),round(topoplots_scale[2]), round(topoplots_scale[2]/3) ),
                                labels= seq(round(topoplots_scale[1]),round(topoplots_scale[2]), round(topoplots_scale[2]/3)    ) )  +
           guides(fill = guide_colourbar(barwidth = 10))+
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
           coord_equal()+theme(plot.title = element_text(hjust = 0.5,vjust = -2))+
           labs(title = paste(lowBound,'ms to',upperBound, 'ms'), fill = paste(title_legend,"    "))


       }



        # legend <- ggpubr::get_legend( topo_forlegend )
       legend <- ggpubr::get_legend( topo_forlegend )
       #topolegend <- cowplot::plot_grid(NULL, legend, ncol=1)
       topolegend <- ggpubr::as_ggplot(legend)
       #saveRDS(topolegend, "legend.RDS")

     }

  }


  topo_ggplots_with_legend <- list(topo_ggplots,topolegend)



  #ggarrange(plotlist=topo_ggplots, nrow = 1, ncol = 6)

  # ggsave(filename=paste(plotname, "topo",'_',min_time,'-',max_time,'.pdf', sep=''), width = 20, height = 8)


  #ggarrange(plotlist=electro_ggplots, nrow = 4, ncol = 10)
  #ggsave(filename=paste("electro",'_',min_time,'-',max_time,'.pdf', sep=''))
  return(topo_ggplots_with_legend)

}
