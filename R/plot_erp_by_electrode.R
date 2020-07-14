#' Plot ERP for 9 electrodes of interest
#'
#' This function creates a plot file with 9 electrodes of interests displaying ERPs
#' for different conditions. It will need a loaded dataframe with your EEG data
#' and a column indicating the condition to display.
#' It assumes that there is a column named Voltage with your voltage values.
#' Default values are provided for electrodes but it can be customized.
#'
#' @param file dataframe containing
#' @param conditionToPlot column of the dataframe witht the condition to plot
#'
#' @return A PDF file containing the ERP plots
#' @export


plot_erp_by_electrode<- function( data,
                                  conditionToPlot,
                                  electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
                                  output_type = 'pdf',
                                  color_palette =  c("#4DAF4A","#377EB8","#FF7F00","#984EA3","#000000")  ,
                                  baseline = c(-2450,-2250),
                                  correction = FALSE,
                                  tick_distance = 200,
                                  vary ="Voltage",
                                  plotname = 'auto',
                                  check_message = FALSE,
                                  conf_interval = FALSE,
                                  y_annot = 6, delta = 1,
                                  rectangles = list(list(-2250,-1600,"N1"),list(-1500,-850,"BA/BEI"),list(-750,-100,"N2"),list(0,650,"Verb"))
                                  ) {

  #data[,conditionToPlot] <- as.factor(data[,conditionToPlot])

  # check that data is there
  # checkDataFrame(data)

  # check that column with condition is presnt


    #length(unique(data$Subject))

  number_of_subjects <- length(unique(data$Subject))
  number_of_levels <- length(levels(data[,conditionToPlot]))
  init_message <- paste("You are about to plot ERPs for",length(electrodes_list), "electrodes for the condition", conditionToPlot , "with",number_of_levels,"levels and for",number_of_subjects,"subjects.")

  # check a few things
  # is condition a factor?
  # is color accessible?
  # is length(color_palette) >= levels(conditionToPlot)
  # check if length(electrodes_list) is 9 or 12




  if(check_message == TRUE) {
    choice <- menu(c("y", "n"), title= paste(init_message,"Do you want to continue?"))
  } else {

    message(init_message)
    choice  <- 1
  }


  # plot erp for 9 electrode basline format  pour la condition x à z nivaux du df x, avec x obs x sujets
  if(choice == 1) {

    if(plotname == 'auto') {
          plotname = paste(Sys.Date(),"_ERPs_",deparse(substitute(data)),"_",conditionToPlot, sep="")
    }

    time_min  <- ((min(data$Time) %/% tick_distance) -1) * tick_distance
    time_max  <- (max(data$Time) %/% tick_distance) * tick_distance
    numberOfRows <- length(electrodes_list)/3
    #message(paste("numberOfRows",numberOfRows))

    dataToPlot <- subset(data, Electrode %in% electrodes_list)
    dataToPlot$Electrode <- factor(dataToPlot$Electrode, levels = electrodes_list)


    if(correction == TRUE) {
      dataToPlot <- baseline_correction(dataToPlot,conditionToPlot,baseline)
      vary <- "RebaselinedVoltage"
    }

    if(conf_interval == TRUE) {

      tempo <- ggplot(dataToPlot ,aes_string(x= "Time", y= vary ,colour = conditionToPlot,fill = conditionToPlot)) +
                  scale_y_reverse() + theme_light() +
                  stat_summary(fun = mean, geom = "line", size = .75)+
                  stat_summary(fun.data = mean_cl_normal,geom = "ribbon",alpha = 0.3 , colour=NA)+
                  scale_color_manual(values=color_palette)+
                  scale_fill_manual(values=color_palette)



    } else {

      tempo <- ggplot(dataToPlot ,aes_string(x= "Time", y= vary ,colour = conditionToPlot)) +
                  scale_y_reverse() + theme_light() +
                  stat_summary(fun = mean, geom = "line", size = .75)+
                  scale_color_manual(values=color_palette)

    }


          tempo <- tempo +
                  labs(x = "Time (in ms)",
                      y = bquote(paste("Voltage amplitude (", mu, "V): ", .(vary))),
                      title = paste("ERP: ", vary,"by",conditionToPlot),
                      subtitle = paste(  Sys.Date(), paste("- Baseline:[",baseline[1],"ms;",baseline[2],"ms]",sep="") ,"- dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects"),
                      caption = "Generated with ERPscope")+
                  # ticks on x axis
                  scale_x_continuous(breaks=seq(time_min,time_max,tick_distance))+
                  # axis
                  geom_vline(xintercept = 0,linetype = "solid" )+
                  geom_hline(yintercept = 0)+
                  # baseline annotation
                  annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-1, ymax=1, alpha = .4,fill = "red")+
                  annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = "Baseline", color = "red",size = 2)



            print(ggplot_build(tempo)$layout$panel_scales_y[[1]]$range$range)

            if(length(rectangles) != 0) {

              for(i in 1:length(rectangles)) {

                tempo =  tempo + geom_vline(xintercept = rectangles[[i]][[1]], linetype = "longdash") +
                                  annotate(geom = "text", x= (rectangles[[i]][[1]]+ rectangles[[i]][[2]])/2, y = y_annot, label = rectangles[[i]][[3]], angle = 0) +
                                  annotate("rect", xmin = rectangles[[i]][[1]], xmax = rectangles[[i]][[2]], ymin= y_annot - delta, ymax=y_annot +delta, alpha = .2)



              }

            }


            tempo <- tempo +  facet_wrap( ~ Electrode , nrow = numberOfRows, ncol = 3, scales='free_x' ) +
              guides(colour = guide_legend(override.aes = list(size = 2))) +
              theme(  strip.text.x = element_text( size = 16, color = "black", face = "bold" ),
                      strip.background = element_rect( fill="white", color=NA),
                      legend.position="bottom",
                      plot.title = element_text(size = 24, face = "bold",hjust = 0.5),
                      plot.subtitle = element_text(size = 18 ,hjust = 0.5),
                      legend.title = element_text(size = 16),
                      legend.text = element_text(size = 15),
                      legend.spacing.x = unit(0.8, "cm"),
                      legend.key.width = unit(3, "lines"),
                      #legend.key.size = unit(2, "lines"),
                      #legend.key.height  = unit(15, "lines"),
                      axis.title=element_text(size=18)

                    )


      message("Saving plot to file")
      print(ggplot_build(tempo)$layout$panel_scales_y[[1]]$range$range)


      ggsave(tempo, filename=paste(plotname,'.',output_type, sep=''), width = 22, height = 18)


    } # end of choice

}


# plot_erp_by_electrode (data=datatest, conditionToPlot = "MM_RAW" )


# check the conditionToPlot is a factor
# check if files already exists
# check output_type is in "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
# scale_x_continuous(breaks=seq(-500,900,100))
# manage annotations
