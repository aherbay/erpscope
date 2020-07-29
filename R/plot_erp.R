#' Plot ERP for 9 electrodes of interest
#'
#' This function creates a plot file with 9 electrodes of interests displaying ERPs
#' for different conditions. It will need a loaded dataframe with your EEG data
#' and a column indicating the condition to display.
#' It assumes that there is a column named Voltage with your voltage values.
#' Default values are provided for electrodes but it can be customized.
#'
#' @param file dataframe containing eeg data
#' @param conditionToPlot column of the dataframe witht the condition and levels to plot
#' @param electrodes_list vector of Electrodes names to plot (between quotes)
#' @return A PDF file containing the ERP plots
#' @export

plot_erp <- function(             data,
                                  conditionToPlot,
                                  electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
                                  baseline = c(-2450,-2250),
                                  color_palette =  c("#4DAF4A", "#EA2721","#377EB8","#FF7F00","#984EA3","#000000","#5c5c5c", "#945D25", "#FF748C", "#2E692C"),
                                  output_type = 'pdf',
                                  adjusted_baseline = FALSE,
                                  time_labels_interval = 200,
                                  plotname = 'auto',
                                  show_check_message = FALSE,
                                  show_conf_interval = FALSE,
                                  custom_labels = list(),
                                  labels_vertical_position = 'auto',
                                  labels_height = 'auto',
                                  vary ="Voltage",
                                  background = "grid",
                                  line_thickness = 0.75 # c(0.75, 1 , 1.25, 1.5),
                                  linetype = 'solid' # c('solid', 'dotted , 'dashed','longdash')
                                  ) {



    ## Check arguments

    conditionToPlot_enq <- rlang::enquo(conditionToPlot)
    conditionToPlot <- rlang::quo_text(conditionToPlot_enq)
    #print(conditionToPlot)

    if(tibble::is_tibble(data))
    {
      data <-  as.data.frame(data)
      message("Converting data tibble as traditional dataframe")

    }

    if(!(conditionToPlot %in% colnames(data)))
    {
      stop(paste("There is no column",conditionToPlot,"in the dataframe",deparse(substitute(data)) ))
    }

   if(!(is.factor(data[,conditionToPlot])) ) {
    data[,conditionToPlot] <- as.factor(data[,conditionToPlot])
    message("Converting condition to plot as a factor")
   }

    #message("Dropping absent levels")
    #data <- droplevels(data)


    if(!(is.factor(data[,"Electrode"])) ) {
      data[,"Electrode"] <- as.factor(data[,"Electrode"])
      message("Converting Electrode as a factor")
    }


    df_electrodes <- unique(data$Electrode)
    for(current_elec in 1:length(electrodes_list)){
      if(!(electrodes_list[current_elec] %in% df_electrodes)) {
         stop(paste("Electrode",electrodes_list[current_elec] ,"is not in the data"))
      }
    }




    if(("Subject" %in% colnames(data)))
    {

      if(!(is.factor(data[,"Subject"])) ) {
        data[,"Subject"] <- as.factor(data[,"Subject"])
        message("Converting Subject as a factor")
      }

      number_of_subjects <- length(unique(data$Subject))

    }else {
      number_of_subjects <- "unknown"
      message(paste("No column Subject in the dataframe",deparse(substitute(data)),"- unknown value will be mentionned" ))
    }

  number_of_levels <- length(levels(data[,conditionToPlot]))

  if(length(color_palette) < number_of_levels) { stop(paste("Please provide more colors in your palette: currently",length(color_palette),"colors to plot",number_of_levels , "levels")) }

  # check if baseline has two values, then that baseline[1] is > tmin and < tmax, same for baseline[2], check that baseline[1]< baseline[2]
  if(length(baseline) != 2) {
    stop(paste("Provided baseline ",baseline,"is not valid"))
  }

  init_message <- paste("You are about to plot ERPs for",length(electrodes_list), "electrodes for the condition", conditionToPlot , "with",number_of_levels,"levels and for",number_of_subjects,"subjects.")

  # check a few things
  # is color accessible?
  # check if length(electrodes_list) is 9 or 12



  if(show_check_message == TRUE) {
    choice <- menu(c("y", "n"), title= paste(init_message,"Do you want to continue?"))
  } else {
    message(init_message)
    choice  <- 1
  }


  # plot erp for 9 electrode basline format  pour la condition x à z nivaux du df x, avec x obs x sujets
  if(choice == 1) {
    #print("A")
    if(plotname == 'auto') {
          plotname = paste(Sys.Date(),"_ERPs_",deparse(substitute(data)),"_",conditionToPlot, sep="")
    }
    #print("B")
    plot_filename <- paste(plotname,'.',output_type, sep='')
    #print("C")
    t_start <- Sys.time()
    #print("D")

    message(paste(Sys.time()," - Beginning to plot ERP in",plot_filename))

    if(file.exists(plot_filename)) message("File already exists! Overwriting it")
    #print("E")


    time_min  <- ((min(data$Time) %/% time_labels_interval) -1) * time_labels_interval
    time_max  <- (max(data$Time) %/% time_labels_interval) * time_labels_interval
    numberOfRows <- length(electrodes_list)/3
    #message(paste("numberOfRows",numberOfRows))

    dataToPlot <- subset(data, Electrode %in% electrodes_list)
    dataToPlot$Electrode <- factor(dataToPlot$Electrode, levels = electrodes_list)


    if(adjusted_baseline == TRUE) {
      if(length(baseline) != 2) {
        stop(paste("Provided baseline ",baseline,"is not valid"))
      }else{
        dataToPlot <- baseline_correction(dataToPlot,conditionToPlot,baseline)
        vary <- "RebaselinedVoltage"
      }

    }

    if(show_conf_interval == TRUE) {

      tempo <- ggplot(dataToPlot ,aes_string(x= "Time", y= vary ,colour = conditionToPlot,fill = conditionToPlot)) +
                  scale_y_reverse() +
                  stat_summary(fun = mean, geom = "line", size = line_thickness)+
                  stat_summary(fun.data = mean_cl_normal,geom = "ribbon",alpha = 0.3 , colour=NA)+
                  scale_color_manual(values=color_palette)+
                  scale_fill_manual(values=color_palette)



    } else {

      tempo <- ggplot(dataToPlot ,aes_string(x= "Time", y= vary ,colour = conditionToPlot)) +
                  scale_y_reverse() +
                  stat_summary(fun = mean, geom = "line", size = line_thickness)+
                  scale_color_manual(values=color_palette)

    }

    # add background

    if( background == "white") {
      tempo <- tempo + theme_classic()
    } else if ( background == "dark") {
      tempo <- tempo + theme_dark()
    } else {
      tempo <- tempo + theme_light()
    }


    # add axis, baseline and legend


          tempo <- tempo +
                  labs(x = "Time (in ms)",
                      y = bquote(paste("Voltage amplitude (", mu, "V): ", .(vary))),
                      title = paste("ERP: ", vary,"by",conditionToPlot),
                      subtitle = paste(  Sys.Date(), paste("- Baseline:[",baseline[1],"ms;",baseline[2],"ms]",sep="") ,"- dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects"),
                      caption = "Generated with ERPscope")+
                  # ticks on x axis
                  scale_x_continuous(breaks=seq(time_min,time_max,time_labels_interval))+
                  # axis
                  geom_vline(xintercept = 0,linetype = "solid" )+
                  geom_hline(yintercept = 0)+
                  # baseline annotation
                  annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-1, ymax=1, alpha = .4,fill = "red")+
                  annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = "Baseline", color = "red",size = 2)



            #print(ggplot_build(tempo)$layout$panel_scales_y[[1]]$range$range)


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

            #print(ggplot_build(tempo)$layout$panel_scales_y[[1]]$range$range)


            if(length(custom_labels) != 0) {

              if(labels_vertical_position == "auto" | labels_height == "auto" ){

                  range <- ggplot_build(tempo)$layout$panel_scales_y[[1]]$range$range

                  y_min <-range[1]
                  y_max <-range[2]


                  if(labels_vertical_position == "auto"){
                    labels_vertical_position =  y_min + (y_max - y_min) / 32
                  }
                  if(labels_height == "auto"){
                    labels_height = (y_max - y_min)/16
                  }
              }

              for(i in 1:length(custom_labels)) {

                tempo =  tempo + geom_vline(xintercept = custom_labels[[i]][[1]], linetype = "longdash") +
                  annotate(geom = "text", x= (custom_labels[[i]][[1]]+ custom_labels[[i]][[2]])/2, y = labels_vertical_position, label = custom_labels[[i]][[3]], angle = 0) +
                  annotate("rect", xmin = custom_labels[[i]][[1]], xmax = custom_labels[[i]][[2]], ymin= labels_vertical_position - labels_height, ymax=labels_vertical_position +labels_height, alpha = .2)



              }

            }



      message("Saving plot to file")


      ggsave(tempo, filename=paste(plotname,plot_filename,sep=''), width = 22, height = 18)
      t_end <- Sys.time()
      message(paste(Sys.time()," - End - Generating the file took",  substring(round(   difftime(t_end,t_start,units="mins")  , 2),1 ),"mins"))



    } # end of choice

}



#                   green     red        blue     orange    purple   black      grey        brown      pink       dark green

# check output_type is in "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
