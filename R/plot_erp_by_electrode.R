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
                                  baseline =c(-500,-200),
                                  vary ="Voltage",
                                  plotname = 'auto',
                                  check_message = "no",
                                  conf_interval = "no",
                                  y_annot = 6, delta = 1
                                  ) {

  #data[,conditionToPlot] <- as.factor(data[,conditionToPlot])

  # check that data is there
  # checkDataFrame(data)

  # check that column with condition is presnt


    #length(unique(data$Subject))

  number_of_subjects <- length(unique(data$Subject))
  number_of_levels <- length(levels(data[,conditionToPlot]))
  title_text <- paste("You are about to plot ERPs for 9 electrodes for the condition", conditionToPlot , "with",number_of_levels,"levels and for",number_of_subjects,"subjects.")


  if(check_message == "yes") {
    choice <- menu(c("y", "n"), title= paste(title_text,"Do you want to continue?"))
  } else {

    message(title_text)
    choice  <- 1
  }

  # plot erp for 9 electrode basline format  pour la condition x à z nivaux du df x, avec x obs x sujets
  if(choice == 1) {

    if(plotname == 'auto') {
          plotname = paste(Sys.Date(),"_ERPs_",deparse(substitute(data)),"_",conditionToPlot, sep="")
    }


    dataToPlot <- subset(data, Electrode %in% electrodes_list)
    dataToPlot$Electrode <- factor(dataToPlot$Electrode, levels = electrodes_list)

    if(conf_interval == "no"){
          ggplot(dataToPlot ,aes_string(x= "Time", y= vary ,colour = conditionToPlot)) +
              scale_y_reverse() + theme_light() +
              stat_summary(fun = mean, geom = "line", size = .75) +
              labs(x = "Time (in ms)",
                  y = bquote(paste("Voltage amplitude (", mu, "V): ", .(vary))),
                  title = paste(vary,"by",conditionToPlot," - dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects"))+
              scale_color_manual(values=color_palette)+
              scale_x_continuous(breaks=seq(-500,900,100))+
              geom_vline(xintercept = 0,linetype = "solid" )+
              geom_hline(yintercept = 0)+

              geom_vline(xintercept = 400, linetype = "dotted")+
              annotate(geom = "text", x = 370, y = y_annot, label = "400ms", color = "dark grey", angle = 90)+


              geom_vline(xintercept = -450, linetype = "longdash")+

              # prime annotation
              annotate(geom = "text", x = -420, y = y_annot, label = "Prime", angle = 90)+
              annotate("rect", xmin = -450, xmax = -250, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+

              # target annotation
              annotate(geom = "text", x = 30, y = y_annot, label = "Target", angle = 90)+
              annotate("rect", xmin = 0, xmax = 200, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+

              # baseline annotation
              annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-0.5, ymax=0.5, alpha = .2)+
              annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = "Baseline", color = "red",size = 3)+

              facet_wrap( ~ Electrode , nrow = 3, ncol = 3 )

    } else {
        ggplot(dataToPlot ,aes_string(x= "Time", y= vary ,colour = conditionToPlot)) +
              scale_y_reverse() + theme_light() +
              stat_summary(fun = mean, geom = "line", size = .75) +
              stat_summary(data = dataToPlot,fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.3, aes(fill = conditionToPlot))+ # CI ribbon
              labs(x = "Time (in ms)",
                  y = bquote(paste("Voltage amplitude (", mu, "V): ", .(vary))),
                  title = paste(vary,"by",conditionToPlot," - dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects"))+
              scale_color_manual(values=color_palette)+
              scale_x_continuous(breaks=seq(-500,900,100))+
              geom_vline(xintercept = 0,linetype = "solid" )+
              geom_hline(yintercept = 0)+

              geom_vline(xintercept = 400, linetype = "dotted")+
              annotate(geom = "text", x = 370, y = y_annot, label = "400ms", color = "dark grey", angle = 90)+


              geom_vline(xintercept = -450, linetype = "longdash")+

              # prime annotation
              annotate(geom = "text", x = -420, y = y_annot, label = "Prime", angle = 90)+
              annotate("rect", xmin = -450, xmax = -250, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+

              # target annotation
              annotate(geom = "text", x = 30, y = y_annot, label = "Target", angle = 90)+
              annotate("rect", xmin = 0, xmax = 200, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+

              # baseline annotation
              annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-0.5, ymax=0.5, alpha = .2)+
              annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = "Baseline", color = "red",size = 3)+

              facet_wrap( ~ Electrode , nrow = 3, ncol = 3 )

    }


      ggsave(filename=paste(plotname,'.',output_type, sep=''), width = 22, height = 18)


    } # end of choice

}


# plot_erp_by_electrode (data=datatest, conditionToPlot = "MM_RAW" )


# check the conditionToPlot is a factor
# check if files already exists
# check output_type is in "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
# scale_x_continuous(breaks=seq(-500,900,100))
# manage annotations
