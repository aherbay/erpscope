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



plot_erp_by_electrode<- function( data,
                                  conditionToPlot,
                                  electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
                                  output_type = 'pdf',
                                  color_palette =  c("#4DAF4A","#377EB8","#FF7F00","#984EA3","#000000")  ,
                                  baseline =c(-500,-200),
                                  vary ="Voltage",
                                  plotname = 'auto',
                                  y_annot = 6, delta = 1
                                  ) {

  #data[,conditionToPlot] <- as.factor(data[,conditionToPlot])

  # check that data is there
  # checkDataFrame(data)

  # check that column with condition is presnt


    #length(unique(data$Subject))
  choice <- menu(c("y", "n"), title="You are about to plot an ERP graph?")

  if(choice == 1) {

    if(plotname == 'auto') {
          plotname = paste(Sys.Date(),"_ERPs_",deparse(substitute(data)),"_",conditionToPlot, sep="")
    }


    dataToPlot <- subset(data, Electrode %in% electrodes_list)
    dataToPlot$Electrode <- factor(dataToPlot$Electrode, levels = electrodes_list)

    ggplot(dataToPlot ,aes_string(x= "Time", y= vary ,colour = conditionToPlot)) +
              scale_y_reverse() + theme_light() +
              stat_summary(fun.y = mean, geom = "line", size = .75) +
              labs(x = "Time (in ms)",
                  y = bquote(paste("Voltage aplitude (", mu, "V): ", .(vary))),
                  title = paste(vary,"by",conditionToPlot))+
              scale_color_manual(values=color_palette)+
              scale_x_continuous(breaks=seq(-500,900,100))+
              geom_vline(xintercept = 0,linetype = "solid" )+
              geom_hline(yintercept = 0)+
              geom_vline(xintercept = 400, linetype = "dotted")+
              geom_vline(xintercept = -450, linetype = "longdash")+
              annotate(geom = "text", x = -420, y = y_annot, label = "Prime", angle = 90)+
              annotate(geom = "text", x = 30, y = y_annot, label = "Target", angle = 90)+
              annotate(geom = "text", x = 370, y = y_annot, label = "400ms", color = "dark grey", angle = 90)+
              annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-0.5, ymax=0.5, alpha = .2)+
              annotate("rect", xmin = -450, xmax = -250, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+
              annotate("rect", xmin = 0, xmax = 200, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+
              annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = "Baseline", color = "red",size = 3)+
              facet_wrap( ~ Electrode , nrow = 3, ncol = 3 )


      ggsave(filename=paste(plotname,'.',output_type, sep=''), width = 22, height = 18)


    } # end of choice

}


# plot_erp_by_electrode (data=datatest, conditionToPlot = "MM_RAW" )

 # check the conditionToPlot is a factor
# check output_type is in "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"

