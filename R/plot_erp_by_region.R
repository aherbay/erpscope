#' Plot ERP by region of interest
#'
#' This function creates a plot file with 9 regions of interests displaying ERPs
#' for different conditions indicated in the input.
#'
#' @param infile Path to the input file
#' @param infile Path to the input file
#'
#' @return A matrix of the infile
#' @export


plot_erp_by_region<- function(  data,
                                conditionToPlot,
                                plotname = 'auto',
                                output_type = 'pdf',
                                color_palette =  c("#4DAF4A","#377EB8","#FF7F00","#984EA3","#000000"),
                                ant_levels= "anteriority_3l",
                                med_levels= "mediality_k",
                                vary="Voltage",
                                y_annot=6,
                                delta=1,
                                baseline= c(-500,-200)) {



  number_of_subjects <- length(unique(data$Subject))
  if(plotname == 'auto') {
    plotname = paste(Sys.Date(),"_ERPs_",deparse(substitute(data)),"_",conditionToPlot, sep="")
  }



  ggplot(data,aes_string(x= "Time", y= vary ,colour = conditionToPlot)) +
    scale_y_reverse() + theme_light() +
    stat_summary(fun.y = mean, geom = "line", size = .75) +
    labs(x = "Time (in ms)",
         y = bquote(paste("Voltage amplitude (", mu, "V): ", .(vary))),
        title = paste(vary,"by",conditionToPlot, " - ",plotname))+
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
    facet_grid( anteriority_3l ~ mediality_a)  # reformulate(med_levels,ant_levels)

  ggsave(filename=paste(plotname,'.pdf', sep=''), width = 22, height = 18)

}
