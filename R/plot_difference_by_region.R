
#' Difference Plots for 9 ROI
#'
#' This function creates a plot file with 9 electrodes of interests displaying ERPs
#' for different conditions. It will need a loaded dataframe with your EEG data
#' and a column indicating the condition to display.
#' It assumes that there is a column named Voltage with your voltage values.
#' Default values are provided for electrodes but it can be customized.
#'
#' @param file dataframe containing
#' @param conditionToPlot column of the dataframe with the condition to plot
#' @return A PDF file containing the Difference plots by region
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @export
#'
plot_difference_by_region  <- function( data,
          plotname,
          conditionToPlot = MM_RAW,
          levelA = semMM_RAW ,
          levelB = consistent,
          color_palette,
          ant_levels= anteriority_3l,
          med_levels= mediality_a,
          vary= Voltage,
          group_var,
          show_group_obs ,
          y_annot=6,
          delta=1,
          baseline= c(-500,-200),
          time_windows = list(c(-250,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
          topoplots_scale = c(-2,2),
          plot_title=" ") {

  conditionToPlot_enq <- rlang::enquo(conditionToPlot)
  levelA_enq <- rlang::enquo(levelA)
  levelB_enq <- rlang::enquo(levelB)
  vary_enq <- rlang::enquo(vary)
  group_var_enq <- rlang::enquo(group_var)
  med_levels_enq <- rlang::enquo(med_levels)
  ant_levels_enq <- rlang::enquo(ant_levels)
  print("Starting")


  number_of_subjects <- length(unique(data$Subject))
  number_of_levels <- length(levels(data[,rlang::quo_text(conditionToPlot_enq)]))

  subject_dataset_info <-

  if(plotname == 'auto') {
      plotname = paste(Sys.Date(),"_",deparse(substitute(data)),"_",number_of_subjects,"PPTS_ERP_DIFF_",rlang::quo_text(conditionToPlot_enq),"_",rlang::quo_text(levelA_enq),"-", rlang::quo_text(levelB_enq) ,sep="")
  }

  data_reduced <- dplyr::select(data, !! group_var_enq, Time, Electrode , !! vary_enq ,!! conditionToPlot_enq,!! ant_levels_enq,!! med_levels_enq)
  print("Data selected")
  data_reduced <- filter(data_reduced, !! conditionToPlot_enq %in% c(rlang::quo_text(levelA_enq), rlang::quo_text(levelB_enq)) ) %>% droplevels()
  print("Data reduced")
  #print(levels(data_reduced$MM_RAW))
  #print(head(data_reduced))

  data_diff <- data_reduced %>%
    group_by( !! group_var_enq, Time, Electrode,!! ant_levels_enq,!! med_levels_enq, !! conditionToPlot_enq) %>%
    summarise(mean_Voltage = mean(!! vary_enq))
  print("Data diff")
  #print(head(data_diff))

  if ( rlang::quo_text(group_var_enq)  == "Trigger_code") {

    print("Entering Trigger_code section")
    # create the two non_consistent and consistent pairs
    #non_consistent <- data_diff %>% filter( !! conditionToPlot_enq != rlang::quo_text(levelB_enq))
    #consistent_means <- data_diff %>% filter( !! conditionToPlot_enq == rlang::quo_text(levelB_enq))


    non_consistent <- subset( data_diff, MM_RAW != "consistent")
    consistent_means <- subset( data_diff, MM_RAW == "consistent")


    rm(data_diff)

    print("Subset non_consistent and consistent means created")

    # compute Trigger_code_cons  100+ (181 %%10)*10 + (181%%10)
    non_consistent$Trigger_code_cons <- 100 + (non_consistent$Trigger_code%%10)*10 + (non_consistent$Trigger_code%%10)
    # old erroneous way non_consistent$Trigger_code_cons <- (non_consistent$Trigger_code%/%10)*10 + (non_consistent$Trigger_code%/%10)%%10
    non_consistent$MM_RAW <- NULL

    print("Trigger_code_cons computed")

    # rename trigger_code and Voltage for future merge
    consistent_means$MM_RAW <- NULL
    #consistent_means <- rename(consistent_means,   Trigger_code_cons = Trigger_code)
    #consistent_means <- rename(consistent_means,   consistentMeanVoltage = mean_Voltage)
    # rename trigger_code and Voltage for future merge
    consistent_means <- rename_(consistent_means, .dots = setNames("Trigger_code", "Trigger_code_cons"))
    consistent_means <- rename_(consistent_means, .dots = setNames("mean_Voltage", "consistentMeanVoltage"))


    print("trigger_code and Voltage renamed")
    print(head(non_consistent))
    print(head(consistent_means))

    # merge consistent means with non_consistent

    data_diff <-   merge(non_consistent,consistent_means,by=c("Trigger_code_cons","Electrode","Time","anteriority_3l","mediality_a"))
    #data_diff <- left_join(consistent_means, non_consistent ,by=c("Trigger_code_cons"))

    rm(non_consistent,consistent_means)
    print(head(data_diff))

    print("consistent means merged with non_consistent")

    # compute difference for each inconsistent trigger code
    data_diff$Voltage <- data_diff$mean_Voltage - data_diff$consistentMeanVoltage
    print("Difference computed")



  } else {

    data_diff <- data_diff %>% spread( !!conditionToPlot_enq, mean_Voltage )  %>% dplyr::mutate( Voltage = !!levelA_enq - !!levelB_enq)
    #print(head(data_diff))
    print("Data mutated")


  }

  data_reduced <- rename(data_reduced,   Condition = !! conditionToPlot_enq)

  data_diff$Condition <- "Difference"

  #topo_ggplots <- plot_topoplots_by_TW(data_diff,  50, 200, 600, plotname)
  #time_windows <- list(c(-150,50),c(50,250),c(250,400),c(400,550),c(550,800),c(800,900))
  #time_windows <- list(c(-200,0),c(0,200),c(200,300),c(300,500),c(500,700),c(700,900))
  # time_windows <- list(c(-250,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700),c(700,900))

  topo_ggplots <- plot_topoplots_by_custom_TW(data_diff, time_windows, plotname,topoplots_scale)

  if(show_group_obs) {

    erp_plot <- ggplot2::ggplot(data_reduced,aes_string(x= "Time", y= "Voltage" )) +
      scale_y_reverse() + theme_light() +
      stat_summary(data = data_diff,fun.y=mean,geom = "line",aes_string(group = rlang::quo_text(group_var_enq),colour = "Condition"),alpha = 0.1)+ # by subject line
      stat_summary(data = data_diff,fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.3, aes(fill = Condition))+ # CI ribbon
      stat_summary(fun= mean,geom = "line",size = .75, aes(colour = Condition) )+ # conditions lines
      stat_summary(data = data_diff,fun=mean,geom = "line", aes(colour = Condition))+ # difference line
      labs(x = "Time (in ms)",
           y = bquote(paste("Voltage amplitude (", mu, "V): ", .("Voltage")))
      )+
      scale_color_manual(values=color_palette)+
      scale_x_continuous(breaks=seq(-500,900,100))+
      geom_vline(xintercept = 0,linetype = "solid" )+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 400, linetype = "dotted")+
      geom_vline(xintercept = -450, linetype = "longdash")+
      annotate(geom = "text", x = -420, y = y_annot, label = "Prime", angle = 90)+
      annotate(geom = "text", x = 30, y = y_annot, label = "Target", angle = 90)+
      annotate(geom = "text", x = 370, y = y_annot, label = "400ms", color = "dark grey", angle = 90)+
      annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-0.5, ymax=0.5, alpha = .4)+
      annotate("rect", xmin = -450, xmax = -250, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+
      annotate("rect", xmin = 0, xmax = 200, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+
      annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = "Baseline", color = "red",size = 3)+
      facet_wrap( anteriority_3l ~ mediality_a, scales='free_x')  # reformulate(med_levels,ant_levels)

    ggsave(filename=paste(plotname,'.pdf', sep=''), width = 22, height = 18)


  } else {


    print("preparing plot")

    erp_plot <- ggplot2::ggplot(data_reduced,aes_string(x= "Time", y= "Voltage" )) +
      guides(colour = guide_legend(override.aes = list(size = 2))) +
      scale_y_reverse() + theme_light() + theme(
        legend.position="bottom",
        strip.text.x = element_text( size = 16, color = "black", face = "bold" ),
        strip.background = element_rect( fill="white", color=NA),
        legend.spacing.x = unit(0.8, "cm"),
        legend.key.width = unit(3, "lines"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        axis.title=element_text(size=18),
        plot.title = element_text(size = 14, face = "bold",hjust = 0.5)
        #,text = element_text(family = "Andale Mono")


        ) +
      #stat_summary(data = data_diff,fun.y=mean,geom = "line",aes_string(group = group_var,colour = "Condition"),alpha = 0.1)+ # by subject line
      stat_summary(data = data_diff,fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.3, aes(fill = Condition), show.legend = F)+ # CI ribbon
      stat_summary(fun = mean,geom = "line",size = .75, aes(colour = Condition) )+ # conditions lines
      stat_summary(data = data_diff,fun=mean,geom = "line", aes(colour = Condition))+ # difference line
      labs(x = "Time (in ms)",
           y = bquote(paste("Voltage amplitude (", mu, "V): ", .("Voltage"))),
           title = paste(  Sys.Date(), paste("- Baseline:[",baseline[1],"ms;",baseline[2],"ms]",sep="") ,"- dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects"),
           caption = "Generated with ERPscope")+
      scale_color_manual(values=color_palette)+
      scale_x_continuous(breaks=seq(-500,900,100))+
      geom_vline(xintercept = 0,linetype = "solid" )+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 400, linetype = "dotted")+
      geom_vline(xintercept = -450, linetype = "longdash")+
      annotate(geom = "text", x = -420, y = y_annot, label = "Prime", angle = 90)+
      annotate(geom = "text", x = 30, y = y_annot, label = "Target", angle = 90)+
      annotate(geom = "text", x = 370, y = y_annot, label = "400ms", color = "dark grey", angle = 90)+
      annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-0.5, ymax=0.5, alpha = .4)+
      annotate("rect", xmin = -450, xmax = -250, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+
      annotate("rect", xmin = 0, xmax = 200, ymin=y_annot -delta, ymax=y_annot +delta, alpha = .2)+
      annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = "Baseline", color = "red",size = 3)+
      facet_wrap( anteriority_3l ~ mediality_a, scales='free_x',labeller = label_wrap_gen_alex(multi_line=FALSE) ) #+theme_ipsum_rc() #+ theme_ipsum()  # reformulate(med_levels,ant_levels) label_wrap_gen_alex(multi_line=FALSE)

      # facet_wrap


  }

  message("assembling topoplot")
  topoplot <- ggpubr::ggarrange(plotlist=topo_ggplots, nrow = 1, ncol = length(time_windows))

  message(paste(Sys.time(),"assembling all plots"))

  figure  <- ggpubr::ggarrange( erp_plot, topoplot, heights = c(2, 0.5),
                        #labels = c("ERPs", "Voltage maps"),
                        ncol = 1, nrow = 2)

  figure  <-  ggpubr::annotate_figure(figure,
                             top = ggpubr::text_grob(paste( "Difference wave for condition",rlang::quo_text(conditionToPlot_enq),":",rlang::quo_text(levelA_enq)," - ", rlang::quo_text(levelB_enq)),
                                             color = "black", face = "bold", size = 18))

  message("creating file")
  ggplot2::ggsave(plot= figure ,filename=paste(plotname,'.pdf', sep=''), width = 22, height = 18)


}

# extrafont::font_import()
#loadfonts(device = "pdf", quiet = FALSE)


label_wrap_gen_alex <- function(width = 25, multi_line = FALSE) {
  fun <- function(labels) {
    labels <- label_value_alex(labels, multi_line = multi_line)
    lapply(labels, function(x) {
      x <- strwrap(x, width = width, simplify = FALSE)
      vapply(x, paste, character(1), collapse = "\n")
    })
  }
  structure(fun, class = "labeller")
}


label_value_alex <- function(labels, multi_line = TRUE) {
  labels <- lapply(labels, as.character)
  if (multi_line) {
    labels
  } else {

    collapse_labels_lines_alex(labels)
  }
}


collapse_labels_lines_alex <- function(labels) {
  out <- do.call("Map", c(list(paste, sep = " - "), rev(labels)))
  list(unname(unlist(out)))
}


