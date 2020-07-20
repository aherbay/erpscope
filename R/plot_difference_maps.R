
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
plot_difference_maps  <- function( data,
                              plotname = 'auto',
                              conditionToPlot = MM_RAW,
                              levelA = semMM_RAW ,
                              levelB = consistent,
                              output_type ='pdf',
                              vary= Voltage,
                              group_var = Subject,

                              topoplots_data = "voltage_difference", # "voltage_difference", "t_test_t_value", "t_test_p_value"
                              topoplots_time_windows = list(c(-250,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
                              topoplots_scale = c(-2,2),
                              t_test_threshold = 0.05,
                              fixed = c(-250,900,50)
) {


  ##############
  # Transforming argument values

  conditionToPlot_enq <- rlang::enquo(conditionToPlot)
  levelA_enq <- rlang::enquo(levelA)
  levelB_enq <- rlang::enquo(levelB)
  vary_enq <- rlang::enquo(vary)
  group_var_enq <- rlang::enquo(group_var)

  number_of_subjects <- length(unique(data$Subject))
  number_of_levels <- length(levels(data[,rlang::quo_text(conditionToPlot_enq)]))

  ##############
  # plotname and plot_filename

  if(plotname == 'auto') {
    plotname = paste(Sys.Date(),"_",deparse(substitute(data)),"_",number_of_subjects,"PPTS_ERP_DIFF_",rlang::quo_text(conditionToPlot_enq),"_",rlang::quo_text(levelA_enq),"-", rlang::quo_text(levelB_enq) ,sep="")
  }
  plot_filename <- paste(plotname,'.',output_type, sep='')
  t_start <- Sys.time()
  message(paste(Sys.time()," - Beginning to plot differences in",plot_filename))

  ##############
  # checking if file already exists

  if(file.exists(plot_filename)) message("File already exists! Overwriting it")

  ##############
  # checking if level A and B are present in data

  levelsConditionToPlot <- levels(data[,rlang::quo_text(conditionToPlot_enq)])
  if(!(rlang::quo_text(levelA_enq) %in% levelsConditionToPlot))
  {
    stop(paste("Level A",rlang::quo_text(levelA_enq),"is not present in the column",rlang::quo_text(conditionToPlot_enq)," of the dataframe",deparse(substitute(data)) ))
  }
  if(!(rlang::quo_text(levelB_enq) %in% levelsConditionToPlot))
  {
    stop(paste("Level B",rlang::quo_text(levelB_enq),"is not present in the column",rlang::quo_text(conditionToPlot_enq)," of the dataframe",deparse(substitute(data)) ))
  }


  ##############
  # selecting relevant columns to reduce df size in memory

  message(paste(Sys.time()," - Selecting relevant data (columns) "))

  data_reduced <- dplyr::select(data, !! group_var_enq, Time, Electrode , !! vary_enq ,!! conditionToPlot_enq)


  ##############
  # Filtering relevant data (data for substracted conditions only)

  message(paste(Sys.time()," - Filtering relevant data (data for substracted conditions only) "))
  data_reduced <- filter(data_reduced, !! conditionToPlot_enq %in% c(rlang::quo_text(levelA_enq), rlang::quo_text(levelB_enq)) ) %>% droplevels()

  ##############
  # Computing the difference between conditions

  message(paste(Sys.time()," - Computing the difference between conditions"))

  # computing mean for each level (A and B)

  data_diff <- data_reduced %>%
    group_by( !! group_var_enq, Time, Electrode, !! conditionToPlot_enq) %>%
    summarise(mean_Voltage = mean(!! vary_enq))

  # computing difference

  data_diff <- data_diff %>% spread( !!conditionToPlot_enq, mean_Voltage )  %>% dplyr::mutate( Voltage = !!levelA_enq - !!levelB_enq)
  message(paste(Sys.time()," - Difference computed "))


  data_reduced <- rename(data_reduced,   Condition = !! conditionToPlot_enq)

  data_diff$Condition <- "Difference"

  ####################
  # process time windows

  if(!(length(fixed) == 0)){

    range <- fixed[2] - fixed[1]
    numberOfWindows<- range / fixed[3]
    time_windows <- list()
    time_windows_index <- 1
    for(time_windows_index in 1:numberOfWindows) {

      time_windows[[time_windows_index]] <- c( fixed[1]+(fixed[3]*(time_windows_index-1)), fixed[1]+(fixed[3]*(time_windows_index)))

    }
    topoplots_time_windows <- time_windows

  }


  ##############
  # Generating voltage maps
  message(paste(Sys.time()," - Computing voltage maps for", topoplots_data))

  if(topoplots_data == "voltage_difference"){

    topo_ggplots_with_legend <- plot_topoplots_by_custom_TW(data_diff, topoplots_time_windows, plotname,topoplots_scale,  data_to_display = "voltage_difference")

  }else if (topoplots_data == "t_test_t_value") {

    topo_ggplots_with_legend <- plot_topoplots_by_custom_TW(data_reduced, topoplots_time_windows, plotname,topoplots_scale,  data_to_display = topoplots_data, levelA= levelA_enq,levelB= levelB_enq )


  } else if (topoplots_data =="t_test_p_value") {

    topo_ggplots_with_legend <- plot_topoplots_by_custom_TW(data_reduced, topoplots_time_windows, plotname,topoplots_scale,  data_to_display = topoplots_data, levelA= levelA_enq,levelB= levelB_enq, t_test_threshold= t_test_threshold  )


  } else { stop(paste("Invalid topoplots_data:",topoplots_data)) }

  topo_ggplots <- topo_ggplots_with_legend[[1]]
  topo_legend <- topo_ggplots_with_legend[[2]]


  ##############
  #  Assembling voltage maps

  message(paste(Sys.time()," - Assembling voltage maps"))
  if(length(topoplots_time_windows) <13){
    topoplot <- ggpubr::ggarrange(plotlist=topo_ggplots, nrow = 1, ncol = length(topoplots_time_windows))
    topoplot_with_legend <- ggpubr::ggarrange( topo_legend, topoplot, heights = c(0.5, 4),
                                               #labels = c("ERPs", "Voltage maps"),
                                               ncol = 1, nrow =2)
  }else{
    topoplot <- ggpubr::ggarrange(plotlist=topo_ggplots, nrow = 2, ncol = (length(topoplots_time_windows)%/%2)+1)
    topoplot_with_legend <- ggpubr::ggarrange( topo_legend, topoplot, heights = c(0.5, 12),
                                               #labels = c("ERPs", "Voltage maps"),
                                               ncol = 1, nrow =2)
  }

  ##############
  # Adding title
  message(paste(Sys.time()," - Adding title"))

  topoplot_with_legend  <-  ggpubr::annotate_figure(topoplot_with_legend,
                                      top = ggpubr::text_grob(paste( "Difference wave for condition",rlang::quo_text(conditionToPlot_enq),":",rlang::quo_text(levelA_enq)," - ", rlang::quo_text(levelB_enq)),
                                                              color = "black", face = "bold", size = 18))

  ##############
  # Creating file

  message(paste(Sys.time()," - Creating file"))
  if(length(topoplots_time_windows) <13){
    ggplot2::ggsave(plot= topoplot_with_legend ,filename= plot_filename, width = 3.8*length(topoplots_time_windows), height = 5)
  } else {
    ggplot2::ggsave(plot= topoplot_with_legend ,filename= plot_filename, width = 2*length(topoplots_time_windows), height = 5)
  }
  t_end <- Sys.time()
  message(paste(Sys.time()," - End - Generating the file took",  substring(round(   difftime(t_end,t_start,units="mins")  , 2),1 ),"mins"))

} # end of plot_difference










