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

plot_cor_with_erp_effect <- function ( erpDataset ,
                                 behavDataset ,
                                 erp_var ,
                                 erp_levelA  ,
                                 erp_levelB  ,
                                 behav_var ,
                                 subject_var = "Subject_short",
                                 erp_start_time  ,
                                 erp_end_time,
                                 var_color = NULL

                                 ) {

  erp_var_enq <- rlang::enquo(erp_var)
  erp_var_txt <- rlang::quo_text(erp_var_enq)
  erp_levelA_enq <- rlang::enquo(erp_levelA)
  erp_levelB_enq <- rlang::enquo(erp_levelB)
  erp_levelA_txt <-   rlang::quo_text(erp_levelA_enq)
  erp_levelB_txt <-   rlang::quo_text(erp_levelB_enq)

  behav_var_enq <- rlang::enquo(behav_var)
  behav_var_txt <-   rlang::quo_text(behav_var_enq)

  summaryData <-   erpDataset  %>% subset( Time > erp_start_time & erpDataset$Time< erp_end_time) %>% dplyr::group_by(Subject,!! erp_var_enq , anteriority_3l) %>% dplyr::summarise(Voltage=mean(Voltage))
  summaryData <- tidyr::spread(summaryData, !!erp_var_enq, Voltage)
  summaryData <- left_join(summaryData,behavDataset)
  summaryData <- as.data.frame(summaryData)
  summaryData$ERPeffect <- summaryData[,erp_levelA_txt] - summaryData[,erp_levelB_txt]


#  print(summaryData[,"ERPeffect"])
  print(summaryData[,behav_var_txt])
 # cor.test(summaryData[,"ERPeffect"],summaryData[,behav_var_txt])


  plot_relation_with_cor(dataset= summaryData,
                         varx= behav_var_txt,
                         vary= "ERPeffect",
                         var_label=subject_var,
                         graph_title_header = paste ("Correlation between [",erp_start_time,",",erp_end_time,"]",erp_var_txt,"(",erp_levelA_txt,"-",erp_levelB_txt,") and",behav_var_txt),
                         var_color)

}
