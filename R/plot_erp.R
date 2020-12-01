#' Plot ERP for 9/12 electrodes of interest
#'
#' This function creates a plot file with 9 or 12 electrodes of interests (for now) displaying ERPs
#' for different conditions. It will need a loaded dataframe with your EEG data
#' and a column indicating the condition to display.
#' It assumes that there is a column named Voltage with your voltage values.
#' Default values are provided for electrodes but it can be customized.
#'
#' @param data dataframe containing ERP data
#' @param conditionToPlot column of the dataframe with different levels to plot
#' @param electrodes_list vector of Electrodes names to plot (between quotes)
#' @param color_palette vector with colors for each levels (in the order of levels of conditionToPlot)
#' @param output_type file type of the output
#' @param baseline vector defining the baseline used during preprocessing
#' @param adjusted_baseline boolean to indicate if the baseline should be simulated on the time-window provided in
#' @param plotname 'auto' or custom string for plot title and plot file name
#' @param show_check_message boolean to indicate if you want a confirmation message to be displayed before running the function
#' @param show_conf_interval boolean to indicate if 95% CI (bootstrapped) should be displayed
#' @param custom_labels list of custom label list. Each custom labels should have the structure: list(start_time, end_time, "label")
#' @param labels_vertical_position 'auto' or custom position for the center of the label (in microVolts)
#' @param labels_height 'auto' or custom height (in microVolts)
#' @param vary variable that is used for the y-axis
#' @param background string that defines the color of the background : "grid" (default), "white" and "dark"
#' @param line_thickness single value (numeric, e.g. 0.75) or a vector of numerics such as: c(0.75, 1 , 1.25, 1.5)
#' @param line_type single value (string, e.g. 'solid') or a vector of strings such as: c('solid', 'dotted , 'dashed','longdash','F1')
#' @param polarity_up defines which polarity is plotted up : "negative" or "positive"
#' @return A file containing the ERP plots
#' @export

plot_erp <- function(             data, #
                                  conditionToPlot,
                                  electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
                                  baseline = c(-2450,-2250),
                                  color_palette =  c("#4DAF4A", "#EA2721","#377EB8","#FF7F00","#984EA3","#000000","#5c5c5c", "#945D25", "#FF748C", "#2E692C"),
                                  output_type = 'pdf',
                                  adjusted_baseline = FALSE,
                                  time_labels_interval = 'auto',
                                  plotname = 'auto',
                                  show_check_message = FALSE,
                                  show_conf_interval = FALSE,
                                  custom_labels = list(),
                                  labels_vertical_position = 'auto',
                                  labels_height = 'auto',
                                  vary ="Voltage",
                                  background = "grid", # alternative : "dark" or "white"
                                  line_thickness = 0.75 , # alternative a vector as:  c(0.75, 1 , 1.25, 1.5)
                                  line_type = 'solid', # alternative a vector as: c('solid', 'dotted , 'dashed','longdash','F1')
                                  polarity_up = 'negative',
                                  font_size_electrode_label = 16,
                                  font_size_x_axis_ticks = 12,
                                  font_size_y_axis_ticks = 12,
                                  display_baseline = TRUE,
                                  baseline_label = "Baseline"
                                  ) {



    ## Check arguments
    conditionToPlot_enq <- rlang::enquo(conditionToPlot)
    conditionToPlot <- rlang::quo_text(conditionToPlot_enq)

    # for now ERPscope works with traditional dataframes
    if(tibble::is_tibble(data))
    {
      data <-  as.data.frame(data)
      message("Converting data tibble as traditional dataframe")
    }

    # checking that the argument conditionToPlot is a column in the dataframe
    if(!(conditionToPlot %in% colnames(data)))
    {
      stop(paste("There is no column",conditionToPlot,"in the dataframe",deparse(substitute(data)) ))
    }

    # checking that the argument conditionToPlot is a factor
     if(!(is.factor(data[,conditionToPlot])) ) {
      data[,conditionToPlot] <- as.factor(data[,conditionToPlot])
      message("Converting condition to plot as a factor")
     }

    # checking that the variable Electrode is a factor
    if(!(is.factor(data[,"Electrode"])) ) {
      data[,"Electrode"] <- as.factor(data[,"Electrode"])
      message("Converting Electrode as a factor")
    }

    # checking that given electrodes to display are in the dataframe
    df_electrodes <- unique(data$Electrode)
    for(current_elec in 1:length(electrodes_list)){
      if(!(electrodes_list[current_elec] %in% df_electrodes)) {
         stop(paste("Electrode",electrodes_list[current_elec] ,"is not in the data"))
      }
    }

    # checking that the variable Subject is in the provided dataframe and is a factor
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

    # storing the number of levels to plot
    number_of_levels <- length(levels(data[,conditionToPlot]))

    # checking that there is enough colors in the palette to plot all levels
    if(length(color_palette) < number_of_levels) { stop(paste("Please provide more colors in your palette: currently",length(color_palette),"colors to plot",number_of_levels , "levels")) }

    # check if vector baseline has two elements, then that baseline[1] is > tmin and < tmax, same for baseline[2], check that baseline[1]< baseline[2]
    if(length(baseline) != 2) {
      stop(paste("Provided baseline ",baseline,"is not valid"))
    }

    # settings of the confirmation menu if show_check_message is set to TRUE
    init_message <- paste("You are about to plot ERPs for",length(electrodes_list), "electrodes for the condition", conditionToPlot , "with",number_of_levels,"levels and for",number_of_subjects,"subjects.")
    if(show_check_message == TRUE) {
      choice <- menu(c("y", "n"), title= paste(init_message,"Do you want to continue?"))
    } else {
      message(init_message)
      choice  <- 1
    }

    # Build the plot
    if(choice == 1) {

      # set the automatic plot name if selected
      if(plotname == 'auto') {
            plotname = paste(Sys.time(),"_ERPs_",deparse(substitute(data)),"_",conditionToPlot, sep="") # Sys.time Sys.Date
      }

      # set the file name (adding the extension to plot name)
      plot_filename <- paste(plotname,'.',output_type, sep='')

      # store start time to compute total duration
      t_start <- Sys.time()

      # message to user
      message(paste(Sys.time()," - Beginning to plot ERP in",plot_filename))

      # check if a file with the same name already exists
      if(file.exists(plot_filename)) {
        overwriting_choice <- menu(c("y", "n"), title="A file with the same name already exists! Do you want to continue?")
        if(overwriting_choice ==1){
            message("File will be overwritten")
        } else {
            return(message("Interrupting to not overwrite existing file with same name"))
        }
      }

      # compute time_labels_interval, time_min, time_max  (for tick marks on the x-axis) if auto is selected
      if(time_labels_interval == 'auto'){
        time_labels_interval <- ceiling((max(data$Time)- min(data$Time)  )/1000)*100
      }
      time_min  <- ((min(data$Time) %/% time_labels_interval) +1) * time_labels_interval
      time_max  <- (max(data$Time) %/% time_labels_interval) * time_labels_interval


      # for now plot_erp work for 9 or 12 electrodes, that will change soon
      numberOfRows <- length(electrodes_list)/3

      # remove from the df electrodes that are not necessary to improve memory load
      dataToPlot <- subset(data, Electrode %in% electrodes_list)
      # set order of levels to provided electrodes list
      dataToPlot$Electrode <- factor(dataToPlot$Electrode, levels = electrodes_list)

      # if baseline simulation is selected, check that there are two elements and launch baseline_correction function
      if(adjusted_baseline == TRUE) {
        if(length(baseline) != 2) {
          stop(paste("Provided baseline ",baseline,"is not valid"))
        }else{
          dataToPlot <- baseline_correction(dataToPlot,conditionToPlot,baseline)
          vary <- "RebaselinedVoltage"
        }
      }


      # ggplot creation

      # generate different ggplot bases depending on line_thickness and line_type arguments

      # if single value for line_thickness
      if(length(line_thickness)<2){

        # if single value for line_type
        if(length(line_type) < 2){
          message('plot with single thickness values and single linetype')
          tempo <- ggplot(dataToPlot, aes_string(x= "Time", y= vary, colour = conditionToPlot, fill = conditionToPlot) )+
            stat_summary(fun = mean, geom = "line", size = line_thickness, linetype= line_type)

        # if vector for line_type
        } else if (is.vector(line_type) ) {
          message('plot with single thickness values and multiple line types')
          tempo <- ggplot(dataToPlot, aes_string(x= "Time", y= vary, colour = conditionToPlot, fill = conditionToPlot,linetype= conditionToPlot) )+
            stat_summary(fun = mean, geom = "line", size = line_thickness) + scale_linetype_manual(values=line_type)
        } else { stop("Not valid type of linetype argument") }

      # if vector value for line_thickness
      } else if (is.vector(line_thickness) ) {

        # if single value for line_type
        if(length(line_type) < 2){
          message('plot with multiple thickness single linetype')
          tempo <- ggplot(dataToPlot, aes_string(x= "Time", y= vary, colour = conditionToPlot, fill = conditionToPlot,size = conditionToPlot) )+
            stat_summary(fun = mean, geom = "line",linetype= line_type) + scale_size_manual(values= line_thickness)

        # if vector for line_type
        } else if (is.vector(line_type) ) {
          message('plot with multiple thickness multiple linetype')
          tempo <- ggplot(dataToPlot, aes_string(x= "Time", y= vary, colour = conditionToPlot, fill = conditionToPlot,size = conditionToPlot,linetype= conditionToPlot) )+
            stat_summary(fun = mean, geom = "line")+
            scale_size_manual(values= line_thickness)+
            scale_linetype_manual(values=line_type)
        } else { stop("Not valid type of linetype argument") }

      } else {
        stop("Not valid type of line thickness argument")
      }

      # add color palettes to the plot
      tempo <- tempo +
        scale_color_manual(values=color_palette)+
        scale_fill_manual(values=color_palette)

      # reverse polarity if negative is selected
      if (polarity_up == 'negative') {
          tempo <- tempo + scale_y_reverse()
      }

      # add error bar if needed
      if(show_conf_interval == TRUE) {
        tempo <- tempo +  stat_summary(fun.data = mean_cl_normal,geom = "ribbon",alpha = 0.3 , colour=NA)
      }

      # add background layer to plot
      if( background == "white") {
        tempo <- tempo + theme_classic()
      } else if ( background == "dark") {
        tempo <- tempo + theme_dark()
      } else {
        tempo <- tempo + theme_light()
      }


      # add axis, baseline and legend
      tempo <- tempo +
               # define labels and title
               labs( x = "Time (in ms)",
                     y = bquote(paste("Voltage amplitude (", mu, "V): ", .(vary))),
                     title = paste("ERP: ", vary,"by",conditionToPlot),
                     subtitle = paste(  Sys.Date(), paste("- Baseline:[",baseline[1],"ms;",baseline[2],"ms]",sep="") ,"- dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects")
                     )+ #caption = "Generated with ERPscope"
                # ticks on x axis
                scale_x_continuous(breaks=seq(time_min,time_max,time_labels_interval))+

                # add line axis
                geom_vline(xintercept = 0,linetype = "solid" )+
                geom_hline(yintercept = 0)

           # baseline annotation
           if(display_baseline) {
                  
                  tempo <- tempo +
                            annotate("rect", xmin = baseline[1] , xmax = baseline[2] , ymin=-1, ymax=1, alpha = .4,fill = "red")+
                            annotate(geom = "text", x = (baseline[2] + baseline[1])/2, y = 0.3, label = baseline_label, color = "red",size = 2)
            }
                

      # add facets and define theme (font sizes, facets labels)
      tempo <- tempo +  facet_wrap( ~ Electrode , nrow = numberOfRows, ncol = 3, scales='free_x' ) +
                        guides(colour = guide_legend(override.aes = list(size = 2))) +

                        theme(  strip.text.x = element_text( size = font_size_electrode_label, color = "black", face = "bold" ),
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
                                axis.title = element_text(size=18),
                                axis.text.x = element_text( size= font_size_x_axis_ticks),
                                axis.text.y = element_text( size= font_size_y_axis_ticks)

                          )

      # if there are custom labels to add
      if(length(custom_labels) != 0) {

        # compute custom labels positions
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

        # add custom labels to the plots

        for(i in 1:length(custom_labels)) {

            tempo =  tempo + geom_vline(xintercept = custom_labels[[i]][[1]], linetype = "longdash") +
                             annotate(geom = "text", x= (custom_labels[[i]][[1]]+ custom_labels[[i]][[2]])/2, y = labels_vertical_position, label = custom_labels[[i]][[3]], angle = 0) +
                             annotate("rect", xmin = custom_labels[[i]][[1]], xmax = custom_labels[[i]][[2]], ymin= labels_vertical_position - labels_height, ymax=labels_vertical_position +labels_height, alpha = .2)
        }

     } # end of custom labels


      # "save" message to user
      message("Saving plot to file")
      # save plot to file
      ggsave(tempo, filename=plot_filename, width = 22, height = 18)

      # store end time for total duration calculation
      t_end <- Sys.time()

      # end message to user
      message(paste(Sys.time()," - End - Generating the file took",  substring(round(   difftime(t_end,t_start,units="mins")  , 2),1 ),"mins"))

    } # end of menu

}

