#' Plot ERP for 9/12 electrodes of interest
#'
#' This function creates a plot file with 9 or 12 electrodes of interests (for now) displaying ERPs
#' for different conditions. It will need a loaded dataframe with your EEG data
#' and a column indicating the condition to display.
#' It assumes that there is a column named Voltage with your voltage values.
#' Default values are provided for electrodes but it can be customized.
#'
#' @param data dataframe containing ERP data
#' @param condition_to_plot column of the dataframe with different levels to plot
#' @param electrodes_list vector of Electrodes names to plot (between quotes)
#' @param line_colors vector with colors for each levels (in the order of levels of condition_to_plot)
#' @param output_file_type file type of the output
#' @param preprocessing_baseline vector defining the baseline used during preprocessing
#' @param simulate_baseline boolean to indicate if the baseline should be simulated on the time-window provided in
#' @param plot_title 'auto' or custom string for plot title
#' @param output_file_name 'auto' or custom string for plot file name
#' @param show_check_message boolean to indicate if you want a confirmation message to be displayed before running the function
#' @param add_ribbon string to indicate if 95% CI (bootstrapped) should be displayed
#' @param custom_labels list of custom label list. Each custom labels should have the structure: list(start_time, end_time, "label")
#' @param labels_vertical_position 'auto' or custom position for the center of the label (in microVolts)
#' @param labels_height 'auto' or custom height (in microVolts)
#' @param vary variable that is used for the y-axis
#' @param plot_background string that defines the color of the plot_background : "grid" (default), "white" and "dark"
#' @param line_thickness single value (numeric, e.g. 0.75) or a vector of numerics such as: c(0.75, 1 , 1.25, 1.5)
#' @param line_type single value (string, e.g. 'solid') or a vector of strings such as: c('solid', 'dotted , 'dashed','longdash','F1')
#' @param polarity_up defines which polarity is plotted up : "negative" or "positive"
#' @return A file containing the ERP plots
#' @export

plot_erp <- function(

        # core arguments
            data,
            condition_to_plot,
            electrodes_layout = 'grid_erp_1020',

            polarity_up = 'negative',

        # output file parameters

            output_file_name = 'auto',
            output_file_type = 'pdf',    #output_file_type

        # plot aesthetics

            line_colors = c("#4DAF4A", "#EA2721","#377EB8","#FF7F00","#984EA3","#000000","#5c5c5c", "#945D25", "#FF748C", "#2E692C"),
            line_thickness = 0.75 , # alternative a vector as:  c(0.75, 1 , 1.25, 1.5)
            line_type  = 'solid', # alternative a vector as: c('solid', 'dotted , 'dashed','longdash','F1')

        # titles
            plot_title = 'auto',
            plot_title_font_size = 'auto',
            plot_title_font_color = 'black',

            plot_subtitle = 'auto',
            plot_subtitle_font_size = 'auto',
            plot_subtitle_font_color = 'black',

            plot_background = "grid", # alternative : "dark" or "white"
            plot_height = 18,
            plot_width = 22 ,
            electrode_labels_font_size = 14,
            electrode_labels_font_color = "black",

        # axis parameters
            voltage_axis_title= bquote(paste("Voltage amplitude (", mu, "V)")),
            time_axis_title= "Time (ms)",
            time_axis_tick_mark_labels_font_size= 12,
            voltage_axis_tick_mark_labels_font_size= 12,


            voltage_scale_limits = 'auto', # c(-4,4)
            time_scale_limits = 'auto',

            time_labels = 'auto', # 'auto', or single value or vector

            voltage_labels_interval = 'auto',



        # add info about the preprocessing baseline time window
            prepro_bsl_display = FALSE,
            prepro_bsl_time_window = c(-200, 0),
            prepro_bsl_fill_color = "#DEE5ED",
            prepro_bsl_label_fill_alpha = 0.9,
            prepro_bsl_label  = "Baseline",
            prepro_bsl_label_font_size = 2,
            prepro_bsl_label_text_color = "blue",
            prepro_bsl_label_vertical_position = 0.3,
            prepro_bsl_vertical_limits = c(-1,1),

        # simulate a new baseline

            simul_bsl_active = FALSE,
            simul_bsl_time_window = c(-200, 0),
            simul_bsl_label_fill_color = "#8282DF",
            simul_bsl_label_fill_alpha = 0.9,
            simul_bsl_label_text = "",
            simul_bsl_label_font_size = 4,

       # custom labels
            custom_labels = list(),
            custom_labels_vertical_position = 'auto_top',
            custom_labels_height = 'auto',
            custom_labels_fill_color ='grey',
            custom_labels_font_size = 1.9,
            custom_labels_line_style = "dashed",
            custom_labels_line_color = "black",

      # Add ribbon
            add_ribbon = 'none',

      # to change the default column containing voltage and time
            vary ="Voltage",
            varx ="Time",
            var_electrode = "Electrode",
            var_subject = "Subject",

      # interface message
            show_check_message = FALSE

  # baselines
  #preprocessing_baseline = list(display=FALSE, time_window= c(-100,0), color='blue', font_size= 1.7, height= 5, pp_baseline_label = ""), # add color and height ?
  #simulate_baseline = list(display=FALSE,time_window= c(-100,0), color='red',font_size= 1.7 ,  height= 5 , sim_baseline_label = ""), # add a rectange with color and height ?

) {

  ## Check arguments - TO REMOVE condition to plot will be given as string now
    #condition_to_plot_enq <- rlang::enquo(condition_to_plot)
    #condition_to_plot <- rlang::quo_text(condition_to_plot_enq)


  # for now ERPscope works with traditional dataframes

      if(tibble::is_tibble(data))
      {
        data <-  as.data.frame(data)
        message("Converting data tibble as traditional dataframe")
      }

  # checking that the argument condition_to_plot is a column in the dataframe

      if(!(condition_to_plot %in% colnames(data)))
      {
        stop(paste("There is no column",condition_to_plot,"in the dataframe",deparse(substitute(data)) ))
      }

  # checking that the argument condition_to_plot is a factor

      if(!(is.factor(data[,condition_to_plot])) ) {
        data[,condition_to_plot] <- as.factor(data[,condition_to_plot])
        message("Converting condition to plot as a factor")
      }

  # checking that the variable Electrode is a factor

      if(!(is.factor(data[,var_electrode])) ) {
        data[,var_electrode] <- as.factor(data[,var_electrode])
        message("Converting Electrode as a factor")
      }

  # checking that given electrodes to display are in the dataframe

      df_electrodes <- unique(data[,var_electrode])
      electrodes_subset <- unique(electrodes_layout$code)
      for(current_elec in 1:length(electrodes_subset)){
        if(!(electrodes_subset[current_elec] %in% df_electrodes)) {
          warning(paste("Electrode",electrodes_subset[current_elec] ,"defined in the layout is not in the data"))
        }
      }

  # checking that the variable Subject is in the provided dataframe and is a factor

      if((var_subject %in% colnames(data)))
      {
        if(!(is.factor(data[,var_subject])) ) {
          data[,var_subject] <- as.factor(data[,var_subject])
          message(paste("Converting variable",var_subject,"as a factor"))
        }

        number_of_subjects <- length(unique(data$Subject))

      } else {

        number_of_subjects <- "unknown"
        message(paste("No column", var_subject ,"in the dataframe",deparse(substitute(data)),"- unknown value will be mentionned" ))
      }

  # storing the number of levels to plot

      number_of_levels <- length(levels(data[,condition_to_plot]))

  # checking that there is enough colors in the palette to plot all levels

      if(length(line_colors) < number_of_levels) { stop(paste("Please provide more colors in your palette: currently",length(line_colors),"colors to plot",number_of_levels , "levels")) }

  # check if vector baseline has two elements, then that baseline[1] is > tmin and < tmax, same for baseline[2], check that baseline[1]< baseline[2]

      if( prepro_bsl_display ){ # if preprocessing baseline is to be displayed on the plot

        if(length(prepro_bsl_time_window) != 2) {
          stop(paste("Provided preprocessing baseline",toString(prepro_bsl_time_window),"is not a vector of 2 elements"))
        }

        if(prepro_bsl_time_window[2] <=  prepro_bsl_time_window[1]) {
          stop(paste("Problem with the provided preprocessing baseline:",prepro_bsl_time_window[1],"is higher than", prepro_bsl_time_window[2]))
        }
      }

  # settings of the confirmation menu if show_check_message is set to TRUE

      init_message <- paste("You are about to plot ERPs for",length(electrodes_subset), "electrodes with the layout",deparse(substitute(electrodes_layout)),"for the condition", condition_to_plot , "with",number_of_levels,"levels and for",number_of_subjects,"subjects.")

      if(show_check_message == TRUE) {
        choice <- menu(c("y", "n"), title= paste(init_message,"Do you want to continue?"))
      } else {
        message(init_message)
        choice  <- 1
      }



  # Build the plot

  if(choice == 1) {

    # set the automatic plot filename if selected

      if(output_file_name == 'auto') {
        output_file_name = paste(Sys.Date(),"_ERPs_",deparse(substitute(data)),"_",condition_to_plot,'.',output_file_type, sep="") # Sys.time Sys.Date
      } else {
        output_file_name = paste(output_file_name,output_file_type, sep="")
      }

    # store start time to compute total duration

      t_start <- Sys.time()

    # message to user

      message(paste(Sys.time()," - Beginning to plot ERP in",output_file_name))

    # check if a file with the same name already exists

      if(file.exists(output_file_name)) {
        overwriting_choice <- menu(c("y", "n"), title="A file with the same name already exists! Do you want to continue?")
        if(overwriting_choice ==1){
          message("File will be overwritten")
        } else {
          return(message("Interrupting to not overwrite existing file with same name"))
        }
      }

    # compute time_labels_interval, time_min, time_max  (for tick marks on the x-axis) if auto is selected
      # 3 possibilities : auto , single interval , or custum ticks defined in a vector by the user

      if(length(time_labels)<2){

        if(time_labels == 'auto'){
          time_labels_interval <- ceiling((max(data[,varx])- min(data[,varx])  )/1000)*100
        } else {
          time_labels_interval <- time_labels
        }

        time_min  <- ((min(data[,varx]) %/% time_labels_interval) +1) * time_labels_interval
        time_max  <- (max(data[,varx]) %/% time_labels_interval) * time_labels_interval

        ticks_vector <- seq(time_min,time_max,time_labels_interval)

      }else{

        ticks_vector <- time_labels
      }

    # remove from the df electrodes that are not necessary to improve memory load

       dataToPlot <- subset(data, Electrode %in% electrodes_subset)

    # for now plot_erp work for 9 or 12 electrodes, that will change soon
        # not relevant with layout numberOfRows <- length(electrodes_subset)/3

    # set order of levels to provided electrodes list
        # not relevant with layout dataToPlot$Electrode <- factor(dataToPlot$Electrode, levels = electrodes_subset)

    # if baseline simulation is selected, check that there are two elements and launch baseline_correction function

        if(simul_bsl_active == TRUE) {
          if(length(simul_bsl_time_window) != 2) {
            stop(paste("Provided simulated baseline c(",toString(simul_bsl_time_window),") is not a vector of 2 elements"))
          }
          if(simul_bsl_time_window[2] <=  simul_bsl_time_window[1] ) {
            stop(paste("Problem with the provided simulated baseline:",simul_bsl_time_window[1],"is higher than",simul_bsl_time_window[2]))
          }

          dataToPlot <- baseline_correction(dataToPlot,condition_to_plot,simul_bsl_time_window)
          vary <- "RebaselinedVoltage"
        }


  # ggplot creation

    # generate different ggplot bases depending on line_thickness and line_type arguments

        # if single value for line_thickness
        if(length(line_thickness)<2){

          # if single value for line_type
          if(length(line_type) < 2){
            message('plot with single thickness values and single linetype')
            erp_plot <- ggplot(dataToPlot, aes_string(x= varx, y= vary, colour = condition_to_plot, fill = condition_to_plot) )+
              stat_summary(fun = mean, geom = "line", size = line_thickness, linetype= line_type)

          # if vector for line_type
          } else if (is.vector(line_type) ) {
            message('plot with single thickness values and multiple line types')
            erp_plot <- ggplot(dataToPlot, aes_string(x= varx, y= vary, colour = condition_to_plot, fill = condition_to_plot,linetype= condition_to_plot) )+
              stat_summary(fun = mean, geom = "line", size = line_thickness) + scale_linetype_manual(values=line_type)
          } else { stop("Not valid type of linetype argument") }

        # if vector value for line_thickness
        } else if (is.vector(line_thickness) ) {

          # if single value for line_type
          if(length(line_type) < 2){
            message('plot with multiple thickness single linetype')
            erp_plot <- ggplot(dataToPlot, aes_string(x= varx, y= vary, colour = condition_to_plot, fill = condition_to_plot,size = condition_to_plot) )+
              stat_summary(fun = mean, geom = "line",linetype= line_type) + scale_size_manual(values= line_thickness)

          # if vector for line_type
          } else if (is.vector(line_type) ) {
            message('plot with multiple thickness multiple linetype')
            erp_plot <- ggplot(dataToPlot, aes_string(x= varx, y= vary, colour = condition_to_plot, fill = condition_to_plot,size = condition_to_plot,linetype= condition_to_plot) )+
              stat_summary(fun = mean, geom = "line")+
              scale_size_manual(values= line_thickness)+
              scale_linetype_manual(values=line_type)
          } else { stop("Not valid type of linetype argument") }

        } else {
          stop("Not valid type of line thickness argument")
        }

   # add color palettes to the plot
      erp_plot <- erp_plot +
        scale_color_manual(values=line_colors)+
        scale_fill_manual(values=line_colors)

    # reverse polarity if negative is selected
        if (polarity_up == 'negative') {
          erp_plot <- erp_plot + scale_y_reverse()
        }

    # add error bar if needed mean_sdl  mean_cl_boot mean_cl_normal
        if(add_ribbon != 'none') {
          erp_plot <- erp_plot +  stat_summary(fun.data = eval(parse(text= add_ribbon)),geom = "ribbon",alpha = 0.3 , colour=NA)
        }

    #eval(parse(text='mean_cl_normal'))

    # add error bar if needed
    #if(add_ribbon == '') {
    #  erp_plot <- erp_plot +  stat_summary(fun.data = mean_cl_normal,geom = "ribbon",alpha = 0.3 , colour=NA)
   # }

    # add background layer to plot
        if( plot_background == "white") {
          erp_plot <- erp_plot + theme_classic()
        } else if ( plot_background == "dark") {
          erp_plot <- erp_plot + theme_dark()
        } else {
          erp_plot <- erp_plot + theme_light()
        }


    # set the automatic plot title if selected
      if(plot_title == 'auto') {
        plot_title = paste("ERP: ", vary,"by",condition_to_plot)
      }

    # set the automatic plot subtitle if selected
      if(plot_subtitle == 'auto') {
        if(prepro_bsl_display){
          plot_subtitle = paste(  Sys.Date(), paste("- Baseline:[",prepro_bsl_time_window[1],"ms;",prepro_bsl_time_window[2],"ms]",sep="") ,"- dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects")
        }else{
          plot_subtitle = paste(  Sys.Date(),"- dataset:",deparse(substitute(data)),"with",number_of_subjects,"subjects")
        }
      }

    # add axis titles and plot title
      erp_plot <- erp_plot +
        labs( x = time_axis_title,
              y = voltage_axis_title,
              title = plot_title,
              subtitle = plot_subtitle
        )

    # ticks on x axis
      erp_plot <- erp_plot + scale_x_continuous(breaks= ticks_vector)

    # add lines of x and y axis
      erp_plot <- erp_plot + geom_vline(xintercept = 0,linetype = "solid" )+ geom_hline(yintercept = 0)


    # add baseline annotation
        if(prepro_bsl_display){
          erp_plot <- erp_plot +
            annotate("rect", xmin = prepro_bsl_time_window[1] , xmax = prepro_bsl_time_window[2] , ymin=prepro_bsl_vertical_limits[1], ymax=prepro_bsl_vertical_limits[2], alpha = prepro_bsl_label_fill_alpha, fill = prepro_bsl_fill_color)+
            annotate(geom = "text", x = (prepro_bsl_time_window[2] + prepro_bsl_time_window[1])/2, y = 0.3, label = prepro_bsl_label, color = prepro_bsl_label_text_color,size = prepro_bsl_label_font_size)
        }

    # add facets and define theme (font sizes, facets labels)
        if(length(electrodes_subset)== 0 ) {
          erp_plot <- erp_plot +  geofacet::facet_geo(~ Electrode, grid = electrodes_layout, scales='free')
        } else {
          erp_plot <- erp_plot +  geofacet::facet_geo(~ Electrode, grid = subset(electrodes_layout, code %in% electrodes_subset)) #, scales='free_x'
        }


    # define theme (font sizes, facets labels)
    erp_plot <- erp_plot + guides(colour = guide_legend(override.aes = list(size = 2))) +

      theme(  strip.text.x = element_text( size = electrode_labels_font_size, color = electrode_labels_font_color, face = "bold" ),
              strip.background = element_rect( fill="white", color=NA),
              legend.position="bottom",
              plot.title = element_text(size = plot_title_font_size, face = "bold",hjust = 0.5),
              plot.subtitle = element_text(size = plot_subtitle_font_size ,hjust = 0.5),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 15),
              legend.spacing.x = unit(0.8, "cm"),
              legend.key.width = unit(3, "lines"),
              #legend.key.size = unit(2, "lines"),
              #legend.key.height  = unit(15, "lines"),
              axis.title=element_text(size=18),
              axis.text.y = element_text( size= voltage_axis_tick_mark_labels_font_size),
              axis.text.x = element_text( size= time_axis_tick_mark_labels_font_size)
      )

    if(voltage_scale_limits != 'auto'){
      erp_plot <- erp_plot +  coord_cartesian(ylim = voltage_scale_limits )
    }


    # if there are custom labels to add
    if(length(custom_labels) != 0) {

      # compute custom labels positions
      if(custom_labels_vertical_position %in% c("auto_top","auto_bottom") | custom_labels_height == "auto" ){

        range <- ggplot_build(erp_plot)$layout$panel_scales_y[[1]]$range$range
        y_min <-range[1]
        y_max <-range[2]

        erp_plot <- erp_plot +  coord_cartesian(ylim = c(y_min,y_max) )

        if((custom_labels_vertical_position == "auto_top" && polarity_up == 'negative') |  (custom_labels_vertical_position == "auto_bottom" && polarity_up == 'positive') ){
          custom_labels_vertical_position =  y_min + (y_max - y_min) / 32
        }

        if((custom_labels_vertical_position == "auto_bottom" && polarity_up == 'negative') |  (custom_labels_vertical_position == "auto_top" && polarity_up == 'positive') ){
          custom_labels_vertical_position =  y_max - (y_max - y_min) / 32
        }

        if(custom_labels_height == "auto"){
          custom_labels_height = (y_max - y_min)/8
        }
      }

      labels_half_height <- custom_labels_height/2 # divide by two

      # add custom labels to the plots

      for(i in 1:length(custom_labels)) {

        erp_plot =  erp_plot + geom_vline(xintercept = custom_labels[[i]][[1]], linetype = "longdash") +
          annotate(geom = "text", x= (custom_labels[[i]][[1]]+ custom_labels[[i]][[2]])/2, y = custom_labels_vertical_position, label = custom_labels[[i]][[3]], angle = 0) +
          annotate("rect", xmin = custom_labels[[i]][[1]], xmax = custom_labels[[i]][[2]], ymin= custom_labels_vertical_position - labels_half_height, ymax=custom_labels_vertical_position +labels_half_height, alpha = .2, fill = "blue")
      }

    } # end of custom labels


    # "save" message to user
    message("Saving plot to file")

    # save plot to file
    ggsave(erp_plot, filename=output_file_name, width = plot_width, height = plot_height)

    # store end time for total duration calculation
    t_end <- Sys.time()

    # end message to user
    message(paste(Sys.time()," - End - Generating the file took",  substring(round(   difftime(t_end,t_start,units="mins")  , 2),1 ),"mins"))

  } # end of menu

}
