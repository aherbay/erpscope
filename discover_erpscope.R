# DISCOVER ERPSCOPE
# By Alexandre HERBAY
# Last updated on 2020-10-01
# Compatible with ERPscope version 0.0.0.9006


# Install ERPSCOPE

install.packages("devtools")
devtools::install_github("aherbay/erpscope")

# Or update ERPSCOPE

detach("package:erpscope", unload=TRUE) # unload it if loaded
devtools::install_github("aherbay/erpscope") # update installation



library(erpscope) # load it


################################# plot_erp function


# Plot simple ERPs for 9 electrodes

plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                   "C3", "Cz","C4",
                                  "P3", "Pz", "P4",
                                  "O1", "Oz", "O2"),
              baseline = c(-500,-300)
)


# Plot simple ERPs for 12 electrodes

plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-500,-300)
)



# Add custom labels & change colors

plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-500,-300),
           custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
           color_palette =  c("#4DAF4A","#EA2721","#000000")


)


# Change file type, plotname and  time_labels_interval

plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-500,-300),
           custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
           color_palette =  c("#4DAF4A","#EA2721","#000000"),
           output_type = 'png',
           plotname = "MyVeryFancyTitle",
           time_labels_interval = 100

)



# Change  background

plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-500,-300),
           custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
           color_palette =  c("#4DAF4A","#EA2721","#000000"),
           background = "dark" # "dark"
)


# Change  line thickness

plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-500,-300),
           custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
           color_palette =  c("#4DAF4A","#EA2721","#000000"),
           line_thickness = c(0.75, 0.6, 1) ,
           line_type = c('solid','dotted','F1')
)



plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4",
                                "O1", "Oz", "O2"),
           baseline = c(-500,-300)
)


# Show 95% confidence interval


plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-500,-300),
           custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
           color_palette =  c("#4DAF4A","#EA2721","#000000"),
           show_conf_interval = TRUE
)



# S Make baseline correction


plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-100,100),
           custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
           color_palette =  c("#4DAF4A","#EA2721","#000000"),
           adjusted_baseline = TRUE
)



#  show check message

plot_erp ( data = relpriming,
           conditionToPlot = Pair.Type,
           electrodes_list =  c("F3", "Fz", "F4",
                                "C3", "Cz","C4",
                                "P3", "Pz", "P4"),
           baseline = c(-100,100),
           custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
           color_palette =  c("#4DAF4A","#EA2721","#000000"),
           show_check_message =   TRUE
)


################################# plot_difference function

#  simple difference wave with voltage difference maps

plot_difference (
  data = relpriming,
  conditionToPlot = Pair.Type,
  levelA = Unrelated ,
  levelB = Consistent,
  electrodes_to_display =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
  baseline = c(-500,-300),
  topoplots_time_windows = list(c(50,200),c(200,300),c(300,500),c(500,700),c(700,900))
)

#  simple difference wave with t-test and subject data


plot_difference (
  data = relpriming,
  conditionToPlot = Pair.Type,
  levelA = Unrelated ,
  levelB = Consistent,
  electrodes_to_display =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
  baseline = c(-500,-300),
  topoplots_time_windows = list(c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
  show_t_test = TRUE, t_test_threshold = 0.01,
  show_group_obs = T
  )

#  simple difference wave with t-test and subject data

plot_difference (
  data = relpriming,
  conditionToPlot = Pair.Type,
  levelA = Unrelated ,
  levelB = Consistent,
  custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
  electrodes_to_display =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
  baseline = c(-500,-300),
  topoplots_time_windows = list(c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
  show_t_test = TRUE, t_test_threshold = 0.01,
  topoplots_data = 'voltage_difference',

)

