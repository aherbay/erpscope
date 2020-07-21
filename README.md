<img src="man/figures/logo_ERPscope.png" align="right" height="200/" />

# ERPscope

A little package to visualize ERPs in R

## Table of Contents

- [Installation](#installation)
- [Update ERPscope](#update-erpscope)
- [Data specifications](#data-specifications)
- [Function plot_erp](#plot_erp)
- [Function plot_difference](#plot_difference)
- [Function plot_difference_maps](#plot_difference_maps)
- [Function generate_ERP_stats_table](#generate_ERP_stats_table)

## Installation

### Devtools package
To install ERPscope, install the devtools package if you don't already have it (https://www.rdocumentation.org/packages/devtools)
```r
install.packages("devtools")
```
### Installing ERPscope
Run the following command to install from the github repository
```r
 devtools::install_github("aherbay/erpscope")
```

## Update ERPscope

```r
  detach("package:erpscope", unload=TRUE) #if ERPscope is loaded in your session
  devtools::install_github("aherbay/erpscope") # update
  library(erpscope) # loading again
```

## Data specifications

Your dataframe should have:
* one column named Voltage
* one column named Subject
* one column named Time
* one column named Electrode
* one column with the variable and conditions(levels) to plot

## Function plot_erp 

* function plot_erp with all arguments

```r
plot_erp( data = relpriming,
          conditionToPlot = Pair.Type,
          electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
          output_type = "pdf",
          color_palette =  c("#4DAF4A","#EA2721","#000000")  ,
          custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
          show_conf_interval = FALSE,
          baseline = c(-500,-300),
          adjusted_baseline = FALSE,
          time_labels_interval = 200,
          plotname = 'auto',
          show_check_message = FALSE,
          labels_vertical_position = 'auto',
          labels_height = 'auto',
          vary ="Voltage"
) 
```

* function plot_erp with only necessary arguments

```r
plot_erp( data = relpriming,
          conditionToPlot = Pair.Type,
          electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
          color_palette =  c("#4DAF4A","#EA2721","#000000")  ,
          custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
          baseline = c(-500,-300),
) 
```

![alt text](man/figures/plot_erp_1.png "plot_erp_1.png")


*  plot_erp with confidence interval ribbons for each condition

To show confidence interval ribbons, just set the argument  *show_conf_interval* to *TRUE* as below:
```r
plot_erp( data = relpriming,
          conditionToPlot = Pair.Type,
          electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
          color_palette =  c("#4DAF4A","#EA2721","#000000")  ,
          custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
          baseline = c(-500,-300),
          show_conf_interval = FALSE

) 
```
<img src="man/figures/plot_erp_2.png" width="100%" />

## Function plot_difference 

* function plot_difference with all arguments displaying 9 electrodes and voltages maps

```r
plot_difference( data = relpriming,
                 conditionToPlot = Pair.Type,
                 levelA = Unrelated ,
                 levelB = Consistent,
                 color_palette =  c("#4DAF4A","#595959", "#000000"),
                 output_type ='pdf',
                 ant_levels= Anteriority.Levels,
                 med_levels= Mediality.Levels,
                 vary= Voltage,
                 group_var = Subject,
                 show_group_obs = TRUE ,
                 labels_vertical_position = 'auto',
                 labels_height = 'auto',
                 baseline= c(-500,-200),
                 topoplots_time_windows = list(c(-250,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
                 topoplots_scale = c(-2,2),
                 time_labels_interval = 200,
                 custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
                 electrodes_to_display = c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
                 plotname = 'auto'
) 
```

<img src="man/figures/plot_difference_Electrodes.png" width="100%" />


* function plot_difference to display Subject individual data

You can display Subject data in light grey with show_group_obs = TRUE 

```r
plot_difference(  data = relpriming,
                  conditionToPlot = Pair.Type,
                  levelA = Unrelated ,
                  levelB = Consistent,
                  color_palette =  c("#4DAF4A","#595959", "#000000"),
                  output_type ='pdf',
                  ant_levels= Anteriority.Levels,
                  med_levels= Mediality.Levels,
                  vary= Voltage,
                  group_var = Subject,
                  show_group_obs = TRUE ,
                  labels_vertical_position = 'auto',
                  labels_height = 'auto',
                  baseline= c(-500,-200),
                  topoplots_time_windows = list(c(-250,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
                  topoplots_scale = c(-2,2),
                  time_labels_interval = 200,
                  custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
                  plotname = 'auto'
) 
```

<img src="man/figures/plot_difference_Electrodes_with_subjectData.png" width="100%" />

* function plot_difference with all arguments displaying 9 ROI and voltages maps
 
 Instead of displaying difference on 9 specific electrodes you can display it on 9 Regions of Interest
 To do so, just remove the line, electrodes_to_display or put it electrodes_to_display = c()

```r
plot_difference(  data = relpriming,
                  conditionToPlot = Pair.Type,
                  levelA = Unrelated ,
                  levelB = Consistent,
                  color_palette =  c("#4DAF4A","#595959", "#000000"),
                  output_type ='pdf',
                  ant_levels= Anteriority.Levels,
                  med_levels= Mediality.Levels,
                  vary= Voltage,
                  group_var = Subject,
                  show_group_obs = FALSE ,
                  labels_vertical_position = 'auto',
                  labels_height = 'auto',
                  baseline= c(-500,-200),
                  topoplots_time_windows = list(c(-250,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
                  topoplots_scale = c(-2,2),
                  time_labels_interval = 200,
                  custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
                  plotname = 'auto'
) 
```
<img src="man/figures/plot_difference_ROI.png" width="100%" />

## Function plot_difference_maps 

* function plot_difference_maps with custom time windows

```r
  plot_difference_maps(  data = relpriming,
                         conditionToPlot = Pair.Type,
                         levelA = Unrelated ,
                         levelB = Consistent,
                         output_type ='png',
                         topoplots_time_windows = list(c(-250,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700)),
                         topoplots_scale = c(-2,2),
                         plotname = 'auto'
  )
```

<img src="man/figures/2020-07-21_relpriming_11PPTS_ERP_DIFF_Pair.Type_Unrelated-Consistent.png" width="100%" />


* function plot_difference_maps with fixed time windows

Precise in the fixed argument the start_time, end_time and time duration of your windows


```r
  plot_difference_maps(  data = relpriming,
                         conditionToPlot = Pair.Type,
                         levelA = Unrelated ,
                         levelB = Consistent,
                         output_type ='png',
                         fixed= c(-300,900,100), # init_time, end_time, step
                         topoplots_scale = c(-2,2),
                         plotname = 'auto'
  )
```

<img src="man/figures/2020-07-21_relpriming_11PPTS_ERP_DIFF_Pair.Type_Unrelated-Consistentfixed100_-300_900.png" width="100%" />




## Function generate_ERP_stats_table 

```r
 generate_ERP_stats_table( data = relpriming,
                                      model_structure = "Voltage ~   Pair.Type * Anteriority.Levels + (1+ Pair.Type|Subject)",
                                      timeWindowMode="custom",
                                      custom_TW =  list(c(-300,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700)),
                                      output_name="2020_07_02_PairTypeModels.html")
) 
```
<img src="man/figures/2020_07_20_PairTypeModels.png" width="100%" />




           
