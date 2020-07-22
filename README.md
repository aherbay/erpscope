<img src="man/figures/logo_ERPscope.png" align="right" height="200/" />

# ERPscope

A little package to visualize ERPs in R

## Table of Contents

- [Installation](#installation)
- [Update ERPscope](#update-erpscope)
- [Data specifications](#data-specifications)
- [Function plot_erp](#function-plot_erp)
- [Function plot_difference](#function-plot_difference)
- [Function plot_difference_maps](#function-plot_difference_maps)
- [Function generate_ERP_stats_table](#function-generate_ERP_stats_table)

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
* one column named Voltage (numeric)
* one column named Subject (factor)
* one column named Time (integer)
* one column named Electrode (factor)
* one column for the conditionToPlot argument with the variable and levels to plot (factor)

## Function plot_erp 

* **function plot_erp with all arguments**

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

* **function plot_erp with only mandatory arguments**

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


*  **plot_erp with 95% bootstrapped confidence interval ribbons for each condition**

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

### plot_erp functionnalities

* **display ERP baseline** 

For now it is a mandatory argument. Display a red rectangle from the beginning to the end of the baseline time window.

* **change ERP baseline** 

You can adjust the ERPs to the baseline provided in the argument baseline by setting the argument **adjusted_baseline** to **TRUE**

* **add custom labels**

You can add custom labels for stimuli appearing, or specific time window. It appears a vertical dotted line at start time with a grey rectangle with the label on the top of the plot.

To add a label you need to add a list to the **custom_labels** argument (which is a list of lists, by default an empty list). 
The list defining a label has the structure : list(start_time, end_time, label). For example, to add a label displaying the word Prime between -450 ms and -250 ms, you would have : `custom_labels = list( list(-450,-250,"Prime") ) `


The position of label is automatic by default but it can be adjusted to your need by changing the argument **labels_vertical_position**  to the voltage you want the label to be centered on and the argument **labels_height** if you also want to change it's height (again in mV) 

* **output file form** 

By default, the file is exported to a PDF file. It is aslo possible to export it to png, jpeg, pdf, png, svg by changing the argument **output_type** to **"png"** for example.

* **plot name**

By default, the plot name is automatic. But it can be customized by changing the argument **plotname** 

* **show 95% confidence intervals**

By default, only the ERPs are displayed. But it is possible to add the 95% confidence intervals for each condition by setting the argument **show_conf_interval** to **FALSE**.

* **changing the colors of ERP lines**

By default there are 9 colors used to display ERPs : "#4DAF4A" (green), "#EA2721" (red), "#377EB8" (blue), "#FF7F00" (orange), "#984EA3" (purple), "#000000" (black), "#5c5c5c" (grey), "#945D25" (brown),"#FF748C" (pink), "#2E692C" (dark green).

To change the order of colors or to change them entirely, you can change the argument **color_palette**. For example, to put the colors red, blue, purple for 3 ERP lines `color_palette =  c("#EA2721","#377EB8","#984EA3")`. You can also use default values from R for example: `c("blue","red","black")`.


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
                         output_type ='pdf',
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
                         output_type ='pdf',
                         fixed= c(-300,900,100), # init_time, end_time, step
                         topoplots_scale = c(-2,2),
                         plotname = 'auto'
  )
```

<img src="man/figures/2020-07-21_relpriming_11PPTS_ERP_DIFF_Pair.Type_Unrelated-Consistentfixed100_-300_900.png" width="100%" />



## Function generate_ERP_stats_table 

```r
 generate_ERP_stats_table( 
      data = relpriming,
      model_structure = "Voltage ~   Pair.Type * Anteriority.Levels + (1+ Pair.Type|Subject)",
      timeWindowMode="custom",
      custom_TW =  list(c(-300,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700)),
      output_name="2020_07_02_PairTypeModels.html"
 ) 
```
<img src="man/figures/2020_07_20_PairTypeModels.png" width="100%" />




           
