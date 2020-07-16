# ERPscope

A little package to visualize ERPs in R

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

## Specification of the R dataframe:
* one column named Voltage
* one column named Subject
* one column named Time
* one column named Electrode
* one column with the condition to plot

## Examples

### function plot_erp with all arguments

```r
plot_erp( data = relpriming,
          conditionToPlot = "Pair.Type",
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

### function plot_erp with only necessary arguments

```r
plot_erp( data = relpriming,
          conditionToPlot = "Pair.Type",
          electrodes_list =  c("F3", "Fz", "F4","C3", "Cz","C4", "P3", "Pz", "P4"),
          color_palette =  c("#4DAF4A","#EA2721","#000000")  ,
          custom_labels = list(list(-450,-250,"Prime"),list(0,200,"Target")),
          baseline = c(-500,-300),
) 
```


<img src="man/figures/plot_erp_1.png" width="100%" />


### function plot_difference with all arguments displaying 9 electrodes and voltages maps
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


### function plot_difference with all arguments displaying 9 ROI and voltages maps
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

