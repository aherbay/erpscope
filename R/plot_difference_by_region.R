# plot_diff_by_region(data= subset(datatest, (Trigger_code %in% c(189,199))),
#                     plotname= paste("ERP_[-500-200]_Verb_198_40mv"),
#                     levelA= semMM_RAW ,
#                     levelB= consistent ,
#                     group_var = Trigger_code,
#                     show_group_obs = TRUE,
#                     conditionToPlot=MM_RAW,
#                     color_palette=c("#4DAF4A","#000000","#377EB8"))  




  plot_diff_by_region<- function( data,
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
                                  plot_title=" ") {
    
    conditionToPlot_enq <- enquo(conditionToPlot)
    levelA_enq <- enquo(levelA)
    levelB_enq <- enquo(levelB)
    vary_enq <- enquo(vary)
    group_var_enq <- enquo(group_var)
    med_levels_enq <- enquo(med_levels)
    ant_levels_enq <- enquo(ant_levels)
    print("Starting")
    
    data_reduced <- data %>% dplyr::select( !! group_var_enq, Time, Electrode , !! vary_enq ,!! conditionToPlot_enq,!! ant_levels_enq,!! med_levels_enq)  
    print("Data selected")
    data_reduced <- filter(data_reduced, !! conditionToPlot_enq %in% c(rlang::quo_text(levelA_enq), rlang::quo_text(levelB_enq)) ) %>% droplevels()
    #print(levels(data_reduced$MM_RAW))
    print(head(data_reduced))
  
    data_diff <- data_reduced %>%
      group_by( !! group_var_enq, Time, Electrode,!! ant_levels_enq,!! med_levels_enq, !! conditionToPlot_enq) %>%
      summarise(mean_Voltage = mean(!! vary_enq))
    
    print(head(data_diff))
    
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
      
      data_diff <- data_diff %>% 
        spread( !! conditionToPlot_enq, mean_Voltage )  %>%  
        mutate( Voltage = !!levelA_enq - !!levelB_enq)
      print(head(data_diff))
      
      
    }
    
    data_reduced <- rename(data_reduced,   Condition = !! conditionToPlot_enq) 
    
    data_diff$Condition <- "Difference"
    
    #topo_ggplots <- plot_topoplots_by_TW(data_diff,  50, 200, 600, plotname)
    #my_tw <- list(c(-150,50),c(50,250),c(250,400),c(400,550),c(550,800),c(800,900))
    my_tw <- list(c(-150,50),c(50,200),c(200,400),c(400,500),c(500,600),c(800,900))
    
    topo_ggplots <- plot_topoplots_by_custom_TW (data_diff, my_tw, plotname)
  
  if(show_group_obs) {
  
    erp_plot <-  ggplot(data_reduced,aes_string(x= "Time", y= "Voltage" )) + 
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

        erp_plot <- ggplot(data_reduced,aes_string(x= "Time", y= "Voltage" )) + 
        scale_y_reverse() + theme_light() + theme(legend.position="bottom") +
        #stat_summary(data = data_diff,fun.y=mean,geom = "line",aes_string(group = group_var,colour = "Condition"),alpha = 0.1)+ # by subject line
        stat_summary(data = data_diff,fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.3, aes(fill = Condition))+ # CI ribbon
        stat_summary(fun = mean,geom = "line",size = .75, aes(colour = Condition) )+ # conditions lines
        stat_summary(data = data_diff,fun=mean,geom = "line", aes(colour = Condition))+ # difference line
        labs(x = "Time (in ms)",
             y = bquote(paste("Voltage amplitude (", mu, "V): ", .("Voltage"))))+
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
    
      

     
    }
  
    print("assembling topoplot")
    topoplot <- ggarrange(plotlist=topo_ggplots, nrow = 1, ncol = 6)
    
    print("assembling all plots")
    
    figure  <- ggarrange( erp_plot, topoplot, heights = c(2, 0.5),
                          #labels = c("ERPs", "Voltage maps"),
                          ncol = 1, nrow = 2)
    
    figure  <- annotate_figure(figure,
                               top = text_grob(paste(plot_title, ":: Difference ",rlang::quo_text(levelA_enq)," - ", rlang::quo_text(levelB_enq),"(",rlang::quo_text(conditionToPlot_enq),")"), color = "black", face = "bold", size = 16))
    print("creating file")
    ggsave(plot= figure ,filename=paste(plotname,'.pdf', sep=''), width = 22, height = 18)
  
  
  }
  
  #plot_diff_by_region (data=data, plotname = "essai3", color_palette=  c("#4DAF4A","#000000","#377EB8") )
  
  library(tidyverse)
  library(akima)
  library(scales)
  library(mgcv)
  library(gridExtra)
  library(png)
  library(grid)
  library(ggpubr)
  
  
  plot_topoplots_by_TW <- function (data_diff, 
                                    time_step, 
                                    min_time, 
                                    max_time,
                                    plotname) {
  
     electrodeLocs <- readRDS("electrodeLocs_63electrodes_SteinhauerLab.RDS")
     
    electrodeLocs <- rename(electrodeLocs,   Electrode = electrode) 
    electrodeLocs <- subset(electrodeLocs, !(Electrode %in% c("AFz","PO5","PO6") ))
  
    print(head(electrodeLocs))
  
    # check if max_time - min_time is a multiple of time_step                           
    if((max_time - min_time)%%time_step != 0){
        stop('max_time should be a multiple of the time_step')
      }
    
  
    # base elements for topo
  
            jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  
  
            circleFun <- function(center = c(0,0),diameter = 1, npoints = 100) {
                  r = diameter / 2
                  tt <- seq(0,2*pi,length.out = npoints)
                  xx <- center[1] + r * cos(tt)
                  yy <- center[2] + r * sin(tt)
                  return(data.frame(x = xx, y = yy))
              }
            
            maskRing <- circleFun(diameter = 1.42) #create a circle round the outside of the plotting area to mask the jagged edges of the interpolation
            headShape <- circleFun(c(0, 0), round(max(electrodeLocs$x)), npoints = 100) # 0
            nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))
            
            
            theme_topo <- function(base_size = 12)
              {
              theme_bw(base_size = base_size) %+replace%
                  theme(
                        rect             = element_blank(),
                        line             = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank()
                      )
            }
  
    # for each time window
            
    topo_ggplots <- list()
    electro_ggplots <- list()
  
    for(lowBound in seq(min_time, max_time-time_step, by=time_step)) {
      
          # compute mean diff voltage for each electrode for the given time window
          means_by_electrodes <- data_diff %>% 
                                  filter ( Time > lowBound  & Time < lowBound + time_step ) %>%   
                                  drop_na() %>%
                                  group_by(Electrode)%>% 
                                  summarise(Voltage= mean(Voltage) )
    
          # preprocess the means voltage
  
  
          # to solve : 1: Column `Electrode` joining factor and character vector, coercing into character vector
          #electrodeLocs$Electrode <- as.factor(electrodeLocs$Electrode)
          means_by_electrodes <- means_by_electrodes %>% right_join(electrodeLocs, by = "Electrode")
          print(head(means_by_electrodes))
  
            gridRes <- 250 # Specify the number of points for each grid dimension i.e. the resolution/smoothness of the interpolation
  
            tmpTopo <- with(means_by_electrodes,
                            akima::interp(x = x, y = y, z = Voltage,
                         xo = seq(min(x)*2,
                                  max(x)*2,
                                  length = gridRes),
                         yo = seq(min(y)*2,
                                  max(y)*2,
                                  length = gridRes),
                         linear = FALSE,
                         extrap = TRUE)
                  ) 
  
            interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)
  
            names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y
  
            interpTopo <- gather(interpTopo,
                                key = y,
                                value = Voltage,
                                -x,
                                convert = TRUE)
  
            interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle
  
            interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle
  
          # plot it
  
            
            # electrodes plots
            electro_ggplots[[length(topo_ggplots) + 1]] <-  ggplot(headShape,aes(x,y))+
              geom_path(size = 1.5)+
              geom_point(data = means_by_electrodes,aes(x,y,colour = Voltage),size = 3)+
              scale_colour_gradientn(colours = jet.colors(10),guide = "colourbar",oob = squish,limits = c(-2,2))+ #note: oob = squish forces everything outside the colour limits to equal nearest colour boundary (i.e. below min colours = min colour)
              geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
              theme_topo()+
              coord_equal()+
              labs(title = paste(lowBound,'-',lowBound+time_step))
            #ggsave(filename=paste("electro",'_',lowBound,'-',lowBound+time_step,'.pdf', sep=''), width = 10, height = 10)
  
  
          topo_ggplots[[length(topo_ggplots) + 1]] <-   ggplot(interpTopo,
                      aes(x = x, y = y, fill = Voltage)
                      ) +
                      geom_raster(show.legend=F) +
                      stat_contour(aes(z = Voltage),
                                  colour = "black",
                                  binwidth = 0.5) +
                      theme_topo()+
                      scale_fill_gradientn(colours = jet.colors(10),
                                          limits = c(-2,2),
                                          guide = FALSE, #"colourbar"
                                          oob = squish) + 
                      geom_path(data = maskRing,
                                aes(x, y, z = NULL, fill =NULL),
                                colour = "white",
                                size = 6)+
                      geom_point(data = means_by_electrodes,
                                aes(x, y),
                                size = 1)+
                      geom_path(data = headShape,
                                aes(x, y, z = NULL, fill = NULL),
                                size = 1.5)+
                      geom_path(data = nose,
                                aes(x, y, z = NULL, fill = NULL),
                                size = 1.5)+
                      coord_equal()+theme(plot.title = element_text(hjust = 0.5))+
                      labs(title = paste(lowBound,'-',lowBound+time_step))
              # display it
            #ggsave(filename=paste("topo",'_',lowBound,'-',lowBound+time_step,'.pdf', sep=''), width = 10, height = 10)
    }
  
    
    #ggarrange(plotlist=topo_ggplots, nrow = 1, ncol = 6)
    
    # ggsave(filename=paste(plotname, "topo",'_',min_time,'-',max_time,'.pdf', sep=''), width = 20, height = 8)
    
    
    #ggarrange(plotlist=electro_ggplots, nrow = 4, ncol = 10)
    #ggsave(filename=paste("electro",'_',min_time,'-',max_time,'.pdf', sep=''))
    return(topo_ggplots)
    
  }
  
  
  plot_topoplots_by_custom_TW <- function (data_diff, 
                                    tw_array , 
                                    plotname) {
    
    electrodeLocs <- readRDS("electrodeLocs_51elec.RDS")
    
    electrodeLocs <- rename(electrodeLocs,   Electrode = electrode) 
    electrodeLocs <- subset(electrodeLocs, !(Electrode %in% c("AFz","PO5","PO6") ))
    
    print(head(electrodeLocs))
    
    # base elements for topo
    
    jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    
    
    circleFun <- function(center = c(0,0),diameter = 1, npoints = 100) {
      r = diameter / 2
      tt <- seq(0,2*pi,length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    
    maskRing <- circleFun(diameter = 1.42) #create a circle round the outside of the plotting area to mask the jagged edges of the interpolation
    headShape <- circleFun(c(0, 0), round(max(electrodeLocs$x)), npoints = 100) # 0
    nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))
    
    
    theme_topo <- function(base_size = 12)
    {
      theme_bw(base_size = base_size) %+replace%
        theme(
          rect             = element_blank(),
          line             = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
        )
    }
    
    # for each time window
    
    topo_ggplots <- list()
    electro_ggplots <- list()
     

    
    
    for( tw_index in 1:length(tw_array)) {
      


      lowBound <- tw_array[[tw_index]][1]
      upperBound <- tw_array[[tw_index]][2]

      print(paste('Processing',lowBound,"-",upperBound))


      # compute mean diff voltage for each electrode for the given time window
      means_by_electrodes <- data_diff %>% 
        filter ( Time > lowBound  & Time < upperBound ) %>%   
        drop_na() %>%
        group_by(Electrode)%>% 
        summarise(Voltage= mean(Voltage) )
      
      # preprocess the means voltage
      
      #print(head(means_by_electrodes))
      
      # to solve : 1: Column `Electrode` joining factor and character vector, coercing into character vector
      #electrodeLocs$Electrode <- as.factor(electrodeLocs$Electrode)
      means_by_electrodes <- means_by_electrodes %>% right_join(electrodeLocs, by = "Electrode")
      #print(head(means_by_electrodes))
      
      gridRes <- 250 # Specify the number of points for each grid dimension i.e. the resolution/smoothness of the interpolation
      
      tmpTopo <- with(means_by_electrodes,
                      akima::interp(x = x, y = y, z = Voltage,
                                    xo = seq(min(x)*2,
                                             max(x)*2,
                                             length = gridRes),
                                    yo = seq(min(y)*2,
                                             max(y)*2,
                                             length = gridRes),
                                    linear = FALSE,
                                    extrap = TRUE)
      ) 
      
      interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)
      
      names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y
      
      interpTopo <- gather(interpTopo,
                           key = y,
                           value = Voltage,
                           -x,
                           convert = TRUE)
      
      interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle
      
      interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle
      
      # plot it
      
      
      # electrodes plots
      #electro_ggplots[[length(topo_ggplots) + 1]] <-  ggplot(headShape,aes(x,y))+
       # geom_path(size = 1.5)+
       # geom_point(data = means_by_electrodes,aes(x,y,colour = Voltage),size = 3)+
       # scale_colour_gradientn(colours = jet.colors(10),guide = "colourbar",oob = squish,limits = c(-2,2))+ #note: oob = squish forces everything outside the colour limits to equal nearest colour boundary (i.e. below min colours = min colour)
       # geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
       # theme_topo()+
       # coord_equal()+
       # labs(title = paste(lowBound,'-',upperBound))
      
      
      topo_ggplots[[length(topo_ggplots) + 1]] <-   ggplot(interpTopo,
                                                           aes(x = x, y = y, fill = Voltage)
      ) +
        geom_raster(show.legend=F) +
        stat_contour(aes(z = Voltage),
                     colour = "black",
                     binwidth = 0.5) +
        theme_topo()+
        scale_fill_gradientn(colours = jet.colors(10),
                             limits = c(-2,2),
                             guide = FALSE, #"colourbar"
                             oob = squish) + 
        geom_path(data = maskRing,
                  aes(x, y, z = NULL, fill =NULL),
                  colour = "white",
                  size = 15)+
        geom_point(data = means_by_electrodes,
                   aes(x, y),
                   size = 1)+
        geom_path(data = headShape,
                  aes(x, y, z = NULL, fill = NULL),
                  size = 1.5)+
        geom_path(data = nose,
                  aes(x, y, z = NULL, fill = NULL),
                  size = 1.5)+
        coord_equal()+theme(plot.title = element_text(hjust = 0.5))+
        labs(title = paste(lowBound,'-',upperBound))
      
    }
    
    
    
  
    
    #ggarrange(plotlist=topo_ggplots, nrow = 1, ncol = 6)
    
    # ggsave(filename=paste(plotname, "topo",'_',min_time,'-',max_time,'.pdf', sep=''), width = 20, height = 8)
    
    
    #ggarrange(plotlist=electro_ggplots, nrow = 4, ncol = 10)
    #ggsave(filename=paste("electro",'_',min_time,'-',max_time,'.pdf', sep=''))
    return(topo_ggplots)
    
  }
  
