
# plot_relation_with_cor

# example 
# plot_relation_with_cor(    dataset=byParticipantEffects, 
#                             varx="Mean_RT_correct",
#                             vary="score", 
#                             var_label="Subject_short", 
#                             graph_title_header ="Simple RT effects", 
#                             var_color=NULL)


# plot_relation_with_cor(dataset, varx, vary, var_label, graph_title_header, var_color=NULL)

plot_relation_with_cor <- function (dataset, varx, vary, var_label, graph_title_header, var_color=NULL) {
  
  graphName <- paste(graph_title_header,varx,"-",vary,'.pdf',sep='')
  
  cor_result <- cor.test(dataset[,varx], dataset[,vary] )
  print(cor_result)
  
  dataset[,var_label] <- as.factor(dataset[,var_label])
  
  cor_label <- paste("r= ",round(cor_result[["estimate"]],digits = 3)," (p=",round(cor_result[["p.value"]], digits=3),")", sep="" )
  
  pdf(graphName ,width=11, height=8)

    
  position_nudge_x = (max(dataset[,varx]) - min(dataset[,varx])) *0.02
  position_nudge_y = (max(dataset[,vary]) - min(dataset[,vary])) *0.02

  if(is.null(var_color)){
    
    currentPlot <- ggplot(dataset, aes(x = dataset[,varx], y = dataset[,vary] )) +
        geom_point()+
        labs(y=vary, x=varx,title= paste(graph_title_header," - ", varx, vary) )+
        geom_vline(xintercept = 0, linetype="solid")+ theme_light()+
        geom_hline(yintercept = 0, linetype="solid")+
        geom_label( label=dataset[,var_label], nudge_y = position_nudge_y, nudge_x = position_nudge_x )+
        xlim(min(dataset[,varx])-sd(dataset[,varx]),max(dataset[,varx])+sd(dataset[,varx]))+
        ylim(min(dataset[,vary])-sd(dataset[,vary]),max(dataset[,vary])+sd(dataset[,vary]))+
        geom_smooth(method='lm')+ 
        annotate("text", x = min(dataset[,varx]), y = mean(dataset[,vary]), label = cor_label)

  } else {

    currentPlot <- ggplot(dataset, aes(x = dataset[,varx], y = dataset[,vary], color=dataset[,var_color] )) +
        geom_point()+
        labs(y=vary, x=varx,title= paste(graph_title_header," - ", varx, vary) )+
        geom_vline(xintercept = 0, linetype="solid")+ theme_light()+
        geom_hline(yintercept = 0, linetype="solid")+
        geom_label( label=dataset[,var_label], nudge_y = position_nudge_y , nudge_x = position_nudge_x )+
        xlim(min(dataset[,varx])-sd(dataset[,varx]),max(dataset[,varx])+sd(dataset[,varx]))+
        ylim(min(dataset[,vary])-sd(dataset[,vary]),max(dataset[,vary])+sd(dataset[,vary]))+
        geom_smooth(method='lm')+ 
        annotate("text", x = min(dataset[,varx]), y = mean(dataset[,vary]), label = cor_label)+
        scale_color_distiller(palette = "RdBu",name=var_color)

  }
  
  print(currentPlot)
  
  dev.off()
  
  
}

