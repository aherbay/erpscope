
# plot_relation_with_cor

# example
# plot_relation_with_cor(    dataset=byParticipantEffects,
#                             varx="Mean_RT_correct",
#                             vary="score",
#                             var_label="Subject_short",
#                             graph_title_header ="Simple RT effects",
#                             var_color=NULL,)


# plot_relation_with_cor(dataset, varx, vary, var_label, graph_title_header, var_color=NULL)

plot_relation_with_cor <- function (dataset, varx, vary, var_label, graph_title_header, var_color=NULL, byAnteriority=T) {

  dataset <- as.data.frame(dataset)

  graphName <- paste(graph_title_header,varx,"-",vary,'.pdf',sep='')


  #print(dataset[,varx])
  cor_result <- cor.test(as.vector(dataset[,varx]), as.vector(dataset[,vary]) )
  print(cor_result)


  if (byAnteriority) {
    ant_data <- subset(dataset, dataset$anteriority_3l == "Anterior")
    cent_data <- subset(dataset, dataset$anteriority_3l == "Central")
    post_data <- subset(dataset, dataset$anteriority_3l == "Posterior")
    cor_result_ant <- cor.test(as.vector(ant_data[,varx]), as.vector(ant_data[,vary]) )
    cor_result_cent <- cor.test(as.vector(cent_data[,varx]), as.vector(cent_data[,vary]) )
    cor_result_post <- cor.test(as.vector(post_data[,varx]), as.vector(post_data[,vary]) )

    cor_label_ant <- paste("Anterior r= ",round(cor_result_ant[["estimate"]],digits = 3)," (p=",round(cor_result_ant[["p.value"]], digits=3),")", sep="" )
    cor_label_cent <- paste("Central r= ",round(cor_result_cent[["estimate"]],digits = 3)," (p=",round(cor_result_cent[["p.value"]], digits=3),")", sep="" )
    cor_label_post <- paste("Posterior r= ",round(cor_result_post[["estimate"]],digits = 3)," (p=",round(cor_result_post[["p.value"]], digits=3),")", sep="" )
    print(cor_label_ant)
    print(cor_label_cent)
    print(cor_label_post)
    pdf(graphName ,width=18, height=8) # normall 11 8

  } else {

    pdf(graphName ,width=11, height=8) # normall 11 8
  }

  dataset[,var_label] <- as.factor(dataset[,var_label])

  cor_label <- paste("r= ",round(cor_result[["estimate"]],digits = 3)," (p=",round(cor_result[["p.value"]], digits=3),")", sep="" )

  pdf(graphName ,width=17, height=8) # normall 11 8


  position_nudge_x = (max(dataset[,varx]) - min(dataset[,varx])) *0.02
  position_nudge_y = (max(dataset[,vary]) - min(dataset[,vary])) *0.02

  if(is.null(var_color)){

    currentPlot <- ggplot2::ggplot(dataset,  ggplot2::aes(x = dataset[,varx], y = dataset[,vary] )) +
        geom_point()+
        labs(y=vary, x=varx,title= paste(graph_title_header," - ", varx, vary) )+
        geom_vline(xintercept = 0, linetype="solid")+ theme_light()+
        geom_hline(yintercept = 0, linetype="solid")+
        geom_label( label=dataset[,var_label], nudge_y = position_nudge_y, nudge_x = position_nudge_x )+
        xlim(min(dataset[,varx])-sd(dataset[,varx]),max(dataset[,varx])+sd(dataset[,varx]))+
        ylim(min(dataset[,vary])-sd(dataset[,vary]),max(dataset[,vary])+sd(dataset[,vary]))+
        geom_smooth(method='lm')+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])+1.2), label = cor_label)+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])+0.6), label = cor_label_ant)+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])), label = cor_label_cent)+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])-0.6), label = cor_label_post)+
        scale_y_reverse() +
        facet_wrap(~anteriority_3l)+
        theme(  strip.text.x = element_text( size = 16, color = "black", face = "bold" ),
              strip.background = element_rect( fill="white", color=NA))

  } else {

    currentPlot <- ggplot2::ggplot(dataset,  ggplot2::aes(x = dataset[,varx], y = dataset[,vary], color=dataset[,var_color] )) +
        geom_point()+
        labs(y=vary, x=varx,title= paste(graph_title_header," - ", varx, vary) )+
        geom_vline(xintercept = 0, linetype="solid")+ theme_light()+
        geom_hline(yintercept = 0, linetype="solid")+
        geom_label( label=dataset[,var_label], nudge_y = position_nudge_y, nudge_x = position_nudge_x )+
        xlim(min(dataset[,varx])-sd(dataset[,varx]),max(dataset[,varx])+sd(dataset[,varx]))+
        ylim(min(dataset[,vary])-sd(dataset[,vary]),max(dataset[,vary])+sd(dataset[,vary]))+
        geom_smooth(method='lm')+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])+1.2), label = cor_label)+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])+0.6), label = cor_label_ant)+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])), label = cor_label_cent)+
        annotate("text", x = min(dataset[,varx])+50, y = (max(dataset[,vary])-0.6), label = cor_label_post)+
        scale_y_reverse() +
        scale_color_gradient2(name=var_color,low ="blue", mid = "grey86", high = "red")+ #low = scales::"blue"), mid = "grey50", high = scales::muted("red")
        facet_wrap(~anteriority_3l)+
        theme(  strip.text.x = element_text( size = 16, color = "black", face = "bold" ),
              strip.background = element_rect( fill="white", color=NA))

  }

  print(currentPlot)

  dev.off()


}

