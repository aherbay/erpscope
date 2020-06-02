

# function generate_ERP_stats_table

# input : dataset , time_step, min_time, max_time, DV, model_structure, model_type, output_name

# output:  html file with the models



# Example: 

# generate_ERP_stats_table( data = pop2_B1, time_step=50, min_time=-250, max_time=700,
#                          model_structure = "Voltage ~ MM_RAW * anteriority_3l.z *  mediality_a.z  + (1+MM_RAW|Subject)",
#                          model_type="lm",
#                          output_name="MM_RAW.lm.-250ms_700B1.html")



generate_ERP_stats_table <- function (data, time_step, min_time, max_time, model_structure, model_type, output_name) {
  
  if((max_time - min_time)%%time_step != 0){
    stop('max_time should be a multiple of the time_step')
  }
  
  #create models lists
  models <- list()
  anovas <- list()
  anova_tables <- list()
  model_index <- 1
  models_names <- c()
  
  # for each time window
  for(lowBound in seq(min_time, max_time-time_step, by=time_step)) {
    #
    print(paste("Processing ",lowBound, "ms - ", Sys.time()))
    models[[model_index]] = lmer(as.formula(model_structure),  data= subset(data, Time > lowBound & Time < (lowBound+time_step) ))
    models_names[model_index] <- paste(as.character(lowBound),"-",as.character(lowBound+time_step),"ms",sep="")
    
    if(model_type=="lm") {
      #summary(models[[model_index]])
    } else {
      if (model_type=="anova") {
        anovas[[model_index]] <- anova(models[[model_index]])
      }
    }
    model_index <- model_index +1
    
  }
  
  # generate table 
  if(model_type=="lm") {
    #print(models_names)
    saveRDS(models, paste(output_name, "current_lm_models.RDS"))
    
    for(i in 1:length(models)){
      class(models[[i]]) <- "lmerMod"
    }
    
    default_arg <- list( type="html",
                         out= output_name,
                         intercept.bottom = F,
                         intercept.top = T,
                         column.labels= models_names,
                         digits=2,
                         model.names = T,
                         single.row = T,
                         star.cutoffs = c(0.05, 0.01, 0.001)
    )
    arguments_list  <-  c(models, default_arg)
    #do.call("stargazer",arguments_list)
    stargazer(models,type="html",
              out= output_name,
              intercept.bottom = F,
              intercept.top = T,
              column.labels= models_names,
              digits=2,
              model.names = T,
              single.row = T,
              star.cutoffs = c(0.05, 0.01, 0.001))
  }
  
  
  if(model_type=="anova") {
    
    #print(models_names)
    for(i in 1:length(models)){
      print(anovas[[i]])
      
      #cat(paste("\n",models_names[i],"\n"), file = paste(output_name,".html",sep = ""), append=TRUE)
      anova_tables[[i]] <- kable(anovas[[i]], digits = 4, format = "html", caption = models_names[i])
      ##%>%
      #cat(., file = paste(output_name,".html",sep = ""), append=TRUE)
    }
    saveRDS(anova_tables, "00_ResourceScripts/anova_tables.RDS")
    render("00_ResourceScripts/prepare_anova_output.Rmd", output_file = paste(output_name,".html",sep=""),params = list(anovas=anova_tables))
  }
  
}




