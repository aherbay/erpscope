#' Generate color coded stats table for ERPs
#'
#' Description to come
#'
#' @param data dataframe containing eeg data
#' @param model_structure string with the lmer model
#' @param output_name string for the output name
#' @return An HTML file containing thes stats tables
#' @export



generate_ERP_stats_table <- function (data,
                                      model_structure,
                                      output_name,
                                      timeWindowMode,
                                      model_type = "lm",
                                      custom_TW =  list(c(-300,-150),c(-150,50),c(50,200),c(200,300),c(300,500),c(500,700),c(700,900)),
                                      time_step,
                                      min_time,
                                      max_time
) {


  if(timeWindowMode== 'byStep') {
    if((max_time - min_time)%%time_step != 0){
      stop('max_time should be a multiple of the time_step')
    }
  }

  #create models lists
  models <- list()
  anovas <- list()
  anova_tables <- list()
  model_index <- 1
  models_names <- c()
  captures <- c()

  # for each time window
  if(timeWindowMode== 'byStep') {

    for(lowBound in seq(min_time, max_time-time_step, by=time_step)) {
      #
      message(paste(Sys.time(),"- Processing",lowBound, "ms"))
      models[[model_index]] = lmerTest::lmer(as.formula(model_structure),  data= subset(data, Time > lowBound & Time < (lowBound+time_step) ))
      models_names[model_index] <- paste(as.character(lowBound)," to ",as.character(lowBound+time_step),"ms",sep="")

      if(model_type=="lm") {
        #summary(models[[model_index]])
      } else {
        if (model_type=="anova") {
          anovas[[model_index]] <- car::Anova(models[[model_index]])
        }
      }
      model_index <- model_index +1

    }

  } else {

    for(tw in custom_TW) {
      #
      message(paste(Sys.time(),"- Processing",tw[1], "ms to", tw[2],"ms"))
      captures[model_index] <- capture.output( models[[model_index]] <-  lmerTest::lmer(as.formula(model_structure),  data= subset(data, Time > tw[1] & Time < tw[2] )))
      models_names[model_index] <- paste(as.character(tw[1])," to ",as.character(tw[2]),"ms",sep="")
      print(captures[model_index])

      if(model_type=="lm") {
        #summary(models[[model_index]])
        #pdf(paste("Check",models_names[model_index],'.pdf', sep=''))
        #check_figure <- capture.output( performance::check_model(models[[model_index]]) ) # a essayer
        #ggplot2::ggsave(plot= check_figure ,filename=paste("Check",models_names[model_index],'.pdf', sep=''), width = 22, height = 18)
        #dev.off()
        #saveRDS(check_figure,"check_figure.RDS")
      } else {
        if (model_type=="anova") {
          anovas[[model_index]] <- car::Anova(models[[model_index]])
        }
      }
      model_index <- model_index +1

    }

    #print(captures)


  }


  # generate table
  if(model_type=="lm") {
    #print(models_names)
    #saveRDS(models, paste(output_name, "current_lm_models.RDS"))

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
    message(paste(Sys.time(),"- Creating stats summary table"))

    capture <- capture.output(stargazer::stargazer(models,type="html",
                                                   out= output_name,
                                                   intercept.bottom = F,
                                                   intercept.top = T,
                                                   column.labels= models_names,
                                                   digits=2,
                                                   model.names = T,
                                                   single.row = T,
                                                   star.cutoffs = c(0.05, 0.01, 0.001)))
    message(paste(Sys.time(),"- Formating stats summary table"))
    format_stats_table(capture,model_structure,output_name)


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
