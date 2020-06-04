
remove_voltage_threshold <- function(data, threshold ) {

  data$outlier <- FALSE

  # extremely high values marking
  extremeHigh <- subset(data, Voltage > threshold)
  summary_byPPT_TG_high <- extremeHigh %>% group_by(Subject, Trigger_code) %>% summarise( meanV = mean(Voltage))
  valuesToProcess <- nrow(summary_byPPT_TG_high)
  for (current_row in 1:valuesToProcess){
    current_ppt <- summary_byPPT_TG_high[[current_row,"Subject"]]
    #print(current_ppt)
    current_TG <- summary_byPPT_TG_high[[current_row,"Trigger_code"]]
    #print(current_TG)
    message(paste(current_row,'of', valuesToProcess))
    data$outlier[ (data$Subject == current_ppt) & (data$Trigger_code == current_TG) ] <- TRUE
  }
# same with extremely low values
extremeLow <- subset(data, Voltage <  -threshold)
summary_byPPT_TG_low <- extremeLow %>% group_by(Subject, Trigger_code) %>% summarise( meanV = mean(Voltage))
valuesToProcess <- nrow(summary_byPPT_TG_low)
for (current_row in 1:valuesToProcess) {
  current_ppt <- summary_byPPT_TG_low[[current_row,"Subject"]]
  #print(current_ppt)
  current_TG <- summary_byPPT_TG_low[[current_row,"Trigger_code"]]
  message(paste(current_row,'of', valuesToProcess))
  data$outlier[ (data$Subject == current_ppt) & (data$Trigger_code == current_TG) ] <- TRUE
}

proportions <- summary(data$outlier)
percentageOfRemoval <- as.integer(proportions[[3]]) / as.integer(proportions[[2]])
message(paste("The threshold",threshold,"microVolt would remove",round(100*percentageOfRemoval,2),"% of the values"))

title_text <- paste("Do you want to remove this data?")
choice <- menu(c("y", "n"), title=title_text)

if(choice == 1) {
  return(subset(data, data$outlier == FALSE))
} else {
  message("Ok, bye!")
}



}

# pop2 <- remove_voltage_threshold(pop2,80)

