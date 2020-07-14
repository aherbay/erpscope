
baseline_correction  <- function( data,
                                  conditionToAdjust,
                                  baseline = c(-200,0)) {

  electrode_list <- unique(data$Electrode)
  condition_values <- unique(data[,conditionToAdjust])

  for(curr_elec in 1:length(electrode_list) ) {
    overall_elec_mean_voltage <- mean(data$Voltage[ data$Electrode == electrode_list[curr_elec]  & data$Time> baseline[1] & data$Time < baseline[2]],na.rm=T)
    for(curr_condition in 1:length(condition_values) ) {
      curr_condition_mean  <- mean(data$Voltage[ data$Electrode == electrode_list[curr_elec]  & data$Time> baseline[1] & maxData$Time < baseline[2]  & data[,conditionToAdjust] == condition_values[curr_condition] ])
      diff_curr_condition <- curr_condition_mean - overall_elec_mean_voltage

      data$NewVoltage[electrode_list[curr_elec]  & data[,conditionToAdjust] == condition_values[curr_condition]  ] <- data$Voltage[ electrode_list[curr_elec]  & data[,conditionToAdjust] == condition_values[curr_condition] ] - diff_curr_condition

    }

  }

  return(data)
}
