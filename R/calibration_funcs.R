

create_calibration_plot_data <- function(bper_cal, race) {
  cal_formula <- as.formula(paste0("obs_", race, "~", "pred_", race))
  
  cal_data <- caret::calibration(cal_formula, 
                                 data = bper_cal)$data |> 
    as_tibble() |> 
    mutate(race := race)
  
  return(cal_data)
}
# create_calibration_plot_data(bper_cal = bper_cal, "white")
