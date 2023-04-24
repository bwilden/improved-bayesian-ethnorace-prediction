

create_calibration_plot_data <- function(bper_cal, race) {
  cal_formula <- as.formula(paste0("obs_", race, "~", "pred_", race))
  
  cal_data <- caret::calibration(cal_formula, 
                                 data = bper_cal)$data |> 
    as_tibble() |> 
    mutate(race := race)
  
  return(cal_data)
}

make_calibration_plots <- function(cal_plot_data) {
  p <- cal_plot_data %>% 
    mutate(race = case_when(race == "aapi" ~ "Asian",
                            TRUE ~ str_to_title(race))) %>% 
    ggplot(aes(x = midpoint, y = Percent)) +
    geom_line() +
    geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = .25) +
    xlim(0, 100) +
    ylim(0, 100) +
    labs(y = "Observed Percent", x = "Predicted Probability Bin Midpoint") +
    facet_wrap(~ race, nrow = 2)
  return(p)
}
