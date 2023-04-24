

make_metric_plot <- function(metric_df, metric_name) {
  
  metric_df <- metric_df |> 
    mutate(party = ifelse(party == TRUE, "Party", "No Party"))
  
  if (metric_name != "Accuracy") {
    metric_df <- metric_df |> 
      filter(race != "Accuracy")
  }
  
  p <- metric_df |> 
    filter(metric == metric_name,
           race != "Other") |>  
    ggplot(aes(x = geography, y = value, fill = party)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ylim(0, 1) +
    labs(y = metric_name, x = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
    
  if (metric_name != "Accuracy") {
      p <- p + facet_wrap(~ race, nrow = 2)
  }
  
  return(p)
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
