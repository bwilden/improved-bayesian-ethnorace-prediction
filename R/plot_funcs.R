

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

make_bias_prec_plot <- function(pred_df, bias_var) {
  p <- pred_df |> 
    filter(race %in% c("aapi", "black", "hispanic", "white")) |>
    mutate(deciles = ntile(!!sym(bias_var), 100)) |>
    group_by(deciles, race) |>
    summarise(prop_miss = mean(race != pred_race, na.rm =TRUE)) |>
    ggplot(aes(x = deciles, y = prop_miss)) +
    geom_point(color = "grey") +
    geom_smooth(se = FALSE, color = "red") +
    facet_wrap(~race, ncol = 2) +
    ylim(0, 1) +
    labs(x = bias_var)
  return(p)
}

make_bias_recall_plot <- function(pred_df, bias_var, plot_race) {
  p <- pred_df |> 
    filter(race == plot_race, pred_race %in% c("aapi", "black", "hispanic", "white")) |>
    mutate(deciles = ntile(!!sym(bias_var), 100)) |>
    group_by(deciles) |>
    mutate(total = n()) |>
    group_by(deciles, pred_race) |>
    reframe(recall = n()/total) |>
    distinct() |>
    ggplot(aes(x = deciles, y = recall)) +
    geom_point(color = "grey") +
    geom_smooth(se = FALSE, color = "red") +
    facet_wrap(~pred_race, ncol = 2) +
    ylim(0, 1) +
    labs(x = bias_var, subtitle = paste0("Race: ", plot_race))
  return(p)
}

# make_bias_recall_plot(bper_predictions, "median_income", "aapi")

# x = cut(median_income, breaks = seq(min(median_income, na.rm = TRUE),
#                                     max(median_income, na.rm = TRUE), 1000), labels = FALSE))
