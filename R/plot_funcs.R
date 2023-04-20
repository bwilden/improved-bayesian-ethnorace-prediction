





make_bper_plot <- function(bper_df, plot_metric) {
  plot_df <- bper_df %>% 
    filter(metric == plot_metric)
  
  plot_df <- plot_df %>% 
    arrange(value) %>% 
    mutate(id = row_number()) %>% 
    pivot_longer(cols = c(where(is.character), -metric), 
                 names_to = "test_type", values_to = "dash") %>% 
    mutate(test_type = factor(test_type, 
                              levels = rev(c(
                                "State",
                                "County",
                                "ZIP",
                                "Place",
                                "Tract",
                                "Block",
                                "Age-Sex",
                                "No Age-Sex",
                                "Party",
                                "No Party",
                                "Urban-Rural All",
                                "Urban",
                                "Rural",
                                "Income Level All",
                                "Income Level High",
                                "Income Level Low",
                                "ForeignBorn Level All",
                                "ForeignBorn Level High",
                                "ForeignBorn Level Low",
                                "College Level All",
                                "College Level High",
                                "College Level Low"
                              ))))
  
  y_axis_title <- case_when(str_detect(plot_metric, "accuracy") ~ "Accuracy",
                            str_detect(plot_metric, "prec") ~ "Precision",
                            str_detect(plot_metric, "rec") ~ "Recall")
  
  p1 <- plot_df %>% 
    ggplot(aes(x = id, y = value)) +
    geom_point(fill = "#178f92", color = "#178f92", size = .75) +
    theme_minimal() +
    labs(x = "", y = y_axis_title) +
    theme(legend.position = "none",
          text = element_text(family = "serif"))
  
  p2 <- plot_df %>% 
    filter(!grepl("All", test_type),
           !grepl("No", test_type),
           !grepl("Party", test_type),
           !grepl("Age-Sex", test_type),
           !(test_type %in% c("State", "ZIP")),
           !is.na(test_type)) %>% 
    ggplot(aes(x = id, y = test_type)) +
    geom_text(aes(label = dash), size = 4, alpha = .5,
              color = "#178f92") +
    theme_minimal() +
    labs(x = "", y = "Data Subset") +
    theme(legend.position = "none",
          text = element_text(family = "serif"))
  
  p <- p1 / p2
  
  return(p)
}


make_all_bper_plots <- function(bper_df) {
  metric_list <- unique(bper_df$metric)
  bper_plots <- map(.x = metric_list, 
                          .f = make_bper_plot,
                          bper_df = bper_df)
  names(bper_plots) <- metric_list
  return(bper_plots)
}

make_comparison_plot <- function(comparisons_df, plot_metric) {
  if (plot_metric == "all") {
    plot_df <- comparisons_df
  } else {
    plot_df <- comparisons_df %>% 
      filter(metric == plot_metric) 
  }
  
  # if (plot_party == "party") {
  #   plot_df <- plot_df %>% 
  #     filter(Party == "|")
  # } else if (plot_party == "no_party") {
  #   plot_df <- plot_df %>% 
  #     filter(Party == "")
  # }
  # if (plot_age_sex == "age_sex") {
  #   plot_df <- plot_df %>% 
  #     filter(`Age-Sex` == "|")
  # } else if (plot_age_sex == "no_age_sex") {
  #   plot_df <- plot_df %>% 
  #     filter(`Age-Sex` == "")
  # }
  plot_df <- plot_df %>% 
    arrange(bper_diff) %>% 
    mutate(id = row_number()) %>% 
    pivot_longer(cols = c(where(is.character), -metric), 
                 names_to = "test_type", values_to = "dash") %>% 
    mutate(test_type = factor(test_type, 
                              levels = rev(c(
                                "County",
                                "Place",
                                "Tract",
                                "Block",
                                "Age-Sex",
                                "No Age-Sex",
                                "Party",
                                "No Party",
                                "Urban-Rural All",
                                "Urban",
                                "Rural",
                                "Income Level All",
                                "Income Level High",
                                "Income Level Middle",
                                "Income Level Low",
                                "ForeignBorn Level All",
                                "ForeignBorn Level High",
                                "ForeignBorn Level Middle",
                                "ForeignBorn Level Low",
                                "College Level All",
                                "College Level High",
                                "College Level Middle",
                                "College Level Low"
                              ))),
           bper_lose = ifelse(bper_diff < 0, "1", "0"))
  
  p1 <- plot_df %>% 
    ggplot(aes(x = id, y = bper_diff / length(unique(test_type)), 
               fill = bper_lose, color = bper_lose)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_fill_manual(values = rev(met.brewer("Isfahan1", 2))) +
    scale_color_manual(values = rev(met.brewer("Isfahan1", 2))) +
    labs(x = "", y = "Difference from wru") +
    theme(legend.position = "none",
          text = element_text(family = "serif"))
  
  p2 <- plot_df %>% 
    filter(!grepl("All", test_type),
           !grepl("No", test_type),
           !grepl("Middle", test_type),
           !grepl("Party", test_type),
           !grepl("Age-Sex", test_type),
           !is.na(test_type)) %>% 
    ggplot(aes(x = id, y = test_type, color = bper_lose)) +
    geom_text(aes(label = dash), size = 4) +
    theme_minimal() +
    scale_color_manual(values = rev(met.brewer("Isfahan1", 2))) +
    labs(x = "", y = "Data Subset") +
    theme(legend.position = "none",
          text = element_text(family = "serif"))
  
  p <- p1 / p2
  
  return(p)
}

make_all_comparison_plots <- function(comparisons_df) {
  arg_list <- tibble(plot_metric = unique(comparisons_df$metric))
  comparison_plots <- pmap(arg_list, 
                           make_comparison_plot,
                           comparisons_df = comparisons_df)
  names(comparison_plots) <- arg_list %>% unite(col = metric) %>% pull(metric)
  return(comparison_plots)
}

make_calibration_plots <- function(cal_plot_data) {
  p <- cal_plot_data %>% 
    mutate(race = case_when(race == "aapi" ~ "Asian",
                            TRUE ~ str_to_title(race))) %>% 
    ggplot(aes(x = midpoint, y = Percent, color = method)) +
    geom_line() +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = .25) +
    scale_color_manual(values = rev(met.brewer("Isfahan1", 2))) +
    xlim(0, 100) +
    ylim(0, 100) +
    theme_minimal() +
    labs(y = "Observed Percent", x = "Predicted Probability Bin Midpoint") +
    facet_grid(method ~ race) +
    theme(legend.position = "none",
          text = element_text(family = "serif"))
  return(p)
}
