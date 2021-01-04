
arg_max_cols <- function(df, num_cols) {
  cols <- df[tail(seq_along(df), num_cols)]
  max_col <- colnames(cols)[max.col(cols)]
  return(max_col)
}


classify_and_report <- function(df, wru = F, nudge = F, nudge_vec, F1 = F) {
  
  if (wru == TRUE) {
    df <- predict_race_wru(df) %>% 
      mutate(race = if_else(race == "aian", "other", race))
  } else {
    df <- bper::predict_ethnorace(df) %>% 
      mutate(race = if_else(race == "aian", "other", race),
             pred_race = if_else(pred_race == "aian", "other", pred_race))
  }
  
  if (nudge == TRUE) {
    df <- df %>% 
      mutate(prob_api = prob_api + nudge_vec[1],
             prob_black = prob_black + nudge_vec[2],
             prob_hispanic = prob_hispanic + nudge_vec[3],
             prob_other = prob_other + prob_aian + nudge_vec[4],
             prob_white = prob_white + nudge_vec[5]) %>% 
      select(-c(prob_aian, pred_race))
    
    df$pred_race <- arg_max_cols(df, 5)
    
    df <- df %>% 
      mutate(pred_race = case_when(pred_race == "prob_white" ~ "white",
                                   pred_race == "prob_black" ~ "black",
                                   pred_race == "prob_hispanic" ~ "hispanic",
                                   pred_race == "prob_api" ~ "api",
                                   pred_race == "prob_other" ~ "other"))
  }
  
  df <- df %>%
    mutate(
      pred_race = as.factor(pred_race),
      race = as.factor(race)
    )
  
  prop_na <- paste("Proportion missing: ", sum(is.na(df$pred_race)) / nrow(df))
  
  true_prop <- round(prop.table(table(df$race)), digits = 4)
  predicted_prop <- round(prop.table(table(df$pred_race)), digits = 4)
  
  conf_matrix <- confusionMatrix(df$race, df$pred_race, mode = "everything")
  
  # conf_matrix <- list(conf_matrix$overall[1], conf_matrix$table, conf_matrix$byClass[, 1:2])
  
  if (F1 == FALSE) {
    return(list(true_prop, predicted_prop, conf_matrix, prop_na))
  } else {
    f1_sums <- c(sum(conf_matrix$byClass[, 7]), sum(conf_matrix$byClass[2, 7], conf_matrix$byClass[3, 7]), conf_matrix$byClass[5, 7])
    return(f1_sums)
  }
  
}

predict_race_wru <- function(df) {
  
  df_nc <- df %>% 
    filter(state == "NC")
  df_nc <- predict_race(voter.file = df_nc, census.geo = "block", census.data = census.nc, party = "PID", age = F, sex = F)
  
  df_fl <- df %>% 
    filter(state == "FL")
  df_fl <- predict_race(voter.file = df_fl, census.geo = "block", census.data = census.fl, party = "PID", age = F, sex = F)
  
  df <- rbind(df_fl, df_nc)
  
  df$pred_race <- arg_max_cols(df, 5)
  
  df <- df %>% 
    rename(pred_white = pred.whi,
           pred_black = pred.bla,
           pred_hispanic = pred.his,
           pred_api = pred.asi,
           pred_other = pred.oth) %>% 
    mutate(pred_race = case_when(pred_race == "pred.whi" ~ "white",
                                 pred_race == "pred.bla" ~ "black",
                                 pred_race == "pred.his" ~ "hispanic",
                                 pred_race == "pred.asi" ~ "api",
                                 pred_race == "pred.oth" ~ "other"),
           black = factor(if_else(pred_race == "black", 1, 0), levels = c(1, 0)),
           white = factor(if_else(pred_race == "white", 1, 0), levels = c(1, 0)),
           hispanic = factor(if_else(pred_race == "hispanic", 1, 0), levels = c(1, 0)),
           api = factor(if_else(pred_race == "api", 1, 0), levels = c(1, 0)),
           other = factor(if_else(pred_race == "other", 1, 0), levels = c(1, 0)))
  
}