
arg_max_cols <- function(df, num_cols) {
  cols <- df[tail(seq_along(df), num_cols)]
  max_col <- colnames(cols)[max.col(cols)]
  return(max_col)
}

predict_race_wru <- function(df, wru_geo, wru_party) {
  
  df_nc <- df %>%
    filter(state == "NC")
  df_fl <- df %>%
    filter(state == "FL")
  
  if (wru_party == TRUE) {
    if (wru_geo == "place") {
      df_nc <-
        predict_race(
          voter.file = df_nc,
          census.geo = wru_geo,
          census.key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
          party = "PID",
          age = F,
          sex = F
        )
      df_fl <-
        predict_race(
          voter.file = df_fl,
          census.geo = wru_geo,
          census.key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
          party = "PID",
          age = F,
          sex = F
        )
    } else {
      df_nc <-
        predict_race(
          voter.file = df_nc,
          census.geo = wru_geo,
          census.data = census.nc,
          party = "PID",
          age = F,
          sex = F
        )
      df_fl <-
        predict_race(
          voter.file = df_fl,
          census.geo = wru_geo,
          census.data = census.fl,
          party = "PID",
          age = F,
          sex = F
        )
    }
  } else if (wru_geo == "place") {
    df_nc <-
      predict_race(
        voter.file = df_nc,
        census.geo = wru_geo,
        census.key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
        age = F,
        sex = F
      )
    df_fl <-
      predict_race(
        voter.file = df_fl,
        census.geo = wru_geo,
        census.key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
        age = F,
        sex = F
      )
  } else {
    df_nc <-
      predict_race(
        voter.file = df_nc,
        census.geo = wru_geo,
        census.data = census.nc,
        age = F,
        sex = F
      )
    df_fl <-
      predict_race(
        voter.file = df_fl,
        census.geo = wru_geo,
        census.data = census.fl,
        age = F,
        sex = F
      )
  }
  
  df <- rbind(df_fl, df_nc)
  
  df$pred_race <- arg_max_cols(df, 5)
  
  df <- df %>%
    rename(
      pred_white = pred.whi,
      pred_black = pred.bla,
      pred_hispanic = pred.his,
      pred_api = pred.asi,
      pred_other = pred.oth
    ) %>%
    mutate(
      pred_race = case_when(
        pred_race == "pred.whi" ~ "white",
        pred_race == "pred.bla" ~ "black",
        pred_race == "pred.his" ~ "hispanic",
        pred_race == "pred.asi" ~ "api",
        pred_race == "pred.oth" ~ "other"
      ),
      black = factor(if_else(pred_race == "black", 1, 0), levels = c(1, 0)),
      white = factor(if_else(pred_race == "white", 1, 0), levels = c(1, 0)),
      hispanic = factor(if_else(pred_race == "hispanic", 1, 0), levels = c(1, 0)),
      api = factor(if_else(pred_race == "api", 1, 0), levels = c(1, 0)),
      other = factor(if_else(pred_race == "other", 1, 0), levels = c(1, 0))
    )
  
}

classify_and_report <- function(df, wru = F, wru_geo, wru_party = TRUE, test_type) {
  if (wru == TRUE) {
    df <- predict_race_wru(df, wru_geo, wru_party) %>%
      mutate(race = if_else(race == "aian", "other", race))
  } else {
    df <- bper::predict_ethnorace(df) %>%
      mutate(
        race = if_else(race == "aian", "other", race),
        pred_race = if_else(pred_race == "aian", "other", pred_race)
      )
  }
  
  # df$pred_race <- arg_max_cols(df, 5)
  #
  # df <- df %>%
  #   mutate(
  #     pred_race = case_when(
  #       pred_race == "prob_white" ~ "white",
  #       pred_race == "prob_black" ~ "black",
  #       pred_race == "prob_hispanic" ~ "hispanic",
  #       pred_race == "prob_api" ~ "api",
  #       pred_race == "prob_other" ~ "other"
  #     )
  #   )
  
  df <- df %>%
    mutate(pred_race = as.factor(pred_race),
           race = as.factor(race))
  
  # prop_na <- sum(is.na(df$pred_race)) / nrow(df)
  
  # true_props <- round(prop.table(table(df$race)), digits = 4)
  pred_props <-
    round(prop.table(table(df$pred_race)), digits = 4)
  prop_api <- pred_props[1]
  prop_black <- pred_props[2]
  prop_hispanic <- pred_props[3]
  prop_other <- pred_props[4]
  prop_white <- pred_props[5]
  
  conf_matrix <-
    confusionMatrix(df$race, df$pred_race, mode = "everything")
  
  accuracy <- conf_matrix$overall[1]
  
  prec_api <- conf_matrix$byClass[1, "Precision"]
  prec_black <- conf_matrix$byClass[2, "Precision"]
  prec_hispanic <- conf_matrix$byClass[3, "Precision"]
  prec_other <- conf_matrix$byClass[4, "Precision"]
  prec_white <- conf_matrix$byClass[5, "Precision"]
  
  rec_api <- conf_matrix$byClass[1, "Recall"]
  rec_black <- conf_matrix$byClass[2, "Recall"]
  rec_hispanic <- conf_matrix$byClass[3, "Recall"]
  rec_other <- conf_matrix$byClass[4, "Recall"]
  rec_white <- conf_matrix$byClass[5, "Recall"]
  
  if (wru == TRUE) {
    test_method <- "wru"
  } else {
    test_method <- "bper"
  }
  
  return(
    tibble(
      test_method = test_method,
      test_type = test_type,
      accuracy = accuracy,
      prec_api = prec_api,
      prec_black = prec_black,
      prec_hispanic = prec_hispanic,
      prec_other = prec_other,
      prec_white = prec_white,
      rec_api = rec_api,
      rec_black = rec_black,
      rec_hispanic = rec_hispanic,
      rec_other = rec_other,
      rec_white = rec_white,
      prop_api = prop_api,
      prop_black = prop_black,
      prop_hispanic = prop_hispanic,
      prop_other = prop_other,
      prop_white = prop_white,
    )
  )
}

