


find_largest_column <- function(df, num_cols) {
  cols <- df[tail(seq_along(df), num_cols)]
  max_col <- colnames(cols)[max.col(cols)]
  return(max_col)
}



classify_and_report <- function(df,
                                method = "bper",
                                wru_data = NULL,
                                bper_data = NULL,
                                geography,
                                party,
                                age,
                                sex,
                                ...) {
  
  if (method == "wru") {
    if (geography == "place") {
      wru_data_option = wru_data[["place"]]
      
      # Not all voters live in places
      df = df |> 
        filter(!is.na(place))
      
      df_surname = df |> 
        filter(missing_wru_place == 1)
      
      df_geo = df |> 
        filter(missing_wru_place == 0)
    } else {
      wru_data_option = wru_data[["block"]]
      
      df_surname = df |> 
        filter(missing_wru_block == 1)
      
      df_geo = df |> 
        filter(missing_wru_block == 0)
    }
    if (party == TRUE) {
      party_option = "PID"
    } else {
      party_option = NULL
    }
    
    if (nrow(df_surname) > 0) {
      df_surname <- predict_race_wru(df = df_surname,
                                     surname.only = TRUE,
                                     party = party_option,
                                     ...)
    }
    
    df_geo <- predict_race_wru(df = df_geo, 
                               census.geo = geography, 
                               census.data = wru_data_option,
                               party = party_option,
                               ...)
    
    df <- rbind(df_surname, df_geo)
  
  } else if (method == "bper") {
    
    if (geography == "tract") {
      df <- df %>% 
        select(-c(block, multi_unit))
    } else if (geography == "place") {
      df <- df %>% 
        select(-c(block, multi_unit, tract)) |> 
        filter(!is.na(place))
    } else if (geography == "zip") {
      df <- df %>% 
        select(-c(block, multi_unit, tract, place))
    } else if (geography == "county") {
      df <- df %>% 
        select(-c(block, multi_unit, tract, place, zip))
    } else if (geography == "state") {
      df <- df %>% 
        select(-c(block, multi_unit, tract, place, zip, county))
    }
    
    if (party == FALSE) { df <- select(df, -"party") }
    if (sex == FALSE) { df <- select(df, -"sex") }
    if (age == FALSE) { df <- select(df, -"age") }
    
    df <- bper::impute_ethnorace(df, year = 2019, bper_data = bper_data) %>% 
      mutate(pred_race = ifelse(pred_race == "aian", "other", pred_race))
  }
  
  # caret needs true and predicted class as factors
  df$pred_race = as.factor(df$pred_race)
  df$race = as.factor(df$race)
  
  conf_matrix <-
    caret::confusionMatrix(df$race, df$pred_race, mode = "everything")
  
  accuracy <- conf_matrix$overall[1]
  
  prec_aapi <- conf_matrix$byClass[1, "Precision"]
  prec_black <- conf_matrix$byClass[2, "Precision"]
  prec_hispanic <- conf_matrix$byClass[3, "Precision"]
  prec_other <- conf_matrix$byClass[4, "Precision"]
  prec_white <- conf_matrix$byClass[5, "Precision"]
  
  rec_aapi <- conf_matrix$byClass[1, "Recall"]
  rec_black <- conf_matrix$byClass[2, "Recall"]
  rec_hispanic <- conf_matrix$byClass[3, "Recall"]
  rec_other <- conf_matrix$byClass[4, "Recall"]
  rec_white <- conf_matrix$byClass[5, "Recall"]
  
  metric_df <- tibble(
    method = method,
    geography = geography,
    party = party,
    age = age,
    sex = sex,
    accuracy = accuracy,
    prec_aapi = prec_aapi,
    prec_black = prec_black,
    prec_hispanic = prec_hispanic,
    prec_other = prec_other,
    prec_white = prec_white,
    rec_aapi = rec_aapi,
    rec_black = rec_black,
    rec_hispanic = rec_hispanic,
    rec_other = rec_other,
    rec_white = rec_white
  )
  
  return(metric_df)
}

set_bisg_args <- function() {
  bisg_args <- tibble(geography = c("state", "county", "zip", "place", "tract", "block")) |> 
    crossing(party = c(TRUE, FALSE),
             age = c(TRUE),
             sex = c(TRUE))
  return(bisg_args)
}


calc_validation_results <- function(test_df) {
  test_df <- test_df |> 
    group_by(geography, party) |> 
    summarise(across(where(is.numeric), mean)) |> 
    pivot_longer(cols = where(is.numeric)) |> 
    mutate(metric = case_when(name == "accuracy" ~ "Accuracy",
                              str_detect(name, "prec") ~ "Precision",
                              str_detect(name, "rec") ~ "Recall"),
           race = sub("^[^_]*_", "", name) |> str_to_title(),
           race = ifelse(race == "Aapi", "Asian", race),
           geography = factor(str_to_title(geography),
                              levels = c("State", "County", "Zip", "Place", "Tract", "Block")))
  
  return(test_df)
}
# calc_validation_results(bper_tests)
