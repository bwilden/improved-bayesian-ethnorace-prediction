
load_wru_data <- function(state_abbrvs) {
  if (!file.exists(here("bper", "data", "wru_data.rda"))) {
    wru_place_agesex_dat <-
      get_census_data(
        key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
        states = state_abbrvs,
        age = TRUE,
        sex = TRUE,
        census.geo = "place",
        retry = 10
      )
    wru_place_dat <-
      get_census_data(
        key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
        states = state_abbrvs,
        age = FALSE,
        sex = FALSE,
        census.geo = "place",
        retry = 10
      )
    wru_block_agesex_dat <-
      get_census_data(
        key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
        states = state_abbrvs,
        age = TRUE,
        sex = TRUE,
        census.geo = "block",
        retry = 10
      )
    wru_block_dat <-
      get_census_data(
        key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
        states = state_abbrvs,
        age = FALSE,
        sex = FALSE,
        census.geo = "block",
        retry = 10
      )
    wru_data <- list(
      "place_agesex" = wru_place_agesex_dat,
      "place" = wru_place_dat,
      "block_agesex" = wru_block_agesex_dat,
      "block" = wru_block_dat
    )
    save(wru_data, file = here("data", "wru_data.rda"))
  } else {
    load(here("bper", "data", "wru_data.rda"))
  }
  return(wru_data)
}







find_largest_column <- function(df, num_cols) {
  cols <- df[tail(seq_along(df), num_cols)]
  max_col <- colnames(cols)[max.col(cols)]
  return(max_col)
}

predict_race_wru <- function(...) {

  df <- predict_race(census.key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
                     ...)
  
  df$pred_race <- find_largest_column(df, 5)
  
  df <- df %>%
    rename(
      pred_white = pred.whi,
      pred_black = pred.bla,
      pred_hispanic = pred.his,
      pred_aapi = pred.asi,
      pred_other = pred.oth
    ) %>%
    mutate(
      pred_race = case_when(
        pred_race == "pred.whi" ~ "white",
        pred_race == "pred.bla" ~ "black",
        pred_race == "pred.his" ~ "hispanic",
        pred_race == "pred.asi" ~ "aapi",
        pred_race == "pred.oth" ~ "other"
      ),
      black = factor(ifelse(pred_race == "black", 1, 0)),
      white = factor(ifelse(pred_race == "white", 1, 0)),
      hispanic = factor(ifelse(pred_race == "hispanic", 1, 0)),
      aapi = factor(ifelse(pred_race == "aapi", 1, 0)),
      other = factor(ifelse(pred_race == "other", 1, 0))
    )
  
  return(df)
}

classify_and_report <- function(df,
                                wru = FALSE,
                                wru_data = NULL,
                                bper_data = NULL,
                                geography,
                                party,
                                age,
                                sex,
                                urban = "All",
                                income_level = "All",
                                foreignborn_level = "All",
                                college_level = "All",
                                ...) {
  
  if (urban == "Urban") {
    df <- df %>% filter(urban == 1)
  } else if (urban == "Rural") {
    df <- df %>% filter(urban == 0)
  }
  if (income_level != "All") {
    df <- df %>% filter(median_income_level == income_level)
  }
  if (foreignborn_level != "All") {
    df <- df %>% filter(pct_foreignborn_level == foreignborn_level)
  }
  if (college_level != "All") {
    df <- df %>% filter(pct_college_level == college_level)
  }
  
  if (wru == TRUE) {
    if (geography == "place") {
      if (age == TRUE & sex == TRUE) {
        wru_dat <- wru_data[["place_agesex"]]
      } else {
        wru_dat <- wru_data[["place"]]
      }
    } else {
      if (age == TRUE & sex == TRUE) {
        wru_dat <- wru_data[["block_agesex"]]
      } else {
        wru_dat <- wru_data[["block"]]
      }
    }
    if (party == TRUE) {
      df <- predict_race_wru(voter.file = df, 
                             census.geo = geography, 
                             census.data = wru_dat,
                             age = age,
                             sex = sex,
                             party = "PID",
                             ...)
    } else {
      df <- predict_race_wru(voter.file = df, 
                             census.geo = geography, 
                             census.data = wru_dat,
                             age = age,
                             sex = sex,
                             ...)
    }
    
  } else {
    if (geography == "tract") {
      df <- df %>% 
        select(-c(block, multi_unit))
    } else if (geography == "place") {
      df <- df %>% 
        select(-c(block, multi_unit, tract))
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
    
    if (party == FALSE) {
      df <- df %>% 
        select(-"party")
    }
    if (sex == FALSE) {
      df <- df %>% 
        select(-"sex")
    }
    if (age == FALSE) {
      df <- df %>% 
        select(-"age")
    }
    
    df <- bper::impute_ethnorace(df, year = 2019, bper_data = bper_data,
                                 census_key = "5e4c2b8438222753a7f4753fa78855eca73b9950",
                                 ...) %>% 
      mutate(pred_race = ifelse(pred_race == "aian", "other", pred_race))
  }
  
  df <- df %>%
    mutate(race = as.factor(ifelse(race %in% c("multi", "aian"), "other", race)),
           pred_race = as.factor(pred_race))
  
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
  
  if (wru == TRUE) {
    method <- "wru"
  } else {
    method <- "bper"
  }
  
  tibble(
    method = method,
    geography = geography,
    party = party,
    age = age,
    sex = sex,
    urban = urban,
    income_level = income_level,
    foreignborn_level = foreignborn_level,
    college_level = college_level
  ) %>% print()
  
  return(
    tibble(
      method = method,
      geography = geography,
      party = party,
      age = age,
      sex = sex,
      urban = urban,
      income_level = income_level,
      foreignborn_level = foreignborn_level,
      college_level = college_level,
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
  )
}

run_validation_tests <- function(wru, ...) {
  argument_df_main <- tibble(
    geography = c("county", "place", "tract", "block"),
    wru = wru
  ) %>% 
    crossing(urban = c("All", "Urban", "Rural"),
             income_level = c("All", "Low", "High"),
             foreignborn_level = c("All", "Low", "High"),
             college_level = c("All", "Low", "High"),
             party = c(TRUE, FALSE),
             age = c(TRUE, FALSE),
             sex = c(TRUE, FALSE)) %>% 
    filter(age == sex,
           str_count(
             paste(urban, income_level, foreignborn_level, college_level), 
             "All") >= 3)
  
  validation_tests <- pmap_dfr(argument_df_main, classify_and_report, ...) %>% 
    mutate(test_id = row_number())
  return(validation_tests)
}

calc_validation_results <- function(bper_test_df, wru_test_df) {
  overall <- rbind(
    bper_test_df,
    wru_test_df
  ) %>% 
    mutate(Party = ifelse(party == TRUE, "|", ""),
           `No Party` = ifelse(party == FALSE, "|", ""),
           `Age-Sex` = ifelse(age == TRUE & sex == TRUE, "|", ""),
           `No Age-Sex` = ifelse(age == FALSE & sex == FALSE, "|", ""),
           Block = ifelse(geography == "block", "|", ""),
           Tract = ifelse(geography == "tract", "|", ""),
           Place = ifelse(geography == "place", "|", ""),
           ZIP = ifelse(geography == "zip", "|", ""),
           County = ifelse(geography == "county", "|", ""),
           State = ifelse(geography == "state", "|", ""),
           `Urban-Rural All` = ifelse(urban == "All", "|", ""),
           `Urban` = ifelse(urban == "Urban", "|", ""),
           `Rural` = ifelse(urban == "Rural", "|", ""),
           `Income Level All` = ifelse(income_level == "All", "|", ""),
           `Income Level High` = ifelse(income_level == "High", "|", ""),
           `Income Level Middle` = ifelse(income_level == "Middle", "|", ""),
           `Income Level Low` = ifelse(income_level == "Low", "|", ""),
           `ForeignBorn Level All` = ifelse(foreignborn_level == "All", "|", ""),
           `ForeignBorn Level High` = ifelse(foreignborn_level == "High", "|", ""),
           `ForeignBorn Level Middle` = ifelse(foreignborn_level == "Middle", "|", ""),
           `ForeignBorn Level Low` = ifelse(foreignborn_level == "Low", "|", ""),
           `College Level All` = ifelse(college_level == "All", "|", ""),
           `College Level High` = ifelse(college_level == "High", "|", ""),
           `College Level Middle` = ifelse(college_level == "Middle", "|", ""),
           `College Level Low` = ifelse(college_level == "Low", "|", ""))
  
  bper_df <- overall %>% 
    filter(method == "bper") %>% 
    select(-c(method, geography, party, age, sex, urban, test_id,
              income_level, foreignborn_level, college_level)) %>% 
    pivot_longer(cols = where(is.numeric), names_to = "metric", values_to = "value")
  
  avgs_df <- bper_df %>% 
    mutate(party = ifelse(Party == "", "No Party", "Party"),
           age_sex = ifelse(`Age-Sex` == "", "No Age-Sex", "Age-Sex"),
           metric_type = case_when(grepl("prec_", metric) ~ "Precision",
                                   grepl("rec_", metric) ~ "Recall",
                                   TRUE ~ "Accuracy"),
           race = case_when(grepl("aapi", metric) ~ "Asian",
                            grepl("black", metric) ~ "Black",
                            grepl("hispanic", metric) ~ "Hispanic",
                            grepl("white", metric) ~ "White",
                            grepl("other", metric) ~ "Other",
                            TRUE ~ "Accuracy"))
  
  party_avgs <- avgs_df %>% 
    group_by(party, metric_type, race) %>% 
    summarise(avg_value = mean(value)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(metric_type, race),
                names_from = party,
                values_from = avg_value)
  
  age_sex_avgs <- avgs_df %>% 
    group_by(age_sex, metric_type, race) %>% 
    summarise(avg_value = mean(value)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = c(metric_type, race),
                names_from = age_sex,
                values_from = avg_value)
  
  comparison_df <- overall %>% 
    # filter(!(geography %in% c("state", "zip")),
    #        `Age-Sex` == "|") %>% 
    # mutate(comparison_group = ceiling(row_number() / 2)) %>% 
    select(-c(method, geography, party, age, sex, urban, ZIP, State,
              income_level, foreignborn_level, college_level)) %>% 
    group_by(test_id) %>% 
    summarise(
      across(where(is.numeric),
             ~ first(.x) - last(.x)),
      across(!where(is.numeric),
             ~ first(.x))) %>% 
    select(-test_id) %>% 
    pivot_longer(cols = where(is.numeric), names_to = "metric", values_to = "bper_diff") %>% 
    filter(!(metric %in% c("rec_other", "prec_other")))
  
  # Summary statistics
  n_comparisons <- nrow(comparison_df)
  overall_bper_win_pct <- mean(comparison_df$bper_diff > 0)
  
  per_test <- comparison_df %>% 
    group_by(metric) %>% 
    summarise(bper_win_pct = mean(bper_diff > 0))
  
  return(lst(n_comparisons, overall_bper_win_pct, per_test, 
             comparison_df, bper_df, party_avgs, age_sex_avgs,
             overall))
}


