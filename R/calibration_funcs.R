
build_calibration_data <- function(voter_file, wru_data, bper_data) {
  wru_cal <- predict_race_wru(voter_file %>% 
                                filter(state %in% c("FL", "NC")) %>% 
                                sample_n(1e6),
                              census.geo = "block", party = "PID",
                              age = T, sex = T, census.data = wru_data$block_agesex)
  bper_cal <- 
    bper::impute_ethnorace(voter_file %>% 
                             sample_n(1e6), 
                           year = 2019,
                           bper_data = bper_data)
  return(lst(wru_cal, bper_cal))
}

brierize <- function(df, pred_probs, obs) {
  f_t <- df[[pred_probs]]
  o_t <- as.numeric(as.character(df[[obs]]))
  bs <- mean((f_t - o_t)^2, na.rm = TRUE)
  return(bs)
}

create_brier_table <- function(calibration_data) {
  bs_inputs <- tibble(
    pred_probs = c("pred_aapi", "pred_black", "pred_hispanic", "pred_white"),
    obs = c("obs_aapi", "obs_black", "obs_hispanic", "obs_white")
  )
  
  wru_bs <- pmap_dbl(bs_inputs, brierize, df = calibration_data$wru_cal)
  bper_bs <- pmap_dbl(bs_inputs, brierize, df = calibration_data$bper_cal)
  
  bs_table <- rbind(bper_bs, wru_bs) %>% 
    as_tibble(.name_repair = "unique") %>% 
    mutate(Method = c("bper", "wru")) %>% 
    select(Method, everything())
  
  names(bs_table) <- c("Method", "Asian", "Black", "Hispanic", "White")
  
  return(bs_table)
}

create_calibration_plot_data <- function(calibration_data, race) {
  cal_formula <- as.formula(paste0("obs_", race, "~", "pred_", race))
  
  wru_cal_data <- caret::calibration(cal_formula, 
                                     data = calibration_data$wru_cal)
  wru_cal_data <- wru_cal_data$data %>% 
    as_tibble() %>% 
    mutate(method = "wru")
  bper_cal_data <- caret::calibration(cal_formula, 
                                      data = calibration_data$bper_cal)
  bper_cal_data <- bper_cal_data$data %>% 
    as_tibble() %>% 
    mutate(method = "bper")
  
  cal_data <- rbind(wru_cal_data, bper_cal_data) %>% 
    mutate(race := race)
  
  return(cal_data)
}
