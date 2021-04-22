

# Calibration -------------------------------------------------------------

N <- 3e6

sample_vec <- sample(1:nrow(nc_fl), N, replace = FALSE)

brierize <- function(probs, obs) {
  f_t <- probs
  o_t <- as.numeric(as.character(obs))
  bs <- mean((f_t - o_t)^2, na.rm = T)
  
  return(bs)
}


# wru ---------------------------------------------------------------------

wru_cal <- nc_fl_wru[sample_vec, ] %>% 
  predict_race_wru(wru_geo = "block", wru_party = T)

wru_cal_black <- calibration(obs_black ~ prob_black, data = wru_cal)
wru_cal_api <- calibration(obs_api ~ prob_api, data = wru_cal)
wru_cal_hispanic <- calibration(obs_hispanic ~ prob_hispanic, data = wru_cal)
wru_cal_white <- calibration(obs_white ~ prob_white, data = wru_cal)
wru_cal_other <- calibration(obs_other ~ prob_other, data = wru_cal)

wru_bs_black <- brierize(wru_cal$prob_black, wru_cal$obs_black)
wru_bs_api <- brierize(wru_cal$prob_api, wru_cal$obs_api)
wru_bs_hispanic <- brierize(wru_cal$prob_hispanic, wru_cal$obs_hispanic)
wru_bs_white <- brierize(wru_cal$prob_white, wru_cal$obs_white)
wru_bs_other <- brierize(wru_cal$prob_other, wru_cal$obs_other)


# bper --------------------------------------------------------------------


bper_cal <- nc_fl[sample_vec, ] %>% 
  predict_ethnorace(prior = "all", dichotomize = TRUE)

bper_cal_black <- calibration(obs_black ~ prob_black, data = bper_cal)
bper_cal_api <- calibration(obs_api ~ prob_api, data = bper_cal)
bper_cal_aian <- calibration(obs_aian ~ prob_aian, data = bper_cal)
bper_cal_hispanic <- calibration(obs_hispanic ~ prob_hispanic, data = bper_cal)
bper_cal_white <- calibration(obs_white ~ prob_white, data = bper_cal)
bper_cal_other <- calibration(obs_other ~ prob_other, data = bper_cal)

bper_bs_black <- brierize(bper_cal$prob_black, bper_cal$obs_black)
bper_bs_api <- brierize(bper_cal$prob_api, bper_cal$obs_api)
bper_bs_aian <- brierize(bper_cal$prob_aian, bper_cal$obs_aian)
bper_bs_hispanic <- brierize(bper_cal$prob_hispanic, bper_cal$obs_hispanic)
bper_bs_white <- brierize(bper_cal$prob_white, bper_cal$obs_white)
bper_bs_other <- brierize(bper_cal$prob_other, bper_cal$obs_other)


# Save Results ------------------------------------------------------------

cal_data <- list(wru_cal_black, wru_cal_api, wru_cal_hispanic, wru_cal_white, 
                 wru_cal_other, wru_bs_black, wru_bs_api, wru_bs_hispanic, 
                 wru_bs_white, wru_bs_other, bper_cal_black, bper_cal_api, 
                 bper_cal_aian, bper_cal_hispanic, bper_cal_white, bper_cal_other, 
                 bper_bs_black, bper_bs_api, bper_bs_aian, bper_bs_hispanic, 
                 bper_bs_white, bper_bs_other)

save(cal_data, file = here("data", "cal_data.rda"))
