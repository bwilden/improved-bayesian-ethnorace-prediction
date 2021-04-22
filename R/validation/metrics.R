
# Metrics Tests -----------------------------------------------------------

N <- 1e5

sample_vec <- sample(1:nrow(nc_fl), N, replace = FALSE)

# wru ---------------------------------------------------------------------

# nc_fl_wru_tests <- nc_fl_wru[sample_vec, ]
# 
# wru_all <- nc_fl_wru_tests %>% 
#   filter(!is.na(block)) %>% 
#   classify_and_report(wru = T, wru_geo = "block", test_type = "all")
# wru_place <- nc_fl_wru_tests %>% 
#   filter(!is.na(place)) %>% 
#   classify_and_report(wru = T, wru_geo = "place", test_type = "place")
# wru_county <- nc_fl_wru_tests %>% 
#   filter(!is.na(county)) %>% 
#   classify_and_report(wru = T, wru_geo = "county", test_type = "county")
# wru_all_np <- nc_fl_wru_tests %>% 
#   filter(!is.na(block)) %>% 
#   select(-PID) %>% 
#   classify_and_report(wru = T, wru_geo = "block", wru_party = F, test_type = "all_np")
# wru_place_np <- nc_fl_wru_tests %>% 
#   select(-PID) %>%
#   filter(!is.na(place)) %>% 
#   classify_and_report(wru = T, wru_geo = "place", wru_party = F, test_type = "place_np")
# wru_county_np <- nc_fl_wru_tests %>% 
#   filter(!is.na(county)) %>% 
#   select(-PID) %>% 
#   classify_and_report(wru = T, wru_geo = "county", wru_party = F, test_type = "county_np")
# 
# wru_tests <- rbind(wru_all, wru_place, wru_county, wru_all_np, wru_place_np, wru_county_np)
# 
# save(wru_tests, file = here("data", "wru_tests.rda"))



# bper --------------------------------------------------------------------

nc_fl_tests <- nc_fl[sample_vec, ]

bper_all <- nc_fl_tests %>% 
  filter(block %in% blocks$block) %>% 
  classify_and_report(test_type = "all")

bper_all_np <- nc_fl_tests %>% 
  filter(block %in% blocks$block) %>% 
  select(-party) %>% 
  classify_and_report(test_type = "all_np")
bper_place <- nc_fl_tests %>% 
  select(-c(block, apartment)) %>% 
  filter(place %in% places$place) %>% 
  classify_and_report(test_type = "place")

bper_county <- nc_fl_tests %>% 
  filter(county %in% counties$county) %>% 
  select(-c(block, place, zip, apartment)) %>% 
  classify_and_report(test_type = "county")

bper_place_np <- nc_fl_tests %>% 
  select(-c(party, block, apartment)) %>% 
  filter(place %in% places$place) %>% 
  classify_and_report(test_type = "place_np")

bper_county_np <- nc_fl_tests %>% 
  filter(county %in% counties$county) %>% 
  select(-c(party, block, place, zip, apartment)) %>% 
  classify_and_report(test_type = "county_np")

bper_zip <- nc_fl_tests %>%
  filter(zip %in% zips$zip) %>%
  select(-c(block, place, apartment)) %>%
  classify_and_report(test_type = "zip")

bper_zip_np <- nc_fl_tests %>%
  filter(zip %in% zips$zip) %>%
  select(-c(party, block, place, apartment)) %>%
  classify_and_report(test_type = "zip_np")

bper_state <- nc_fl_tests %>%
  filter(!is.na(state)) %>%
  select(-c(block, place, zip, county, apartment)) %>%
  classify_and_report(test_type = "state")

bper_state_np <- nc_fl_tests %>%
  filter(!is.na(state)) %>%
  select(-c(party, block, place, zip, county, apartment)) %>%
  classify_and_report(test_type = "state_np")

load(here("data", "wru_tests.rda"))

tests <- rbind(wru_tests, bper_all, bper_all_np, bper_place, bper_county, 
               bper_place_np, bper_county_np, bper_zip, bper_zip_np, 
               bper_state, bper_state_np)


# save(tests, file = here("data", "tests.rda"))
