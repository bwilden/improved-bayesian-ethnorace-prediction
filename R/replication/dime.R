library(data.table)
library(tidyverse)
library(here)
library(tidylog)
library(readxl)



# DIME Analysis (cycle) ---------------------------------------------------


# Yearly Contribs ---------------------------------------------------------

state_abbrvs <- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 
                  'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 
                  'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 
                  'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 
                  'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 
                  'WY')

contrib_years <- list.files(path = here("data-raw", "dime"), pattern = "contribDB_*",
                      full.names = TRUE)

contribs <- list()
count <- 1

# Clean all year contributor files
for (year in contrib_years) {
  
  df <- read_csv(year, col_types = cols(.default = "c")) %>% 
    filter(contributor.type == "I",
           contributor.state %in% state_abbrvs,
           seat == "federal:house") %>% 
    mutate(last_name = if_else(grepl("mc ", contributor.lname), 
                               str_remove_all(contributor.lname, " "), contributor.lname),
           last_name = toupper(str_extract(last_name, "[A-Za-z]+")),
           first_name = str_remove(contributor.fname, "^[A-Za-z] "),
           first_name = str_remove(first_name, "(dr )|(wm )|(ew )|(pa )|(the )|(la )|(mr )|(mrs )"),
           first_name = toupper(str_extract(first_name, "[A-Za-z]+")),
           county = str_sub(censustract, end = 5),
           tract = str_sub(censustract, start = 6),
           zip = str_sub(contributor.zipcode, start = 1, end = 5),
           zip = str_pad(zip, width = 5, pad = "0"),
           female = case_when(contributor.gender == "F" ~ 1,
                              TRUE ~ 0),
           sex = female,
           surname = last_name,
           amount = as.numeric(amount)) %>% 
    select(cycle, amount, date, bonica.rid, bonica.cid, sex, surname, first_name, 
           last_name, female, zip, state = contributor.state, county, tract)
  
  contribs[[count]] <- df
  count <- count + 1
  
  rm(df)
    
}

contribs <- rbindlist(contribs) %>% 
  as_tibble()

save(contribs, file = here("data", "contribs.rda"))


load(here("data", "contribs.rda"))

bper_contribs <- contribs %>% 
  predict_ethnorace(prior = "all", dichotomize = T)


wru_surname_only_contribs <- contribs %>% 
  filter(is.na(county)) %>%
  predict_race(census.geo = "county", census.key = census_api, sex = TRUE, surname.only = TRUE)

wru_full_contribs <- contribs %>% 
  filter(!(is.na(county)),
         state %in% states$state) %>% 
  mutate(county = str_sub(county, start = 3)) %>% 
  predict_race(census.geo = "county", census.key = census_api, sex = TRUE)

wru_contribs <- rbind(wru_surname_only_contribs, wru_full_contribs) 

wru_contribs$pred_race <- arg_max_cols_wru(wru_contribs, 5)

wru_contribs <- wru_contribs %>% 
  rename(
    prob_white = pred.whi,
    prob_black = pred.bla,
    prob_hispanic = pred.his,
    prob_api = pred.asi,
    prob_other = pred.oth
  ) %>%
  mutate(
    pred_race = case_when(
      pred_race == "pred.whi" ~ "white",
      pred_race == "pred.bla" ~ "black",
      pred_race == "pred.his" ~ "hispanic",
      pred_race == "pred.asi" ~ "api",
      pred_race == "pred.oth" ~ "other"
    ),
    pred_black = factor(if_else(pred_race == "black", 1, 0), levels = c(1, 0)),
    pred_white = factor(if_else(pred_race == "white", 1, 0), levels = c(1, 0)),
    pred_hispanic = factor(if_else(pred_race == "hispanic", 1, 0), levels = c(1, 0)),
    pred_api = factor(if_else(pred_race == "api", 1, 0), levels = c(1, 0)),
    pred_other = factor(if_else(pred_race == "other", 1, 0), levels = c(1, 0))
  )


save(bper_contribs, file = here("data", "bper_contribs.rda"))
save(wru_contribs, file = here("data", "wru_contribs.rda"))




# Recipients --------------------------------------------------------------


# load(here("sahn_grumbach", "Candidate Data with Candidate Race and District and State Data.RData"))
# 
# cands <- data %>% 
#   select(bonica_rid, Name, r_pred_race) %>% 
#   filter(!is.na(r_pred_race)) %>% 
#   distinct() %>% 
#   group_by(bonica_rid, Name) %>% 
#   summarise(n = n()) %>% 
#   filter(n > 1)
# 
# cands2 <- data %>% 
#   select(bonica_rid, Name, r_pred_race) %>% 
#   left_join(bper_recips, by = c("bonica_rid" = "bonica.rid")) %>% 
#   distinct()



recips <- read_csv(here("data-raw", "dime", "dime_recipients_all_1979_2014.csv")) %>% 
  filter(state %in% state_abbrvs) %>% 
  mutate(last_name = if_else(grepl("mc ", lname), 
                             str_remove_all(lname, " "), lname),
         last_name = toupper(str_extract(last_name, "[A-Za-z]+")),
         first_name = str_remove(fname, "^[A-Za-z] "),
         first_name = str_remove(first_name, "(dr )|(wm )|(ew )|(pa )|(the )|(la )|(mr )|(mrs )"),
         first_name = toupper(str_extract(first_name, "[A-Za-z]+")),
         party = case_when(party == 100 ~ "DEM",
                           party == 200 ~ "REP",
                           TRUE ~ "UNA"),
         female = case_when(cand.gender == "F" ~ 1,
                            TRUE ~ 0),
         sex = female,
         surname = last_name) %>% 
  select(bonica.rid, first_name, last_name, party, state, female, surname, sex) %>% 
  distinct()
  
bper_recips <- recips %>% 
  predict_ethnorace(prior = "all") %>% 
  select(bonica.rid, r_pred_race = pred_race) %>% 
  distinct()

wru_recips <- recips %>%
  predict_race(surname.only = T,
               party = party,
               sex = T)

wru_recips$pred_race <- arg_max_cols_wru(wru_recips, 5)

wru_recips <- wru_recips %>% 
  rename(
    prob_white = pred.whi,
    prob_black = pred.bla,
    prob_hispanic = pred.his,
    prob_api = pred.asi,
    prob_other = pred.oth
  ) %>%
  mutate(
    pred_race = case_when(
      pred_race == "pred.whi" ~ "white",
      pred_race == "pred.bla" ~ "black",
      pred_race == "pred.his" ~ "hispanic",
      pred_race == "pred.asi" ~ "api",
      pred_race == "pred.oth" ~ "other"
    )
  ) %>%
  select(bonica.rid, r_pred_race = pred_race) %>% 
  distinct()



# Recipients with Contributors --------------------------------------------

bper_dimes <- bper_contribs %>% 
  left_join(bper_recips)

wru_dimes <- wru_contribs %>% 
  left_join(wru_recips)


  
save(bper_dimes, file = here("data", "bper_dimes.rda"))
save(wru_dimes, file = here("data", "wru_dimes.rda"))  
  
  
  
  
  
  
  


# DIME Analysis (full) -----------------------------------------------------

contribs <- read_csv(here("data-raw", "dime", "dime_contributors_1979_2014.csv"))


# bper --------------------------------------------------------------------

contribs_bper <- contribs %>% 
  filter(contributor.type == "I") %>% 
  separate(most.recent.contributor.name, into = c("last_name", "first_name"), sep = ",") %>% 
  mutate(last_name = if_else(grepl("mc ", last_name), str_remove_all(last_name, " "), last_name),
         last_name = toupper(str_extract(last_name, "[A-Za-z]+")),
         first_name = str_remove(first_name, "^[A-Za-z] "),
         first_name = str_remove(first_name, "(dr )|(wm )|(ew )|(pa )|(the )|(la )|(mr )|(mrs )"),
         first_name = toupper(str_extract(first_name, "[A-Za-z]+")),
         zip = str_sub(most.recent.contributor.zipcode, start = 1, end = 5),
         zip = str_pad(zip, width = 5, pad = "0"),
         state = toupper(most.recent.contributor.state),
         female = case_when(contributor.gender == "F" ~ 1,
                            TRUE ~ 0)) %>% 
  select(first_name, last_name, zip, state, female, contains("amount_"))

# missing_states <- contribs_bper %>% 
#   select(state) %>% 
#   group_by(state) %>% 
#   summarize(n = n()) %>% 
#   anti_join(states)
# 
# missing_surnames <- contribs_bper %>% 
#   select(last_name) %>% 
#   group_by(last_name) %>% 
#   summarize(n = n()) %>% 
#   anti_join(surnames)
# 
# missing_firsts <- contribs_bper %>% 
#   select(first_name) %>% 
#   group_by(first_name) %>% 
#   summarize(n = n()) %>% 
#   anti_join(firstnames)

contribs_bper_preds <- contribs_bper %>% 
  predict_ethnorace(prior = "all", dichotomize = TRUE)


# wru ---------------------------------------------------------------------

zip_country_crosswalk <- read_xlsx(here("data-raw", "ZIP_COUNTY_122014.xlsx"),
                                   col_types = "text") %>% 
  mutate(RES_RATIO = as.numeric(RES_RATIO),
         COUNTY = str_sub(COUNTY, 3, 5)) %>% 
  group_by(ZIP) %>% 
  top_n(RES_RATIO, COUNTY) %>% 
  select(zip = ZIP, county = COUNTY)


contribs_wru <- contribs_bper %>% 
  left_join(zip_country_crosswalk) %>% 
  rename(surname = last_name,
         sex = female) %>% 
  mutate(VoterID = row_number())

contribs_wru_surname_only <- contribs_wru %>% 
  filter(is.na(county)) %>% 
  predict_race(census.geo = "county", census.key = census_api, sex = TRUE, surname.only = TRUE)

contribs_wru_full <- contribs_wru %>% 
  filter(!(is.na(county)),
         state %in% states$state) %>% 
  predict_race(census.geo = "county", census.key = census_api, sex = TRUE)

contribs_wru_preds <- rbind(contribs_wru_surname_only, contribs_wru_full) 

contribs_wru_preds$pred_race <- arg_max_cols_wru(contribs_wru_preds, 5)

contribs_wru_preds <- contribs_wru_preds %>% 
  rename(
    prob_white = pred.whi,
    prob_black = pred.bla,
    prob_hispanic = pred.his,
    prob_api = pred.asi,
    prob_other = pred.oth
  ) %>%
  mutate(
    pred_race = case_when(
      pred_race == "pred.whi" ~ "white",
      pred_race == "pred.bla" ~ "black",
      pred_race == "pred.his" ~ "hispanic",
      pred_race == "pred.asi" ~ "api",
      pred_race == "pred.oth" ~ "other"
    ),
    pred_black = factor(if_else(pred_race == "black", 1, 0), levels = c(1, 0)),
    pred_white = factor(if_else(pred_race == "white", 1, 0), levels = c(1, 0)),
    pred_hispanic = factor(if_else(pred_race == "hispanic", 1, 0), levels = c(1, 0)),
    pred_api = factor(if_else(pred_race == "api", 1, 0), levels = c(1, 0)),
    pred_other = factor(if_else(pred_race == "other", 1, 0), levels = c(1, 0))
  )




# Save Results ------------------------------------------------------------

save(contribs_bper_preds, file = here("data", "contribs_bper_preds.rda"))
save(contribs_wru_preds, file = here("data", "contribs_wru_preds.rda"))

