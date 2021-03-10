library(tidyverse)
library(tidylog)
library(here)


# Clean State Files -------------------------------------------------------

source(here("R", "prep", "florida.R"))
source(here("R", "prep", "north_carolina.R"))

# Main Data ---------------------------------------------------------------

load(file = here("data", "florida.rda"))
load(file = here("data", "north_carolina.rda"))

nc_fl <- rbind(florida, north_carolina)
save(nc_fl, file = here("data", "nc_fl.rda"))


# WRU Data ----------------------------------------------------------------

nc_fl_wru <- nc_fl %>% 
  mutate(VoterID = row_number(),
         surname = last_name,
         county = str_sub(block, 3, 5),
         tract = str_sub(block, 6, 11),
         block = str_sub(block, 12, 15),
         place = str_sub(place, 3, 7),
         age = 2010 - as.numeric(birth_year),
         sex = female,
         PID = case_when(party == "UNA" ~ 0,
                         party == "DEM" ~ 1,
                         party == "REP" ~ 2)) %>% 
  select(VoterID, surname, state, county, tract, block, place, age, sex, PID, race)

save(nc_fl_wru, file = here("data", "nc_fl_wru.rda"))



# Joint Prob Data ---------------------------------------------------------

load(file = "data/nc_fl.rda")

nc_fl2 <- nc_fl %>% 
  mutate(age = 2010 - as.numeric(birth_year),
         age = case_when(age < 5 ~ "t5",
                         age >= 5 & age <= 9 ~ "5t9",
                         age >= 10 & age <= 14 ~ "10t14",
                         age >= 15 & age <= 17 ~ "15t17",
                         age >= 18 & age <= 19 ~ "18t19",
                         age == 20 ~ "20",
                         age == 21 ~ "21",
                         age >= 22 & age <= "24" ~ "22t24",
                         age >= 25 & age <= "29" ~ "25t29",
                         age >= 30 & age <= "34" ~ "30t34",
                         age >= 35 & age <= "39" ~ "35t39",
                         age >= 40 & age <= "44" ~ "40t44",
                         age >= 45 & age <= "49" ~ "45t49",
                         age >= 50 & age <= "54" ~ "50t54",
                         age >= 55 & age <= "59" ~ "55t59",
                         age >= 60 & age <= "61" ~ "60t61",
                         age >= 62 & age <= "64" ~ "62t64",
                         age >= 65 & age <= "66" ~ "65t66",
                         age >= 67 & age <= "69" ~ "67t69",
                         age >= 70 & age <= "74" ~ "70t74",
                         age >= 75 & age <= "79" ~ "75t79",
                         age >= 80 & age <= "84" ~ "80t84",
                         age >= 85 ~ "85t"),
         sex = case_when(female == 1 ~ "F",
                         TRUE ~ "M")) %>% 
  select(-birth_year, -female)

save(nc_fl2, file = here("data", "nc_fl2.rda"))
