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
