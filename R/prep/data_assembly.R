

# Main Data ---------------------------------------------------------------

load(file = "data/florida.rda")
load(file = "data/north_carolina.rda")

nc_fl <- rbind(florida, north_carolina)
save(nc_fl, file = "data/nc_fl.rda")


# WRU Data ----------------------------------------------------------------

nc_fl_wru <- nc_fl %>% 
  mutate(VoterID = row_number(),
         surname = last_name,
         county = str_sub(block, 3, 5),
         tract = str_sub(block, 6, 11),
         block = str_sub(block, 12, 15),
         age = 2010 - as.numeric(birth_year),
         sex = female,
         PID = case_when(party == "UNA" ~ 0,
                         party == "DEM" ~ 1,
                         party == "REP" ~ 2)) %>% 
  select(VoterID, surname, state, county, tract, block, age, sex, PID, race)

save(nc_fl_wru, file = "data/nc_fl_wru.rda")
