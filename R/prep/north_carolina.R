# North Carolina

nc <- read_tsv(here("raw-data", "north_carolina", "ncvoter_Statewide.txt"))


common_surname_prefixes <- c("MC", "DE", "VAN", "DEL", "ST", "LA", "DI", 
                             "JEAN", "LE", "MAC", "SAINT", "DA ", "O", "VON", 
                             "SAN", "DU", "EL", "LO", "DOS", "AL", "D", "DELLA", 
                             "DELA", "DES", "L")

nc_clean <- nc %>% 
  mutate(apartment = as.numeric(grepl("#", res_street_address, fixed = T)),
         address = res_street_address,
         last_name = toupper(gsub("\\W+", " ", last_name)),
         last_name = if_else(grepl(paste(common_surname_prefixes, collapse = " |^"), last_name), 
                             gsub(" ", "", last_name),
                             gsub(" .*", "", last_name)),
         city = res_city_desc,
         state = state_cd,
         zip = zip_code,
         full_address = str_squish(paste(address, city, state, zip)),
         female = case_when(gender_code == "F" ~ 1, TRUE ~ 0),
         race = case_when(ethnic_code == "HL" ~ "hispanic",
                          race_code == "A" | race_code == "P" ~ "api",
                          race_code == "B" ~ "black",
                          race_code == "I" ~ "aian",
                          race_code == "O" | race_code == "U" ~ "other",
                          race_code == "W" ~ "white"),
         party = case_when(party_cd %in% c("REP", "DEM") ~  party_cd, TRUE ~ "UNA"))


# missing_lasts <- nc_clean %>% 
#   select(last_name) %>% 
#   filter(!(last_name %in% surnames$last_name)) %>% 
#   group_by(last_name) %>% 
#   summarise(n())


# nc_clean %>% 
#   select(full_address) %>% 
#   distinct() %>% 
#   write_csv(paste0(state_path, "nc_address.csv"))
# 


# Clean Data --------------------------------------------------------------

nc_geo <- read_csv(here("raw-data", "north_carolina", "nc_geocode.csv"), guess_max = 1e6) %>% 
    select(full_address, block = `Full FIPS`, place = `Place FIPS`)
  
north_carolina <- nc_clean %>% 
  left_join(nc_geo, by = "full_address") %>% 
  mutate(block = if_else(is.na(state), NA_character_, block),
         state = if_else(is.na(state), "NC", state),
         county = str_sub(block, 1, 5)) %>% 
  select(first_name, last_name, female, party, apartment, birth_year, state, zip, county, place, block, race)

save(north_carolina, file = here("data", "north_carolina.rda"))  
 

