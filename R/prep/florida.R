# Florida

state_path = "raw-data/florida/"

files <- list.files(path = paste0(state_path, "20170207_VoterDetail"), pattern = "*.txt", full.names = T)

fl <- map_df(files, ~read_tsv(., guess_max = 4e4, col_names = F,
                              col_types = cols(.default = "c")))

common_surname_prefixes <- c("MC", "DE", "VAN", "DEL", "ST", "LA", "DI", 
                             "JEAN", "LE", "MAC", "SAINT", "DA ", "O", "VON", 
                             "SAN", "DU", "EL", "LO", "DOS", "AL", "D", "DELLA", 
                             "DELA", "DES", "L")

fl_clean <- fl %>% 
  filter(X7 != "Y") %>% 
  mutate(apartment = case_when(is.na(X9) ~ 0, TRUE ~ 1),
         last_name = toupper(gsub("\\W+", " ", X3)),
         last_name = if_else(grepl(paste(common_surname_prefixes, collapse = " |^"), last_name), 
                             gsub(" ", "", last_name),
                             gsub(" .*", "", last_name)),
         first_name = toupper(X5),
         first_name = str_squish((str_replace_all(first_name, regex("\\W+"), ""))),
         unit = str_replace_na(X9, ""),
         address = str_squish(paste(X8, unit)),
         city = X10,
         state = "FL",
         zip = str_sub(X12, 1, 5),
         full_address = str_squish(paste(address, unit, city, state, zip)),
         female = case_when(X20 == "F" ~ 1, TRUE ~ 0),
         race = case_when(X21 == "1" ~ "aian",
                          X21 == "2" ~ "api",
                          X21 == "3" ~ "black",
                          X21 == "4" ~ "hispanic",
                          X21 == "5" ~ "white",
                          TRUE ~ "other"),
         birth_year = format(as.Date(X22, format = "%m/%d/%Y"), "%Y"),
         party = case_when(X24 %in% c("REP", "DEM") ~ X24, TRUE ~ "UNA"))

missing_lasts <- fl_clean %>% 
  select(last_name) %>% 
  filter(!(last_name %in% surnames$last_name)) %>% 
  group_by(last_name) %>% 
  summarise(n())

# fl_clean %>% 
#   select(full_address) %>% 
#   distinct() %>% 
#   write_csv(paste0(state_path, "fl_address.csv"))


# Clean Data --------------------------------------------------------------

fl_geo <- read_csv(paste0(state_path, "fl_geocode.csv"), guess_max = 1e7) %>% 
  select(full_address, block = `Full FIPS`)

florida <- fl_clean %>% 
  left_join(fl_geo, by = "full_address") %>% 
  mutate(county = str_sub(block, 1, 5)) %>% 
  select(first_name, last_name, female, party, apartment, birth_year, state, zip, county, block, race)

save(florida, file = "data/florida.rda")

