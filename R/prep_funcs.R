
# Clean raw data from state voter files
clean_voter_file <- function(voter_file, state, year) {
  common_surname_prefixes <- c("MC", "DE", "VAN", "DEL", "ST", "LA", "DI", 
                               "JEAN", "LE", "MAC", "SAINT", "DA ", "O", "VON", 
                               "SAN", "DU", "EL", "LO", "DOS", "AL", "D", "DELLA", 
                               "DELA", "DES", "L")
  key_voter_file_vars <- c("first_name", "last_name", "address", "city", "state", 
                           "zip", "sex", "age", "party", "multi_unit", "party", "race")
  
  if (state == "florida") {
    df <- map_df(voter_file, ~read_tsv(., guess_max = 4e4, col_names = F,
                                  col_types = cols(.default = "c"))) 
    df <- df %>% 
      filter(X7 != "Y") %>% 
      mutate(multi_unit = case_when(is.na(X9) ~ 0, TRUE ~ 1),
             last_name = toupper(gsub("\\W+", " ", X3)),
             last_name = if_else(
               grepl(paste(common_surname_prefixes, collapse = " |^"), last_name), 
               gsub(" ", "", last_name),
               gsub(" .*", "", last_name)),
             first_name = toupper(X5),
             first_name = str_squish((str_replace_all(first_name, regex("\\W+"), ""))),
             address = str_squish(X8),
             city = X10,
             state = "FL",
             zip = str_sub(X12, 1, 5),
             sex = case_when(X20 == "F" ~ 1, TRUE ~ 0),
             race = case_when(X21 == "1" ~ "aian",
                              X21 == "2" ~ "aapi",
                              X21 == "3" ~ "black",
                              X21 == "4" ~ "hispanic",
                              X21 == "5" ~ "white",
                              X21 == "7" ~ "multi",
                              TRUE ~ "other"),
             birth_year = format(as.Date(X22, format = "%m/%d/%Y"), "%Y"),
             age := year - as.numeric(birth_year),
             party = case_when(X24 %in% c("REP", "DEM") ~ X24, TRUE ~ "UNA"))
  }
  if (state == "north_carolina") {
    df <- read_tsv(voter_file, skip = 1, col_names = FALSE) %>% 
      mutate(multi_unit = case_when(!is.na(X23) ~ 1, TRUE ~ 0),
             across(c(X16, X17, X18, X19, X20), ~str_replace_na(., "")),
             address = str_squish(paste(X16, X17, X18, X19, X20)),
             last_name = toupper(gsub("\\W+", " ", X12)),
             last_name = if_else(
               grepl(paste(common_surname_prefixes, collapse = " |^"), last_name), 
               gsub(" ", "", last_name),
               gsub(" .*", "", last_name)),
             first_name = X13,
             city = X24,
             state = X25,
             zip = as.character(X26),
             age = X44,
             sex = case_when(X42 == "F" ~ 1, TRUE ~ 0),
             race = case_when(X38 == "HL" ~ "hispanic",
                              X36 == "A" | X36 == "P" ~ "aapi",
                              X36 == "B" ~ "black",
                              X36 == "I" ~ "aian",
                              X36 == "O" | X36 == "U" ~ "other",
                              X36 == "M" ~ "multi",
                              X36 == "W" ~ "white"),
             party = case_when(X40 %in% c("REP", "DEM") ~  X40, TRUE ~ "UNA"))
  }
  df <- df %>% 
    select(all_of(key_voter_file_vars))
  return(df)
}

# Split voter file ahead of geocoding so that ArcGIS doesn't crash
split_voter_file <- function(voter_file, state, n_splits) {
  voter_file_address <- voter_file %>%
    select(address, city, state, zip) %>%
    distinct() %>% 
    group_by((row_number() - 1) %/% (n() / n_splits)) %>% 
    nest %>% 
    pull(data)
  
  for (i in 1:length(voter_file_address)) {
    write_csv(voter_file_address[[i]],
      file = here("data-raw", state, paste0(state, "_address_", i, ".csv")))
  }
}

apply_spatial_merge <- function(voter_file, address_path, state, year) {
  if (state == "florida") {
    state_abbrv <- "FL"
    coords_df <- map_df(address_path, ~read_csv(., guess_max = 1e5) %>% 
                          select(USER_address, USER_city, USER_state, USER_zip,
                                 Status, X, Y)) 
  } 
  if (state == "north_carolina") {
    state_abbrv <- "NC"
    coords_df <- read_csv(address_path)
  }
  
  block_data <- tigris::blocks(state = state_abbrv, class = "sf", year = year)
  place_data <- tigris::places(state = state_abbrv, class = "sf", year = year)
  
  coords_df <- coords_df %>% 
    filter(Status != "U") %>% 
    select(address = USER_address, 
           city = USER_city, 
           state = USER_state, 
           zip = USER_zip, 
           X, Y) %>% 
    st_as_sf(coords = c("X", "Y"),
             crs = st_crs(block_data),
             remove = F)
  
  geos_df <- coords_df %>%  
    st_join(block_data) %>% 
    st_join(place_data) %>% 
    mutate(zip = as.character(zip)) %>% 
    select(address, city, state, zip, X, Y,
           county = COUNTYFP10, tract = TRACTCE10, block = BLOCKCE10, place = PLACEFP)
  
  geocoded_df <- voter_file %>% 
    left_join(geos_df, by = c("address", "city", "state", "zip")) %>% 
    select(first_name, last_name, sex, party, multi_unit, 
           age, state, zip, county, tract, place, block, race, X, Y)
  
  return(geocoded_df)
}

get_state_census_data <- function(state_abbrvs) {
  acs_dat <- get_acs(geography = "tract",
                     variables = c("total_pop" = "B01001_001",
                                   "foreign_born" = "B05006_001",
                                   "english_less_than_very_well" = "B16004_003",
                                   "median_income" = "B19013_001",
                                   "bachelors_degree" = "B15003_022",
                                   "median_home_value" = "B25077_001"),
                     output = "wide",
                     state = state_abbrvs,
                     year = 2019) %>% 
    mutate(pct_foreignborn = foreign_bornE / total_popE,
           pct_englishpoor = english_less_than_very_wellE / total_popE,
           median_income = median_incomeE,
           median_home_value = median_home_valueE,
           pct_college = bachelors_degreeE / total_popE,
           # For some reason I wanted terciles?
           across(c(pct_foreignborn, pct_englishpoor, median_income, pct_college, median_home_value),
                  ~ cut(., quantile(., probs = c(0, .33, .66, 1), na.rm = TRUE), 
                        labels = c("Low", "Middle", "High")),
                  .names = "{.col}_level"),
           state_code = str_sub(GEOID, 1, 2),
           county = str_sub(GEOID, 3, 5),
           tract = str_sub(GEOID, 6, 11)) %>% 
    select(pct_foreignborn, pct_englishpoor, median_income, median_home_value, pct_college,
           state_code, county, tract)
    # select(contains("level"), state_code, county, tract)
  
  decennial_dat <- get_decennial("block", "P002002", 
                                 state = state_abbrvs) %>% 
    mutate(urban = ifelse(value > 0, 1, 0),
           state_code = str_sub(GEOID, 1, 2),
           county = str_sub(GEOID, 3, 5),
           tract = str_sub(GEOID, 6, 11),
           block = str_sub(GEOID, 12, 15)) %>% 
    select(-c(GEOID, NAME, variable, value))
  
  return(lst(acs_dat, decennial_dat))
}

assemble_states <- function(state_voter_file_list,
                            state_abbrvs,
                            acs_dat,
                            decennial_dat) {
  combined_voter_files <- list_rbind(state_voter_file_list) %>% 
    # Get rid of missing races and people in the wrong states
    filter(!(is.na(race)),
           state %in% state_abbrvs) %>% 
    left_join(tidycensus::fips_codes %>% 
                select(state, state_code) %>% 
                unique()) %>% 
    mutate(VoterID = as.character(row_number()),
           # Dummies for calibration plots
           obs_black = factor(ifelse(race == "black", 1, 0), levels = c(1, 0)),
           obs_white = factor(ifelse(race == "white", 1, 0), levels = c(1, 0)),
           obs_hispanic = factor(ifelse(race == "hispanic", 1, 0), levels = c(1, 0)),
           obs_aapi = factor(ifelse(race == "aapi", 1, 0), levels = c(1, 0)),
           obs_aian = factor(ifelse(race == "aian", 1, 0), levels = c(1, 0)),
           obs_other = factor(ifelse(race %in% c("other", "multi"), 1, 0), levels = c(1, 0)),
           race = ifelse(race %in% c("aian", "multi"), "other", race)) %>% 
    # Merge in census demographic data
    left_join(decennial_dat, by = c("state_code", "county", "tract", "block")) %>% 
    left_join(acs_dat, by = c("state_code", "county", "tract"))
  return(combined_voter_files)
}
