

nhgis_colnames <- c()
for (race in c(
  "black", "aian", "asian", "pi", "other", "twoormore", "hispanic", "white")) {
  for (sex in c(
    "M", "F")) {
    for (age in c(
      "t5", "5t9", "10t14", "15t17", "18t19", "20", "21", "22t24", "25t29", 
      "30t34", "35t39", "40t44", "45t49", "50t54", "55t59", "60t61", "62t64", 
      "65t66", "67t69", "70t74", "75t79", "80t84", "85t"
    )) {
      cname <- paste(sex, age, race, sep = "_")
      nhgis_colnames <- c(nhgis_colnames, cname)
    }
  }
}

clean_sex_file <- function(df, geo_level, psuedocount) {
  
  pr_colnames <- c("sex", "age", "pr_sex_agaian", "pr_sex_agapi", "pr_sex_agblack", 
                   "pr_sex_aghispanic", "pr_sex_agother", "pr_sex_agwhite")
  
  
  df <- df %>% 
    mutate(geo_sum = rowSums(across(contains("001")))) %>%
    filter(geo_sum > 0) %>% 
    select(geo_level, starts_with("H9"), -contains(c("001", "002", "026"))) %>% 
    `colnames<-` (c("geo_level", nhgis_colnames)) %>% 
    pivot_longer(
      -geo_level,
      names_to = c("sex", "age", "race"),
      names_sep = "_",
      values_to = "obs"
    ) %>% 
    mutate(race = case_when(race == "asian" ~ "api",
                            race == "pi" ~ "api",
                            race == "twoormore" ~ "other",
                            TRUE ~ race)) %>% 
    group_by(geo_level, sex, age, race) %>% 
    summarise(obs = sum(obs) + psuedocount) %>% 
    group_by(geo_level, age, race) %>% 
    mutate(total_agrace = sum(obs)) %>% 
    ungroup() %>% 
    mutate(pr_sex_agrace = obs / total_agrace) %>% 
    pivot_wider(id_cols = c("geo_level", "sex", "age"),
                names_from = race, 
                values_from = pr_sex_agrace) %>% 
    `colnames<-` (c(geo_level, pr_colnames))
  
  return(df)
}



clean_age_file <- function(df, geo_level, psuedocount) {
  
  pr_colnames <- c("age", "pr_age_gaian", "pr_age_gapi", "pr_age_gblack", 
                   "pr_age_ghispanic", "pr_age_gother", "pr_age_gwhite")

  
  df <- df %>% 
    mutate(geo_sum = rowSums(across(contains("001")))) %>%
    filter(geo_sum > 0) %>% 
    select(geo_level, starts_with("H9"), -contains(c("001", "002", "026"))) %>% 
    `colnames<-` (c("geo_level", nhgis_colnames)) %>% 
    pivot_longer(
      -geo_level,
      names_to = c("sex", "age", "race"),
      names_sep = "_",
      values_to = "obs"
    ) %>% 
    mutate(race = case_when(race == "asian" ~ "api",
                            race == "pi" ~ "api",
                            race == "twoormore" ~ "other",
                            TRUE ~ race)) %>% 
    group_by(geo_level, age, race) %>% 
    summarise(obs = sum(obs) + psuedocount) %>% 
    group_by(geo_level, race) %>% 
    mutate(total_grace = sum(obs)) %>% 
    ungroup() %>% 
    mutate(pr_age_grace = obs / total_grace) %>% 
    pivot_wider(id_cols = c("geo_level", "age"),
                names_from = race, 
                values_from = pr_age_grace) %>% 
    `colnames<-` (c(geo_level, pr_colnames))
  
  return(df)
}






# State -------------------------------------------------------------------


state_code_conc <- read_csv(here("data-raw", "state_code_conc.csv")) %>%
  mutate(STATEA = as.character(STATE),
                STATEA = str_pad(STATEA, 2, pad = "0")) %>%
  select(c(STATEA, state = STUSAB))

states_sex <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_state_sexage.csv")) %>% 
  left_join(state_code_conc, by = "STATEA") %>% 
  clean_sex_file(geo_level = "state", psuedocount = 0)
  
states_age <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_state_sexage.csv")) %>% 
  left_join(state_code_conc, by = "STATEA") %>%
  clean_age_file(geo_level = "state", psuedocount = 0)


# County ------------------------------------------------------------------


counties_sex <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_county_sexage.csv")) %>% 
  mutate(county = paste0(STATEA, COUNTYA)) %>% 
  clean_sex_file(geo_level = "county", psuedocount = 0)

counties_age <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_county_sexage.csv")) %>% 
  mutate(county = paste0(STATEA, COUNTYA)) %>% 
  clean_age_file(geo_level = "county", psuedocount = 0)


# Place -------------------------------------------------------------------


places_sex <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_place_sexage.csv")) %>% 
  mutate(place = paste0(STATEA, PLACEA)) %>% 
  clean_sex_file(geo_level = "place", psuedocount = 0)

places_age <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_place_sexage.csv")) %>% 
  mutate(place = paste0(STATEA, PLACEA)) %>% 
  clean_age_file(geo_level = "place", psuedocount = 0)


# ZIP ---------------------------------------------------------------------


zips_sex <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_zip_sexage.csv")) %>% 
  mutate(zip = as.character(ZCTA5A)) %>% 
  clean_sex_file(geo_level = "zip", psuedocount = 0)

zips_age <- read_csv(here("data-raw", "sexage_priors", "nhgis_2010_zip_sexage.csv")) %>% 
  mutate(zip = as.character(ZCTA5A)) %>% 
  clean_age_file(geo_level = "zip", psuedocount = 0)


# Block -------------------------------------------------------------------

blocks_sex <- vroom::vroom(here("data-raw", "nc_fl_blocks", "nhgis_2010_block_sexage_ncfl.csv")) %>% 
  mutate(
    block = paste0(
      as.character(STATEA),
      as.character(COUNTYA),
      as.character(TRACTA),
      as.character(BLOCKA)
    )) %>% 
  clean_sex_file(geo_level = "block", psuedocount = 0.01)
save(blocks_sex, file = here("data", "blocks_sex.rda"))

blocks_age <- vroom::vroom(here("data-raw", "nc_fl_blocks", "nhgis_2010_block_sexage_ncfl.csv")) %>% 
  mutate(
    block = paste0(
      as.character(STATEA),
      as.character(COUNTYA),
      as.character(TRACTA),
      as.character(BLOCKA)
    )) %>% 
  clean_age_file(geo_level = "block", psuedocount = 0.01)
save(blocks_age, file = here("data", "blocks_age.rda"))