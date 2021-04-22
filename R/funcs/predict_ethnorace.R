


arg_max_cols <- function(df, num_cols, max_col_name = "max_col") {
  cols <- df[utils::tail(seq_along(df), num_cols)]
  max_col <- colnames(cols)[max.col(cols)]
  df[[max_col_name]] <- max_col
  
  return(df)
}

`%notin%` <- Negate(`%in%`)


predict_ethnorace <- function(df, dichotomize = FALSE, prior) {
  
  # Prep Data/Correct Input Errors ------------------------------------------
  
  df <- df %>% dplyr::mutate(id = dplyr::row_number())
  
  # Adding columns if missing from original data set
  # Would love to find more concise code for this
  if ("apartment" %notin% colnames(df)) {
    df$apartment <- NA_integer_
  }
  if ("birth_year" %notin% colnames(df)) {
    df$birth_year <- NA_integer_
  }
  if ("block" %notin% colnames(df)) {
    df$block <- NA_character_
  }
  if ("county" %notin% colnames(df)) {
    df$county <- NA_character_
  }
  if ("first_name" %notin% colnames(df)) {
    df$first_name <- NA_character_
  }
  if ("female" %notin% colnames(df)) {
    df$female <- NA_integer_
  }
  if ("party" %notin% colnames(df)) {
    df$party <- NA_character_
  }
  if ("place" %notin% colnames(df)) {
    df$place <- NA_character_
  }
  if ("state" %notin% colnames(df)) {
    df$state <- NA_character_
  }
  if ("last_name" %notin% colnames(df)) {
    df$last_name <- NA_character_
  }
  if ("zip" %notin% colnames(df)) {
    df$zip <- NA_character_
  }
  
  # Fixing columns if wrong type
  if (is.character(df$birth_year)) {
    df$birth_year <- as.numeric(df$birth_year)
  }
  if (is.numeric(df$county)) {
    df$county <- as.character(df$county)
  }
  if (is.numeric(df$place)) {
    df$place <- as.character(df$place)
  }
  if (is.numeric(df$zip)) {
    df$zip <- as.character(df$zip)
  }
  if (is.numeric(df$block)) {
    df$block <- as.character(df$block)
  }
  
  if (!exists("blocks")) {
    blocks <- data.frame(block = NA)
  }
  
  
  # Merge Geolocation Probabilities -----------------------------------------
  
  # Split data into sets depending on which geolocation variable exists
  
  if (exists("blocks")) {
    block_matches <- df %>%
      dplyr::filter(!is.na(block)) %>%
      dplyr::left_join(blocks, by = "block")
  } else {
    block_matches <- data.frame(id = NA)
  }
  
  zip_matches <- df %>%
    dplyr::filter(!is.na(zip), id %notin% block_matches$id) %>%
    dplyr::left_join(zips, by = "zip")
  
  place_matches <- df %>%
    dplyr::filter(!is.na(place), id %notin% c(block_matches$id, zip_matches$id)) %>%
    dplyr::left_join(places, by = "place")
  
  county_matches <- df %>%
    dplyr::filter(!is.na(county), id %notin% c(block_matches$id, zip_matches$id, place_matches$id)) %>%
    dplyr::left_join(counties, by = "county")
  
  state_matches <- df %>%
    dplyr::filter(!is.na(state), id %notin% c(block_matches$id, zip_matches$id, place_matches$id, county_matches$id)) %>%
    dplyr::left_join(states, by = "state")
  
  none_matches <- df %>%
    dplyr::filter(id %notin% c(state_matches$id, block_matches$id, zip_matches$id, place_matches$id, county_matches$id)) %>%
    dplyr::mutate(GEOID = "nationwide") %>%
    dplyr::left_join(nationwide, by = "GEOID") %>%
    dplyr::select(-GEOID)
  
  df <- rbind(zip_matches, place_matches, county_matches, state_matches, none_matches)
  
  if (exists("blocks")) {
    df <- rbind(df, block_matches)
  }
  
  
  # Merge Other Probabilities -----------------------------------------------
  
  # Add "alt" names to merge with "all other" first/last name probabilities
  df <- df %>%
    dplyr::mutate(
      first_name_alt = ifelse(first_name %notin% firstnames$first_name,
                              "ALL OTHER FIRST NAMES", first_name
      ),
      last_name_alt = ifelse(last_name %notin% surnames$last_name,
                             "ALL OTHER NAMES", last_name
      )
    ) %>%
    dplyr::left_join(surnames, by = c("last_name_alt" = "last_name")) %>%
    dplyr::left_join(firstnames, by = c("first_name_alt" = "first_name")) %>%
    # Remove alternative names
    dplyr::select(-c(first_name_alt, last_name_alt)) %>%
    
    dplyr::left_join(parties, by = "party") %>%
    dplyr::left_join(apartments, by = "apartment") %>%
    dplyr::left_join(genders, by = "female") %>%
    dplyr::left_join(birth_years, by = "birth_year")
  
  
  # Compute Posterior Probabilities -----------------------------------------
  
  if (prior == "all") {
  
  df <- df %>%
    # Input pr = 1 for unmatched priors/likelihoods
    dplyr::mutate_at(dplyr::vars(dplyr::contains("pr_")), ~ ifelse(is.na(.), 1, .)) %>%
    # Doing all the math...
    dplyr::mutate(
      norm_factor_s = ((pr_black_s * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) +
                         (pr_white_s * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) +
                         (pr_hispanic_s * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
                         (pr_api_s * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) +
                         (pr_aian_s * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
                         (pr_other_s * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other)
      ),
      prob_black_s = pr_black_s *
        (pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_s,
      prob_white_s = pr_white_s *
        (pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_s,
      prob_hispanic_s = pr_hispanic_s *
        (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_s,
      prob_api_s = pr_api_s *
        (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_s,
      prob_aian_s = pr_aian_s *
        (pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_s,
      prob_other_s = pr_other_s *
        (pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_s,
      norm_factor_f = ((pr_black_f * pr_s_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) +
                         (pr_white_f * pr_s_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) +
                         (pr_hispanic_f * pr_s_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
                         (pr_api_f * pr_s_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) +
                         (pr_aian_f * pr_s_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
                         (pr_other_f * pr_s_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other)
      ),
      prob_black_f = pr_black_f *
        (pr_s_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_f,
      prob_white_f = pr_white_f *
        (pr_s_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_f,
      prob_hispanic_f = pr_hispanic_f *
        (pr_s_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_f,
      prob_api_f = pr_api_f *
        (pr_s_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_f,
      prob_aian_f = pr_aian_f *
        (pr_s_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_f,
      prob_other_f = pr_other_f *
        (pr_s_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_f,
      norm_factor_g = ((pr_black_g * pr_f_black * pr_s_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) +
                         (pr_white_g * pr_f_white * pr_s_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) +
                         (pr_hispanic_g * pr_f_hispanic * pr_s_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
                         (pr_api_g * pr_f_api * pr_s_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) +
                         (pr_aian_g * pr_f_aian * pr_s_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
                         (pr_other_g * pr_f_other * pr_s_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other)
      ),
      prob_black_g = pr_black_g *
        (pr_f_black * pr_s_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_g,
      prob_white_g = pr_white_g *
        (pr_f_white * pr_s_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_g,
      prob_hispanic_g = pr_hispanic_g *
        (pr_f_hispanic * pr_s_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_g,
      prob_api_g = pr_api_g *
        (pr_f_api * pr_s_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_g,
      prob_aian_g = pr_aian_g *
        (pr_f_aian * pr_s_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_g,
      prob_other_g = pr_other_g *
        (pr_f_other * pr_s_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_g,
      norm_factor_p = ((pr_black_p * pr_f_black * pr_g_black * pr_s_black * pr_a_black * pr_fem_black * pr_y_black) +
                         (pr_white_p * pr_f_white * pr_g_white * pr_s_white * pr_a_white * pr_fem_white * pr_y_white) +
                         (pr_hispanic_p * pr_f_hispanic * pr_g_hispanic * pr_s_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) +
                         (pr_api_p * pr_f_api * pr_g_api * pr_s_api * pr_a_api * pr_fem_api * pr_y_api) +
                         (pr_aian_p * pr_f_aian * pr_g_aian * pr_s_aian * pr_a_aian * pr_fem_aian * pr_y_aian) +
                         (pr_other_p * pr_f_other * pr_g_other * pr_s_other * pr_a_other * pr_fem_other * pr_y_other)
      ),
      prob_black_p = pr_black_p *
        (pr_f_black * pr_g_black * pr_s_black * pr_a_black * pr_fem_black * pr_y_black) / norm_factor_p,
      prob_white_p = pr_white_p *
        (pr_f_white * pr_g_white * pr_s_white * pr_a_white * pr_fem_white * pr_y_white) / norm_factor_p,
      prob_hispanic_p = pr_hispanic_p *
        (pr_f_hispanic * pr_g_hispanic * pr_s_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_p,
      prob_api_p = pr_api_p *
        (pr_f_api * pr_g_api * pr_s_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_p,
      prob_aian_p = pr_aian_p *
        (pr_f_aian * pr_g_aian * pr_s_aian * pr_a_aian * pr_fem_aian * pr_y_aian) / norm_factor_p,
      prob_other_p = pr_other_p *
        (pr_f_other * pr_g_other * pr_s_other * pr_a_other * pr_fem_other * pr_y_other) / norm_factor_p,
      norm_factor_a = ((pr_black_a * pr_f_black * pr_g_black * pr_p_black * pr_s_black * pr_fem_black * pr_y_black) +
                         (pr_white_a * pr_f_white * pr_g_white * pr_p_white * pr_s_white * pr_fem_white * pr_y_white) +
                         (pr_hispanic_a * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_s_hispanic * pr_fem_hispanic * pr_y_hispanic) +
                         (pr_api_a * pr_f_api * pr_g_api * pr_p_api * pr_s_api * pr_fem_api * pr_y_api) +
                         (pr_aian_a * pr_f_aian * pr_g_aian * pr_p_aian * pr_s_aian * pr_fem_aian * pr_y_aian) +
                         (pr_other_a * pr_f_other * pr_g_other * pr_p_other * pr_s_other * pr_fem_other * pr_y_other)
      ),
      prob_black_a = pr_black_a *
        (pr_f_black * pr_g_black * pr_p_black * pr_s_black * pr_fem_black * pr_y_black) / norm_factor_a,
      prob_white_a = pr_white_a *
        (pr_f_white * pr_g_white * pr_p_white * pr_s_white * pr_fem_white * pr_y_white) / norm_factor_a,
      prob_hispanic_a = pr_hispanic_a *
        (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_s_hispanic * pr_fem_hispanic * pr_y_hispanic) / norm_factor_a,
      prob_api_a = pr_api_a *
        (pr_f_api * pr_g_api * pr_p_api * pr_s_api * pr_fem_api * pr_y_api) / norm_factor_a,
      prob_aian_a = pr_aian_a *
        (pr_f_aian * pr_g_aian * pr_p_aian * pr_s_aian * pr_fem_aian * pr_y_aian) / norm_factor_a,
      prob_other_a = pr_other_a *
        (pr_f_other * pr_g_other * pr_p_other * pr_s_other * pr_fem_other * pr_y_other) / norm_factor_a,
      norm_factor_fem = ((pr_black_fem * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_s_black * pr_y_black) +
                           (pr_white_fem * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_s_white * pr_y_white) +
                           (pr_hispanic_fem * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_s_hispanic * pr_y_hispanic) +
                           (pr_api_fem * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_s_api * pr_y_api) +
                           (pr_aian_fem * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_s_aian * pr_y_aian) +
                           (pr_other_fem * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_s_other * pr_y_other)
      ),
      prob_black_fem = pr_black_fem *
        (pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_s_black * pr_y_black) / norm_factor_fem,
      prob_white_fem = pr_white_fem *
        (pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_s_white * pr_y_white) / norm_factor_fem,
      prob_hispanic_fem = pr_hispanic_fem *
        (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_s_hispanic * pr_y_hispanic) / norm_factor_fem,
      prob_api_fem = pr_api_fem *
        (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_s_api * pr_y_api) / norm_factor_fem,
      prob_aian_fem = pr_aian_fem *
        (pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_s_aian * pr_y_aian) / norm_factor_fem,
      prob_other_fem = pr_other_fem *
        (pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_s_other * pr_y_other) / norm_factor_fem,
      norm_factor_y = ((pr_black_y * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_s_black) +
                         (pr_white_y * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_s_white) +
                         (pr_hispanic_y * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_s_hispanic) +
                         (pr_api_y * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_s_api) +
                         (pr_aian_y * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_s_aian) +
                         (pr_other_y * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_s_other)
      ),
      prob_black_y = pr_black_y *
        (pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_s_black) / norm_factor_y,
      prob_white_y = pr_white_y *
        (pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_s_white) / norm_factor_y,
      prob_hispanic_y = pr_hispanic_y *
        (pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_s_hispanic) / norm_factor_y,
      prob_api_y = pr_api_y *
        (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_s_api) / norm_factor_y,
      prob_aian_y = pr_aian_y *
        (pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_s_aian) / norm_factor_y,
      prob_other_y = pr_other_y *
        (pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_s_other) / norm_factor_y
    )
  
  # Calculates means of all choices of "prior"
  df <- df %>%
    dplyr::mutate(
      prob_black = rowMeans(dplyr::across(dplyr::contains("prob_black"))),
      prob_white = rowMeans(dplyr::across(dplyr::contains("prob_white"))),
      prob_hispanic = rowMeans(dplyr::across(dplyr::contains("prob_hispanic"))),
      prob_api = rowMeans(dplyr::across(dplyr::contains("prob_api"))),
      prob_aian = rowMeans(dplyr::across(dplyr::contains("prob_aian"))),
      prob_other = rowMeans(dplyr::across(dplyr::contains("prob_other")))
    )
  
  } else if (prior == "last_name") {
    
    df <- df %>%
      # Input pr = 1 for unmatched priors/likelihoods
      dplyr::mutate_at(dplyr::vars(dplyr::contains("pr_")), ~ ifelse(is.na(.), 1, .)) %>%
      # Doing all the math...
      dplyr::mutate(
        norm_factor_s = ((
          pr_black_s * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black
        ) +
          (
            pr_white_s * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white
          ) +
          (
            pr_hispanic_s * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic
          ) +
          (
            pr_api_s * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api
          ) +
          (
            pr_aian_s * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian
          ) +
          (
            pr_other_s * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other
          )
        ),
        prob_black = pr_black_s *
          (
            pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black
          ) / norm_factor_s,
        prob_white = pr_white_s *
          (
            pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white
          ) / norm_factor_s,
        prob_hispanic = pr_hispanic_s *
          (
            pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic
          ) / norm_factor_s,
        prob_api = pr_api_s *
          (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_s,
        prob_aian = pr_aian_s *
          (
            pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian
          ) / norm_factor_s,
        prob_other = pr_other_s *
          (
            pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other
          ) / norm_factor_s
      )
    
  } else if (prior == "first_name") {
    
    df <- df %>%
      # Input pr = 1 for unmatched priors/likelihoods
      dplyr::mutate_at(dplyr::vars(dplyr::contains("pr_")), ~ ifelse(is.na(.), 1, .)) %>%
      # Doing all the math...
      dplyr::mutate(
        norm_factor_f = ((
          pr_black_f * pr_s_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black
        ) +
          (
            pr_white_f * pr_s_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white
          ) +
          (
            pr_hispanic_f * pr_s_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic
          ) +
          (
            pr_api_f * pr_s_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api
          ) +
          (
            pr_aian_f * pr_s_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian
          ) +
          (
            pr_other_f * pr_s_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other
          )
        ),
        prob_black = pr_black_f *
          (
            pr_s_black * pr_g_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black
          ) / norm_factor_f,
        prob_white = pr_white_f *
          (
            pr_s_white * pr_g_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white
          ) / norm_factor_f,
        prob_hispanic = pr_hispanic_f *
          (
            pr_s_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic
          ) / norm_factor_f,
        prob_api = pr_api_f *
          (pr_s_api * pr_g_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_f,
        prob_aian = pr_aian_f *
          (
            pr_s_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian
          ) / norm_factor_f,
        prob_other = pr_other_f *
          (
            pr_s_other * pr_g_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other
          ) / norm_factor_f
      ) 
    
  } else if (prior == "geo") {
    
    df <- df %>%
      # Input pr = 1 for unmatched priors/likelihoods
      dplyr::mutate_at(dplyr::vars(dplyr::contains("pr_")), ~ ifelse(is.na(.), 1, .)) %>%
      mutate(
        norm_factor_g = ((
          pr_black_g * pr_f_black * pr_s_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black
        ) +
          (
            pr_white_g * pr_f_white * pr_s_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white
          ) +
          (
            pr_hispanic_g * pr_f_hispanic * pr_s_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic
          ) +
          (
            pr_api_g * pr_f_api * pr_s_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api
          ) +
          (
            pr_aian_g * pr_f_aian * pr_s_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian
          ) +
          (
            pr_other_g * pr_f_other * pr_s_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other
          )
        ),
        prob_black = pr_black_g *
          (
            pr_f_black * pr_s_black * pr_p_black * pr_a_black * pr_fem_black * pr_y_black
          ) / norm_factor_g,
        prob_white = pr_white_g *
          (
            pr_f_white * pr_s_white * pr_p_white * pr_a_white * pr_fem_white * pr_y_white
          ) / norm_factor_g,
        prob_hispanic = pr_hispanic_g *
          (
            pr_f_hispanic * pr_s_hispanic * pr_p_hispanic * pr_a_hispanic * pr_fem_hispanic * pr_y_hispanic
          ) / norm_factor_g,
        prob_api = pr_api_g *
          (pr_f_api * pr_s_api * pr_p_api * pr_a_api * pr_fem_api * pr_y_api) / norm_factor_g,
        prob_aian = pr_aian_g *
          (
            pr_f_aian * pr_s_aian * pr_p_aian * pr_a_aian * pr_fem_aian * pr_y_aian
          ) / norm_factor_g,
        prob_other = pr_other_g *
          (
            pr_f_other * pr_s_other * pr_p_other * pr_a_other * pr_fem_other * pr_y_other
          ) / norm_factor_g
      )
    
  }
  
  
  
  
  
  
  
  # Calculate highest posterior probability and impute race/ethnicity label
  df <- arg_max_cols(df, 6, max_col_name = "pred_race") %>%
    dplyr::mutate(pred_race = gsub("prob_", "", pred_race))
  
  
  
  # Dichotomize Option ------------------------------------------------------
  
  if (dichotomize == TRUE) {
    df <- df %>%
      dplyr::mutate(
        pred_black = factor(ifelse(pred_race == "black", 1, 0), levels = c(1, 0)),
        pred_white = factor(ifelse(pred_race == "white", 1, 0), levels = c(1, 0)),
        pred_hispanic = factor(ifelse(pred_race == "hispanic", 1, 0), levels = c(1, 0)),
        pred_api = factor(ifelse(pred_race == "api", 1, 0), levels = c(1, 0)),
        pred_aian = factor(ifelse(pred_race == "aian", 1, 0), levels = c(1, 0)),
        pred_other = factor(ifelse(pred_race == "other", 1, 0), levels = c(1, 0))
      )
  }
  
  
  # Clean Up Remaining Columns ----------------------------------------------
  
  df <- df %>%
    dplyr::select(-"id") %>%
    dplyr::select(!(dplyr::starts_with(c("pr_", "norm")))) %>%
    dplyr::select(!(dplyr::ends_with(
      c("_s", "_f", "_g", "_fem", "_y", "_p", "_a")
    )))
  
  return(df)
}







predict_ethnorace2 <- function(df, prior) {
  
  # Prep Data/Correct Input Errors ------------------------------------------
  
  df <- df %>% mutate(id = row_number())
  
  if ("apartment" %notin% colnames(df)) {
    df$apartment <- NA_integer_
  }
  if ("birth_year" %notin% colnames(df)) {
    df$birth_year <- NA_integer_
  }
  if ("block" %notin% colnames(df)) {
    df$block <- NA_character_
  }
  if ("county" %notin% colnames(df)) {
    df$county <- NA_character_
  }
  if ("first_name" %notin% colnames(df)) {
    df$first_name <- NA_character_
  }
  if ("female" %notin% colnames(df)) {
    df$female <- NA_integer_
  }
  if ("party" %notin% colnames(df)) {
    df$party <- NA_character_
  }
  if ("place" %notin% colnames(df)) {
    df$place <- NA_character_
  }
  if ("state" %notin% colnames(df)) {
    df$state <- NA_character_
  }
  if ("last_name" %notin% colnames(df)) {
    df$last_name <- NA_character_
  }
  if ("zip" %notin% colnames(df)) {
    df$zip <- NA_character_
  }
  
  # Merge Geolocation Probabilities -----------------------------------------
  
  # Split data into sets depending on which geolocation variable exists
  

  if (exists("blocks")) {
    block_matches <- df %>%
      filter(!is.na(block)) %>%
      left_join(blocks, by = "block") %>% 
      left_join(blocks_sex, by = c("block", "age", "sex")) %>% 
      left_join(blocks_age, by = c("block", "age"))
  } else {
    block_matches <- data.frame(id = NA)
  }
  
  zip_matches <- df %>%
    filter(!is.na(zip), id %notin% block_matches$id) %>%
    left_join(zips, by = "zip") %>% 
    left_join(zips_sex, by = c("zip", "age", "sex")) %>% 
    left_join(zips_age, by = c("zip", "age"))
  
  place_matches <- df %>%
    filter(!is.na(place), id %notin% c(block_matches$id, zip_matches$id)) %>%
    left_join(places, by = "place") %>% 
    left_join(places_sex, by = c("place", "age", "sex")) %>% 
    left_join(places_age, by = c("place", "age"))
  
  county_matches <- df %>%
    filter(!is.na(county), id %notin% c(block_matches$id, zip_matches$id, place_matches$id)) %>%
    left_join(counties, by = "county") %>% 
    left_join(counties_sex, by = c("county", "age", "sex")) %>% 
    left_join(counties_age, by = c("county", "age"))
  
  state_matches <- df %>%
    filter(!is.na(state), id %notin% c(block_matches$id, zip_matches$id, place_matches$id, county_matches$id)) %>%
    left_join(states, by = "state") %>% 
    left_join(states_sex, by = c("state", "age", "sex")) %>% 
    left_join(states_age, by = c("state", "age"))
  
  # none_matches <- df %>%
  #   filter(id %notin% c(state_matches$id, block_matches$id, zip_matches$id, place_matches$id, county_matches$id)) %>%
  #   mutate(GEOID = "nationwide") %>%
  #   left_join(nationwide, by = "GEOID") %>%
  #   select(-GEOID)
  
  df <- rbind(zip_matches, place_matches, county_matches, state_matches)
  
  if (exists("blocks")) {
    df <- rbind(df, block_matches)
  }
  
  
  # Merge Other Probabilities -----------------------------------------------
  
  # Add "alt" names to merge with "all other" first/last name probabilities
  df <- df %>%
    mutate(
      first_name_alt = ifelse(first_name %notin% firstnames$first_name,
                              "ALL OTHER FIRST NAMES", first_name
      ),
      last_name_alt = ifelse(last_name %notin% surnames$last_name,
                             "ALL OTHER NAMES", last_name
      )
    ) %>%
    left_join(surnames, by = c("last_name_alt" = "last_name")) %>%
    left_join(firstnames, by = c("first_name_alt" = "first_name")) %>%
    # Remove alternative names
    select(-c(first_name_alt, last_name_alt)) %>%
    left_join(parties, by = "party") %>%
    left_join(apartments, by = "apartment")
  
  
  # Compute Posterior Probabilities -----------------------------------------
  


  df <- df %>%
    # Input pr = 1 for unmatched priors/likelihoods
    dplyr::mutate_at(dplyr::vars(dplyr::contains("pr_")), ~ ifelse(is.na(.), 1, .)) %>%
    # Doing all the math...
    dplyr::mutate(
      norm_factor_s = ((
        pr_black_s * pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_sex_agblack * pr_age_gblack
      ) +
        (
          pr_white_s * pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_sex_agwhite * pr_age_gwhite
        ) +
        (
          pr_hispanic_s * pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_sex_aghispanic * pr_age_ghispanic
        ) +
        (
          pr_api_s * pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_sex_agapi * pr_age_gapi
        ) +
        (
          pr_aian_s * pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_sex_agaian * pr_age_gaian
        ) +
        (
          pr_other_s * pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_sex_agother * pr_age_gother
        )
      ),
      prob_black = pr_black_s *
        (
          pr_f_black * pr_g_black * pr_p_black * pr_a_black * pr_sex_agblack * pr_age_gblack
        ) / norm_factor_s,
      prob_white = pr_white_s *
        (
          pr_f_white * pr_g_white * pr_p_white * pr_a_white * pr_sex_agwhite * pr_age_gwhite
        ) / norm_factor_s,
      prob_hispanic = pr_hispanic_s *
        (
          pr_f_hispanic * pr_g_hispanic * pr_p_hispanic * pr_a_hispanic * pr_sex_aghispanic * pr_age_ghispanic
        ) / norm_factor_s,
      prob_api = pr_api_s *
        (pr_f_api * pr_g_api * pr_p_api * pr_a_api * pr_sex_agapi * pr_age_gapi) / norm_factor_s,
      prob_aian = pr_aian_s *
        (
          pr_f_aian * pr_g_aian * pr_p_aian * pr_a_aian * pr_sex_agaian * pr_age_gaian
        ) / norm_factor_s,
      prob_other = pr_other_s *
        (
          pr_f_other * pr_g_other * pr_p_other * pr_a_other * pr_sex_agother * pr_age_gother
        ) / norm_factor_s
    )
  
  
  # Calculate highest posterior probability and impute race/ethnicity label
  df <- arg_max_cols(df, 6, max_col_name = "pred_race") %>%
    mutate(pred_race = gsub("prob_", "", pred_race))
  
  

  
  # Clean Up Remaining Columns ----------------------------------------------
  
  df <- df %>%
    select(-"id") %>%
    select(!(starts_with(c("pr_", "norm")))) %>%
    select(!(ends_with(
      c("_s", "_f", "_g", "_fem", "_y", "_p", "_a")
    )))
  
  return(df)
}
