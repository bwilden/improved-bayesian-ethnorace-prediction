
clean_contribs <- function(path) {
  state_abbrvs <- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 
                    'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 
                    'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 
                    'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 
                    'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 
                    'WY')
  contribs <- read_csv(path, col_types = cols(.default = "c")) %>% 
    filter(contributor.type == "I",
           contributor.state %in% state_abbrvs,
           seat == "federal:house") %>% 
    mutate(last_name = if_else(grepl("mc ", contributor.lname), 
                               str_remove_all(contributor.lname, " "), contributor.lname),
           last_name = toupper(str_extract(last_name, "[A-Za-z]+")),
           first_name = str_remove(contributor.fname, "^[A-Za-z] "),
           first_name = str_remove(first_name, "(dr )|(wm )|(ew )|(pa )|(the )|(la )|(mr )|(mrs )"),
           first_name = toupper(str_extract(first_name, "[A-Za-z]+")),
           county = str_sub(censustract, start = 3, end = 5),
           tract = str_sub(censustract, start = 6),
           zip = str_sub(contributor.zipcode, start = 1, end = 5),
           zip = str_pad(zip, width = 5, pad = "0"),
           sex = case_when(contributor.gender == "F" ~ 1,
                           TRUE ~ 0),
           surname = last_name,
           amount = as.numeric(amount),
           cycle = as.numeric(cycle),
           dist = case_when(cycle < 2000 ~ contributor.district.90s,
                            cycle < 2010 & cycle >= 2000 ~ contributor.district.00s,
                            cycle >= 2010 ~ contributor.district.10s)) %>% 
    select(cycle, amount, dist, bonica.rid, bonica.cid, sex, surname, first_name, 
           last_name, zip, state = contributor.state, county, tract, election.type)
  return(contribs)
}

clean_recips <- function(path) {
  state_abbrvs <- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 
                    'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 
                    'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 
                    'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 
                    'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 
                    'WY')
  recips <- read_csv(path) %>% 
    filter(state %in% state_abbrvs,
           seat == "federal:house") %>% 
    mutate(last_name = if_else(grepl("mc ", lname), 
                               str_remove_all(lname, " "), lname),
           last_name = toupper(str_extract(last_name, "[A-Za-z]+")),
           first_name = str_remove(fname, "^[A-Za-z] "),
           first_name = str_remove(first_name, "(dr )|(wm )|(ew )|(pa )|(the )|(la )|(mr )|(mrs )"),
           first_name = toupper(str_extract(first_name, "[A-Za-z]+")),
           party = case_when(party == 100 ~ "DEM",
                             party == 200 ~ "REP",
                             TRUE ~ "UNA"),
           sex = case_when(cand.gender == "F" ~ 1,
                           TRUE ~ 0),
           surname = last_name) %>% 
    select(bonica.rid, first_name, last_name, party, state, surname, sex) %>% 
    distinct()
}

calc_bper_dime <- function(contribs_df, recips_df) {
  bper_contribs <- tibble()
  for (year in unique(contribs_df$cycle)) {
    contrib_year <- contribs_df %>%
      filter(cycle == year) %>%
      impute_ethnorace(year = as.numeric(year),
                       census_key = "5e4c2b8438222753a7f4753fa78855eca73b9950")
    bper_contribs <- rbind(
      bper_contribs,
      contrib_year
    )
    print(year)
  }
  
  bper_recips <- recips_df %>% 
    impute_ethnorace(year = 2010,
                     census_key = "5e4c2b8438222753a7f4753fa78855eca73b9950") %>% 
    select(bonica.rid, r_party = party, r_sex = sex, 
           r_pred_race = pred_race)
  
  bper_dimes <- bper_contribs %>% 
    left_join(bper_recips)
  
  return(bper_dimes)
}

calc_wru_dime <- function(contribs_df, recips_df) {
  state_abbrvs <- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 
                    'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 
                    'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 
                    'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 
                    'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 
                    'WY')
  wru_surname_only_contribs <- contribs_df %>% 
    filter(is.na(tract)) %>%
    predict_race_wru(census.geo = "tract",
                     sex = TRUE, surname.only = TRUE)
  
  wru_full_contribs <- contribs_df %>% 
    filter(!(is.na(tract)),
           state %in% state_abbrvs) %>% 
    predict_race_wru(census.geo = "tract")
  
  wru_contribs <- rbind(wru_surname_only_contribs, wru_full_contribs) 
  
  wru_recips <- recips_df %>%
    predict_race_wru(surname.only = T,
                    party = party) %>%
    select(bonica.rid, r_party = party, r_sex = sex, 
           r_pred_race = pred_race)
  
  wru_dimes <- wru_contribs %>% 
    left_join(wru_recips)
  
  return(wru_dimes)
}

create_dime_metric_tables <- function(validation_results) {
  contrib_diff <- validation_results$comparison_df %>% 
    filter(Tract == "|",
           `Age-Sex` == "|",
           `No Party` == "|",
           `Income Level High` == "|") %>% 
    select(metric, bper_diff)
  
  contrib_bper <- validation_results$bper_df %>% 
    filter(Tract == "|",
           `Age-Sex` == "|",
           `No Party` == "|",
           `Income Level High` == "|") %>% 
    select(metric, value)
  
  contrib_metric_table <- contrib_diff %>% 
    left_join(contrib_bper) %>% 
    mutate(wru = value - bper_diff,
           metric = str_remove_all(metric, "prec_"),
           metric = str_remove_all(metric, "rec_"),
           metric = str_to_title(metric)) %>% 
    select(Metric = metric, bper = value, wru, `bper Difference` = bper_diff)
  
  return(lst(contrib_metric_table))
}

prep_gs_figs_data <- function(bper_df, wru_df) {
  fig1_data <- rbind(
    bper_df %>% 
      select(c(pred_aapi, pred_black, pred_hispanic, pred_white, pred_race)) %>% 
      mutate(Method = "bper"),
    wru_df %>% 
      select(c(pred_aapi, pred_black, pred_hispanic, pred_white, pred_race)) %>% 
      mutate(Method = "wru")
  ) %>% 
    mutate(`Posterior Probability` = case_when(pred_race == "black" ~ pred_black,
                                               pred_race == "aapi" ~ pred_aapi,
                                               pred_race == "white" ~ pred_white,
                                               pred_race == "hispanic" ~ pred_hispanic,
                                               TRUE ~ NA_real_)) %>% 
    group_by(Method) %>%
    sample_n(1e5)
  
  fig2_data <- rbind(
    bper_df %>% 
      select(c(cycle, amount, pred_race)) %>% 
      mutate(Method = "bper"),
    wru_df %>% 
      select(c(cycle, amount, pred_race)) %>% 
      mutate(Method = "wru")
  ) %>% 
    group_by(cycle, pred_race, Method) %>% 
    summarise(race_total = sum(amount)) %>% 
    group_by(cycle, Method) %>% 
    mutate(cycle_total = sum(race_total)) %>% 
    ungroup() %>% 
    mutate(Share = race_total / cycle_total,
           Race = case_when(pred_race == "aapi" ~ "Asian",
                            pred_race == "black" ~ "Black",
                            pred_race == "hispanic" ~ "Hispanic"),
           Year = cycle,
           Method = factor(Method, levels = c("wru", "bper"))) %>% 
    filter(pred_race %in% c("aapi", "black", "hispanic"))
  
  pop_race_stats <- read_csv(here("sahn_grumbach", "Population Race Statistics (Pew).csv")) %>% 
    filter(race != "White",
           type %in% c("Registered Voters", "Members of Congress")) %>% 
    select(Race = race, Freq = pct.money, type)
  
  fig3_data <- rbind(
    as.data.frame(prop.table(table(bper_df$pred_race))) %>% 
      mutate(type = "bper Contributors"),
    as.data.frame(prop.table(table(wru_df$pred_race))) %>% 
      mutate(type = "wru Contributors")
  ) %>% 
    mutate(Race = case_when(Var1 == "aapi" ~ "Asian",
                            Var1 == "black" ~ "Black",
                            Var1 == "hispanic" ~ "Hispanic",
                            TRUE ~ NA_character_)) %>% 
    filter(!is.na(Race)) %>% 
    select(-Var1) %>% 
    rbind(pop_race_stats) %>% 
    mutate(type = factor(type, levels = c("Registered Voters", "Members of Congress",
                                          "wru Contributors", "bper Contributors")),
           Race = case_when(Race == "Latino" ~ "Hispanic",
                            TRUE ~ Race))
  
  fig4_data <- rbind(
    bper_df %>% 
      select(pred_race, r_pred_race, bonica.rid, amount) %>% 
      mutate(Method = "bper"),
    wru_df %>% 
      select(pred_race, r_pred_race, bonica.rid, amount) %>% 
      mutate(Method = "wru")
  ) %>% 
    filter(pred_race %in% c("black", "aapi", "hispanic", "white"),
           r_pred_race %in% c("black", "aapi", "hispanic", "white")) %>% 
    group_by(pred_race, bonica.rid, Method) %>% 
    summarise(avg_amount = sum(amount, na.rm = T), r_pred_race = first(r_pred_race)) %>% 
    group_by(pred_race, r_pred_race, Method) %>% 
    summarise(avg_amount = mean(avg_amount, na.rm = T)) %>% 
    mutate(r_pred_race = case_when(r_pred_race == "aapi" ~ "Asian",
                                   r_pred_race == "black" ~ "Black",
                                   r_pred_race == "hispanic" ~ "Hispanic",
                                   r_pred_race == "white" ~ "White"),
           pred_race = case_when(pred_race == "aapi" ~ "Asian",
                                 pred_race == "black" ~ "Black",
                                 pred_race == "hispanic" ~ "Hispanic",
                                 pred_race == "white" ~ "White"))
  
  
  rm(bper_df, wru_df)
  gs_figs_data <- as.list(environment())
  return(gs_figs_data)
}

create_gs_fig1 <- function(fig1_data) {
  p <- fig1_data %>% 
    ggplot(aes(x = `Posterior Probability`, color = Method, fill = Method)) +
    geom_density(alpha = .65) +
    theme_minimal() +
    scale_fill_manual(values = rev(met.brewer("Isfahan1", 2))) +
    scale_color_manual(values = rev(met.brewer("Isfahan1", 2))) +
    labs(y = "Density", x = "Posterior Probability of Highest Probability Ethnorace") +
    theme(text = element_text(family = "serif"))
  return(p)
}

create_gs_fig2 <- function(fig2_data) {
  p <- fig2_data %>% 
    ggplot(aes(x = Year, y = Share, fill = Race)) +
    geom_col() +
    scale_fill_manual(values = rev(met.brewer("Isfahan1", 3))) +
    facet_wrap(~ Method) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          text = element_text(family = "serif"))
  return(p)
}

create_gs_fig3 <- function(fig3_data) {
  p <- fig3_data %>% 
    ggplot(aes(x = Race, fill = Race, y = Freq)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = rev(met.brewer("Isfahan1", 3))) +
    scale_y_continuous(breaks = c(0, .05, .1)) +
    guides(fill = F) +
    labs(x = "", y = "Proportion") +
    theme_minimal() +
    facet_wrap( ~ type, nrow = 1) +
    theme(text = element_text(family = "serif"))
  return(p)
}

create_gs_fig4 <- function(fig4_data) {
  p <- fig4_data %>% 
    ggplot(aes(x = r_pred_race, y = avg_amount, fill = Method)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = rev(met.brewer("Isfahan1", 2))) +
    labs(x = "Candidate Ethnorace",
         y = "Average Contributions (Total $)",
         fill = "Method",
         title = "Contributor Ethnorace") +
    theme_minimal() +
    facet_grid(Method ~ pred_race) +
    theme(axis.text.x = element_text(angle = 35),
          text = element_text(family = "serif"))
  return(p)
}

prep_gs_did_data <- function(df, method) {
  did_data <- df %>%
    filter(
      pred_race %in% c("black", "aapi", "hispanic", "white"),
      r_pred_race %in% c("black", "aapi", "hispanic", "white"),
      !is.na(dist),
      amount > 0,
      r_party == "DEM",
      election.type == "G"
    ) %>%
    group_by(cycle, dist, r_pred_race) %>%
    mutate(total_amount = sum(amount, na.rm = T)) %>%
    ungroup() %>%
    group_by(pred_race, cycle, dist, r_pred_race) %>%
    summarise(share = sum(amount, na.rm = T) / total_amount) %>%
    distinct() %>%
    ungroup() %>%
    pivot_wider(
      names_from = "pred_race",
      values_from = "share",
      names_prefix = "share_"
    ) %>%
    mutate(
      across(contains("share_"), ~ coalesce(., 0)),
      cand_A = ifelse(r_pred_race == "aapi", 1, 0),
      cand_B = ifelse(r_pred_race == "black", 1, 0),
      cand_H = ifelse(r_pred_race == "hispanic", 1, 0),
      cand_W = ifelse(r_pred_race == "white", 1, 0),
      method := method
    )
  
  return(did_data)
}


run_gs_did_models <- function(df) {
  dvs <-
    c("share_aapi",
      "share_black",
      "share_hispanic",
      "share_white")
  
  did_models <- tibble()
  for (method_type in unique(df$method)) {
    model_df <- df %>%
      filter(method == method_type)
    for (dv in dvs) {
      did_models <- rbind(
        did_models,
        lm_robust(
          model_df[[dv]] ~ cand_A + cand_B + cand_H,
          data = model_df,
          clusters = dist,
          se_type = "stata",
          fixed_effects = ~ dist + cycle
        ) %>%
          tidy() %>%
          mutate(outcome = dv,
                 Method = method_type)
      )
    }
  }
  return(did_models)
}

create_gs_fig5 <- function(plot_df) {
  p <- plot_df %>% 
    mutate(outcome = case_when(outcome == "share_aapi" ~ "Asian",
                               outcome == "share_black" ~ "Black",
                               outcome == "share_hispanic" ~ "Latino",
                               outcome == "share_white" ~ "White"),
           term = case_when(term == "cand_A" ~ "Asian\nCandidate",
                            term == "cand_B" ~ "Black\nCandidate",
                            term == "cand_H" ~ "Latino\nCandidate"),
           term = factor(term, 
                         levels = c("Latino\nCandidate",
                                    "Black\nCandidate", 
                                    "Asian\nCandidate"))) %>% 
    ggplot(aes(x = estimate, y = term, color = Method)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(position = position_dodge(width = -.75)) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = -.75),
                  width = 0) +
    scale_color_manual(values = rev(met.brewer("Isfahan1", 2))) +
    facet_wrap(~ outcome) +
    scale_y_discrete(limits = rev) +
    theme_bw() +
    labs(y = "", x = "Share Total of\nGeneral Election Individual Contribs.")
    theme(legend.title = element_blank(),
          text = element_text(family = "serif"))
  return(p)
}
