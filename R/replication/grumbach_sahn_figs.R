


# Load DIME data ----------------------------------------------------------

load(file = here("data", "bper_dimes.rda"))
load(file = here("data", "wru_dimes.rda"))

pop_race_stats <- read_csv(here("sahn_grumbach", "Population Race Statistics (Pew).csv")) %>% 
  filter(race != "White",
         type %in% c("Registered Voters", "Members of Congress")) %>% 
  select(race, share = pct.money, type)


# Figure Funcs ------------------------------------------------------------

gen_fig1_data <- function(df, method) {
  df <- df %>% 
    sample_n(1e6) %>% 
    mutate(pred_prob = case_when(pred_race == "black" ~ prob_black,
                                 pred_race == "api" ~ prob_api,
                                 pred_race == "white" ~ prob_white,
                                 pred_race == "hispanic" ~ prob_hispanic,
                                 TRUE ~ NA_real_),
           method := method) %>% 
    select(pred_prob, method)
  
  return(df)
}


gen_fig2_data <- function(df, method) {
  
  df <- df %>% 
    group_by(cycle, pred_race) %>% 
    summarise(race_total = sum(amount)) %>% 
    group_by(cycle) %>% 
    mutate(cycle_total = sum(race_total)) %>% 
    ungroup() %>% 
    mutate(share = race_total / cycle_total,
           method := method)
  
  return(df)
}


gen_fig3_data <- function(df, method) {
  contrib_table <- prop.table(table(df$pred_race))
  
  contrib_props <- tibble(
    race = c("Black", "Latino", "Asian"),
    share = c(contrib_table["black"], contrib_table["hispanic"], contrib_table["api"]),
    type := method
  )
  
  return(contrib_props)
}


gen_fig4_data <- function(df, method) {
  
  df <- df %>% 
    filter(pred_race %in% c("black", "api", "hispanic", "white"),
           r_pred_race %in% c("black", "api", "hispanic", "white")) %>% 
    group_by(pred_race, bonica.rid) %>% 
    summarise(avg_amount = sum(amount, na.rm = T), r_pred_race = first(r_pred_race)) %>% 
    group_by(pred_race, r_pred_race) %>% 
    summarise(avg_amount = mean(avg_amount, na.rm = T)) %>% 
    mutate(method := method)
  
  return(df)
}


# Generating Figure Data --------------------------------------------------

fig1 <- rbind(gen_fig1_data(bper_dimes, "bper"),
              gen_fig1_data(wru_dimes, "wru"))

fig2 <- rbind(
  gen_fig2_data(bper_dimes, "bper"),
  gen_fig2_data(wru_dimes, "wru")) %>% 
  filter(pred_race %in% c("api", "black", "hispanic")) %>%
  mutate(year = as.numeric(cycle),
         method = factor(method, levels = c("wru", "bper")))

fig3 <- rbind(
  gen_fig3_data(bper_dimes, "bper Contributors"),
  gen_fig3_data(wru_dimes, "wru Contributors"),
  pop_race_stats) %>% 
  mutate(type = factor(type, levels = c("Registered Voters", "Members of Congress",
                                        "wru Contributors", "bper Contributors")),
         race = case_when(race == "Latino" ~ "Hispanic",
                          TRUE ~ race))

fig4 <- rbind(
  gen_fig4_data(bper_dimes, "bper"),
  gen_fig4_data(wru_dimes, "wru")
)


# Saving ------------------------------------------------------------------

sahn_grumbach_figs <- list(fig1, fig2, fig3, fig4)

save(sahn_grumbach_figs, file = here("data", "sahn_grumbach_figs.rda"))