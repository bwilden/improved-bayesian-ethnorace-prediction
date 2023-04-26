library(targets)

tar_source(here::here("R", "prep_funcs.R"))
tar_source(here::here("R", "validation_funcs.R"))
tar_source(here::here("R", "plot_funcs.R"))
tar_source(here::here("R", "replication_funcs.R"))
tar_source(here::here("R", "calibration_funcs.R"))

options(tidyverse.quiet = TRUE)
set.seed(111)

tar_option_set(packages = c("here",
                            "tidyverse",
                            "sf",
                            "tidycensus",
                            "wru",
                            "bper",
                            "patchwork",
                            "MetBrewer",
                            "estimatr",
                            "broom"))
suppressWarnings(library(tidyverse))

list(
  # Prep NC/Florida files
  tar_target(
    raw_nc_voter_file,
    here("data-raw", "north_carolina", "VR_Snapshot_20190101_fix.txt"),
    format = "file"
  ),
  tar_target(
    raw_florida_voter_file,
    list.files(path = here("data-raw", "florida", "20190212_VoterDetail"), 
               pattern = "*.txt", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    clean_nc_voter_file,
    clean_voter_file(raw_nc_voter_file, "north_carolina", 2019)
  ),
  tar_target(
    clean_florida_voter_file,
    clean_voter_file(raw_florida_voter_file, "florida", 2019)
  ),
  tar_target(
    florida_splits,
    split_voter_file(clean_florida_voter_file,
                     "florida",
                     n_split = 4)
  ),
  tar_target(
    florida_addresses,
    list.files(path = here("data-raw", "florida"),
               pattern = "*Geocoded.csv", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    nc_addresses,
    here("data-raw", "north_carolina", "nc_20190101_address_Geocoded.csv"),
    format = "file"
  ),
  tar_target(
    geocoded_florida_voter_file,
    apply_spatial_merge(clean_florida_voter_file,
                        florida_addresses,
                        "florida", 2019)
  ),
  tar_target(
    geocoded_nc_voter_file,
    apply_spatial_merge(clean_nc_voter_file,
                        nc_addresses,
                        "north_carolina", 2019)
  ),
  tar_target(
    state_census_data,
    get_state_census_data(c("NC", "FL"))
  ),
  tar_target(
    combined_voter_file,
    assemble_states(list(geocoded_florida_voter_file,
                         geocoded_nc_voter_file),
                    c("NC", "FL"),
                    acs_dat = state_census_data$acs_dat,
                    decennial = state_census_data$decennial)
  ),
  
  # BISG tests
  tar_target(
    bper_data,
    bper::load_bper_data(combined_voter_file, year = 2019)
  ),
  tar_target(
    bper_tests,
    pmap_dfr(.l = set_bisg_args(),
         .f = classify_and_report,
         bper_data = bper_data,
         df = combined_voter_file |> 
           slice_sample(n = 2e6),
         .progress = TRUE)
  ),
  tar_target(
    bper_metrics,
    calc_validation_results(bper_tests)
  ),
  tar_target(
    bper_plots,
    map(c("Accuracy", "Precision", "Recall"), 
        make_metric_plot,
        metric_df = bper_metrics)
  ),
  tar_target(
    bper_predictions,
    bper::impute_ethnorace(combined_voter_file |>  
                             slice_sample(n = 1e6), 
                             year = 2019,
                             bper_data = bper_data)
  ),
  tar_target(
    bper_cal_data,
    map_dfr(c("aapi", "black", "hispanic", "white"),
           create_calibration_plot_data,
           bper_cal = bper_predictions)
  ),
  tar_target(
    bper_cal_plot,
    make_calibration_plots(bper_cal_data)
  ),
  tar_target(
    bias_prec_plots,
    map(c("pct_foreignborn", "pct_englishpoor", "median_income", 
          "median_home_value", "pct_college"),
        make_bias_prec_plot,
        pred_df = bper_predictions)
  ),
  tar_target(
    bias_recall_plots,
    pmap(crossing(bias_var = c("pct_foreignborn", "pct_englishpoor", "median_income", 
                               "median_home_value", "pct_college"),
                  plot_race = c("aapi", "black", "hispanic", "white")),
         make_bias_recall_plot,
         pred_df = bper_predictions)
  ),
  
  # Grumbach Sahn Replication
  tar_target(
    raw_dime_contribs,
    list.files(path = here("data-raw", "dime"), pattern = "contribDB_*",
               full.names = TRUE),
    format = "file"
  ),
  tar_target(
    raw_dime_recips,
    here("data-raw", "dime", "dime_recipients_all_1979_2014.csv"),
    format = "file"
  ),
  tar_target(
    dime_contribs,
    map_dfr(raw_dime_contribs, 
            clean_contribs,
            .progress = TRUE)
  ),
  tar_target(
    dime_recips,
    clean_recips(raw_dime_recips)
  ),
  tar_target(
    bper_dimes,
    calc_bper_dime(dime_contribs, dime_recips)
  ),
  tar_target(
    wru_dimes,
    calc_wru_dime(dime_contribs, dime_recips)
  ),
  # tar_target(
  #   dime_metric_tables,
  #   create_dime_metric_tables(validation_results)
  # ),
  
  # Grumbach Sahn Figures
  tar_target(
    gs_figs_data,
    prep_gs_figs_data(bper_dimes, wru_dimes)
  ),
  tar_target(
    gs_fig1,
    create_gs_fig1(gs_figs_data$fig1_data)
  ),
  tar_target(
    gs_fig2,
    create_gs_fig2(gs_figs_data$fig2_data)
  ),
  tar_target(
    gs_fig3,
    create_gs_fig3(gs_figs_data$fig3_data)
  ),
  tar_target(
    gs_fig4,
    create_gs_fig4(gs_figs_data$fig4_data)
  ),
  tar_target(
    gs_did_data,
    rbind(
      prep_gs_did_data(bper_dimes, "bper"),
      prep_gs_did_data(wru_dimes, "wru")
    )
  ),
  tar_target(
    gs_did_results,
    run_gs_did_models(gs_did_data)
  ),
  tar_target(
    gs_fig5,
    create_gs_fig5(gs_did_results)
  )
)
