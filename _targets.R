library(targets)

source("R/prep_funcs.R")
source("R/validation_funcs.R")
source("R/plot_funcs.R")
source("R/replication_funcs.R")
source("R/calibration_funcs.R")
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
  tar_target(
    raw_nc_voter_file,
    here("bper", "data-raw", "north_carolina", "VR_Snapshot_20190101_fix.txt"),
    format = "file"
  ),
  tar_target(
    raw_florida_voter_file,
    list.files(path = here("bper", "data-raw", "florida", "20190212_VoterDetail"), 
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
    list.files(path = here("bper", "data-raw", "florida"),
               pattern = "*Geocoded.csv", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    nc_addresses,
    here("bper", "data-raw", "north_carolina", "nc_20190101_address_Geocoded.csv"),
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
    combined_voter_file,
    assemble_states(list(geocoded_florida_voter_file,
                         geocoded_nc_voter_file),
                    c("NC", "FL"))
  ),
  tar_target(
    wru_data,
    load_wru_data(c("NC", "FL"))
  ),
  tar_target(
    bper_data,
    bper::load_bper_data(combined_voter_file, year = 2019)
  ),
  tar_target(
    wru_tests,
    run_validation_tests(wru = TRUE,
                         df = combined_voter_file %>% 
                           sample_n(1e7),
                         wru_data = wru_data)
  ),
  tar_target(
    bper_tests,
    run_validation_tests(wru = FALSE,
                         df = combined_voter_file %>% 
                           sample_n(1e7),
                         bper_data = bper_data)
  ),
  tar_target(
    validation_results,
    calc_validation_results(bper_tests,
                            wru_tests)
  ),
  tar_target(
    comparison_plots,
    make_all_comparison_plots(validation_results$comparison_df)
  ),
  tar_target(
    bper_plots,
    make_all_bper_plots(validation_results$bper_df)
  ),
  tar_target(
    calibration_data,
    build_calibration_data(combined_voter_file,
                           wru_data = wru_data,
                           bper_data = bper_data)
  ),
  tar_target(
    brier_table,
    create_brier_table(calibration_data)
  ),
  tar_target(
    calibration_plot_data,
    map_dfr(c("aapi", "black", "hispanic", "white"),
            create_calibration_plot_data,
            calibration_data = calibration_data)
  ),
  tar_target(
    calibration_plots,
    make_calibration_plots(calibration_plot_data)
  ),
  
  # Grumbach Sahn Replication
  tar_target(
    raw_dime_contribs,
    list.files(path = here("bper", "data-raw", "dime"), pattern = "contribDB_*",
               full.names = TRUE),
    format = "file"
  ),
  tar_target(
    raw_dime_recips,
    here("bper", "data-raw", "dime", "dime_recipients_all_1979_2014.csv"),
    format = "file"
  ),
  tar_target(
    dime_contribs,
    map_dfr(raw_dime_contribs, clean_contribs)
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
  tar_target(
    dime_metric_tables,
    create_dime_metric_tables(validation_results)
  ),
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
