# =============================================================================
# run_all.R
# Rebuilds the entire thesis from raw data to final tables and figures.
# Run from the project root (open via the .Rproj file first):
#   source("run_all.R")
# =============================================================================

message("== 01: load policy shocks ==");   source("R/01_load_policy.R")
message("== 02: clean FX ==");              source("R/02_clean_fx.R")
message("== 03: build panel ==");           source("R/03_build_panel.R")
message("== 04: estimate models ==");       source("R/04_estimate.R")
message("== 05: tables and figures ==");    source("R/05_tables_figures.R")
message("== 06: window robustness ==");     source("R/06_window_robustness.R")

message("== fig1: FX indexed series ==");          source("R/fig_fx_series.R")
message("== fig2: MP surprise stem plot ==");       source("R/fig_shock_stem.R")
message("== fig3: cumulative FX response ==");      source("R/fig_cumulative_response.R")
message("== fig: attrition funnel ==");             source("R/fig_attrition_funnel.R")
message("== fig3a: forest plot, country ==");       source("R/fig_forest_country.R")
message("== fig3b: forest plot, heterogeneity ==");  source("R/fig_forest_heterogeneity.R")

message("== 07: event-subset re-estimation (exploratory) =="); source("R/07_event_subset.R")
message("== 08: descriptive statistics ==");        source("R/08_descriptive_stats.R")

# R/09-20 build and use the extended panel (R/10), which itself needs the
# external FRED series (R/09) and R/01-02's outputs. Order below follows what
# each script actually reads and writes (per its own header), not its number:
#   R/09  -> data-clean/external_daily.csv
#   R/10  -> data-clean/reg_data_ext_main.csv, needed by everything from here on
#   R/11-12, R/16-18 only need R/10's panel (plus R/16-17 also read
#     data-clean/policy_shocks_main.csv from R/01); no dependencies among
#     themselves, so numeric order is used
#   R/14  -> also fetches/caches data-raw/external/DTWEXAFEGS.csv
#   R/13, R/15, R/19 require that cached file (R/13 since it added an AFE-
#     dollar control column), so R/14 must precede all three -- their own
#     headers say so explicitly ("fetched by R/14")
#   R/20  only needs output/models.rds, already produced by R/04
message("== 09: external FRED series ==");          source("R/09_external_data.R")
message("== 10: build extended panel ==");           source("R/10_build_panel_extended.R")

message("== 11: numeraire decomposition ==");                     source("R/11_numeraire.R")
message("== 12: sample split (H3) ==");                           source("R/12_sample_split.R")
message("== 14: dollar control fix ==");                          source("R/14_dollar_control_fix.R")
message("== 13: regime stress tests ==");                         source("R/13_regime_stress_tests.R")
message("== 15: mediator or confounder ==");                      source("R/15_mediator_or_confounder.R")
message("== 16: instrument mix ==");                              source("R/16_instrument_mix.R")
message("== 17: instrument tests ==");                            source("R/17_instrument_tests.R")
message("== 18: FOMC-adjacent + drop-2020 regime robustness =="); source("R/18_fomc_regime_robustness.R")
message("== 19: mediation cluster bootstrap ==");                 source("R/19_mediation_bootstrap.R")
message("== 20: heterogeneity joint F-test (H2) ==");             source("R/20_heterogeneity_ftest.R")

message("== DONE. See data-clean/ and output/. ==")

message("== DONE. See data-clean/ and output/. ==")