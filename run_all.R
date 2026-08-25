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

# R/21-26: further supervisor-response extensions, all built on R/10's
# extended panel. R/21 additionally needs the cached AFE dollar index
# (data-raw/external/DTWEXAFEGS.csv), fetched by R/14 -- same dependency as
# R/13/R/15/R/19 -- so it runs after R/14. R/22, R/24-26 need only R/10's
# panel (R/23-24 also need the [-5,+1]/[-5,0] window columns R/10 now
# builds); R/23 additionally reads output/tables/regime_windows.csv, written
# by R/13, so it must run after R/13 too (already true numerically). R/26
# needs only R/10's panel.
message("== 21: AFE-basket numeraire ==");                        source("R/21_afe_basket_numeraire.R")
message("== 22: China exchange-rate regime split ==");            source("R/22_china_regime_split.R")
message("== 23: wide/forward window robustness ==");              source("R/23_window_wide.R")
message("== 24: forward local projections ==");                   source("R/24_forward_local_projections.R")
message("== fig_lp: LP coefficient-path plot ==");                source("R/fig_lp_path.R")
message("== 25: announcement-day dummy + zero-surprise placebo =="); source("R/25_announcement_dummy_placebo.R")
message("== 26: full leave-one-out on the regime interaction ==="); source("R/26_regime_interaction_full_loo.R")
message("== 27: instrument-type Table 11, excl. 26 Nov 2008 ==="); source("R/27_instrument_type_excl_nov2008.R")

message("== DONE. See data-clean/ and output/. ==")

message("== DONE. See data-clean/ and output/. ==")