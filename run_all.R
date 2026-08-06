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

# R/09-17 (extended panel, sample-split, mediation, instrument checks) are
# supervisor-response revisions kept as standalone scripts, run manually --
# see each script's header. R/18-20 below need the extended panel (R/09-10)
# and, for R/19, the cached AFE dollar file R/14 downloads, so those two are
# sourced here as prerequisites without pulling in the rest of that chain.
message("== 09: external FRED series ==");          source("R/09_external_data.R")
message("== 10: build extended panel ==");           source("R/10_build_panel_extended.R")

message("== 18: FOMC-adjacent + drop-2020 regime robustness =="); source("R/18_fomc_regime_robustness.R")
message("== 19: mediation cluster bootstrap ==");                  source("R/19_mediation_bootstrap.R")
message("== 20: heterogeneity joint F-test (H2) ==");              source("R/20_heterogeneity_ftest.R")

message("== DONE. See data-clean/ and output/. ==")

message("== DONE. See data-clean/ and output/. ==")