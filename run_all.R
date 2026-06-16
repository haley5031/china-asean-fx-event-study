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
message("== DONE. See data-clean/ and output/. ==")
message("== 06: window robustness ==");     source("R/06_window_robustness.R")