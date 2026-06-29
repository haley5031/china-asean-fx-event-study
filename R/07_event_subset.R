# =============================================================================
# 07_event_subset.R
# Re-estimates the preferred FE specification (fx_return_01 ~ shock_1y |
# country, cluster = ~date) restricted to the 56 main-sample event dates with
# nonzero 1Y surprise -- i.e. identification rests only on dates with genuine
# surprise variation, instead of all dates (where non-event days carry
# shock_1y == 0).
#
# This is exploratory: results are reported here for review and are NOT yet
# wired into output/models.rds, the tables, or any figure.
#
# Input : data-clean/reg_data01_main.csv
#         output/models.rds   (for the baseline fe_1y comparison)
# Output: console comparison; output/tables/event_subset_comparison.csv
# =============================================================================

source("R/00_setup.R")

reg    <- read_csv(file.path(paths$clean, "reg_data01_main.csv"), show_col_types = FALSE)
models <- readRDS(file.path(paths$output_root, "models.rds"))

event_dates <- unique(reg$date[reg$event_day == 1 & reg$shock_1y != 0])
message("Event-subset dates with nonzero shock_1y: ", length(event_dates))

reg_event_subset <- reg %>% filter(date %in% event_dates)

fe_1y_event_subset <- feols(
  fx_return_01 ~ shock_1y | country,
  cluster = ~date, data = reg_event_subset
)

baseline_ct <- as.data.frame(coeftable(models$fe_1y))
subset_ct   <- as.data.frame(coeftable(fe_1y_event_subset))

comparison <- data.frame(
  spec     = c("Baseline FE (all dates)", "Event-subset FE (nonzero surprise only)"),
  coef     = c(baseline_ct["shock_1y", "Estimate"],   subset_ct["shock_1y", "Estimate"]),
  se       = c(baseline_ct["shock_1y", "Std. Error"], subset_ct["shock_1y", "Std. Error"]),
  pval     = c(baseline_ct["shock_1y", "Pr(>|t|)"],   subset_ct["shock_1y", "Pr(>|t|)"]),
  nobs     = c(models$fe_1y$nobs, fe_1y_event_subset$nobs),
  n_dates  = c(NA_integer_, length(event_dates))
)

cat("\n=== Baseline vs. event-subset FE (shock_1y, [0,+1]) ===\n")
print(comparison, digits = 4, row.names = FALSE)

write_csv(comparison, file.path(paths$out_tables, "event_subset_comparison.csv"))
message("Saved comparison to ", file.path(paths$out_tables, "event_subset_comparison.csv"),
        " (exploratory only -- not wired into the thesis tables/figures).")
