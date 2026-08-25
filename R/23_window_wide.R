# =============================================================================
# 23_window_wide.R
# A colleague of the supervisor's: the [0,+1] window may be too narrow because
# FX markets anticipate policy moves. Adds two wider/forward-looking windows,
# [-5,+1] and [-5,0] (built in R/10, alongside [0]/[0,+1]/[-1,+1]), to the
# existing window-robustness machinery: R/06's plain FE ladder ("baseline"
# below) and R/13's shock x post_split regime grid ("regime" below), each
# extended -- not replaced -- to cover the two new windows.
#
# Grid: numeraire (USD, RMB cross) x shock (1Y, 5Y) x matching rule (exact
# date, forward-rolled) x window ([-5,+1], [-5,0]).
#
# Input : data-clean/reg_data_ext_main.csv   (from R/10)
# Output: output/tables/window_wide_baseline.csv
#         output/tables/window_wide_regime.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

WINDOWS_WIDE <- c("[-5,+1]" = "w5p1", "[-5,0]" = "w5m0")
SHOCKS       <- c("1Y" = "shock_1y", "1Y (rolled)" = "shock_1y_roll",
                  "5Y" = "shock_5y", "5Y (rolled)" = "shock_5y_roll")
NUMERAIRES   <- c("USD" = "usd", "RMB cross" = "cny")

grid <- expand.grid(numer = names(NUMERAIRES), shock = names(SHOCKS),
                    window = names(WINDOWS_WIDE), stringsAsFactors = FALSE)

# =============================================================================
# A. Baseline: plain FE ladder (no regime interaction), extending R/06's
#    window ladder to [-5,+1] and [-5,0]
# =============================================================================
baseline <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  numer <- NUMERAIRES[[g$numer]]
  shock <- SHOCKS[[g$shock]]
  y     <- paste0(numer, "_", WINDOWS_WIDE[[g$window]])
  m <- feols(as.formula(sprintf("%s ~ %s | country", y, shock)),
             cluster = ~date, data = panel)
  cbind(numeraire = g$numer, window = g$window, shock = g$shock,
        coef_row(m, shock, shock))
}))

cat("\n=== A. Baseline FE ladder, wide/forward windows ===\n\n")
print(baseline[, c("numeraire", "window", "shock", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(baseline, file.path(paths$out_tables, "window_wide_baseline.csv"))

# =============================================================================
# B. Regime: shock x post_split interaction, extending R/13 check 1's grid
# =============================================================================
regime <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  numer <- NUMERAIRES[[g$numer]]
  shock <- SHOCKS[[g$shock]]
  y     <- paste0(numer, "_", WINDOWS_WIDE[[g$window]])
  m <- feols(as.formula(sprintf("%s ~ %s * post_split | country", y, shock)),
             cluster = ~date, data = panel)
  int_term <- paste0(shock, ":post_split")
  cbind(numeraire = g$numer, window = g$window, shock = g$shock,
        coef_row(m, int_term, int_term))
}))

cat("\n\n=== B. Regime interaction (shock x post_split), wide/forward windows ===\n\n")
print(regime[, c("numeraire", "window", "shock", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(regime, file.path(paths$out_tables, "window_wide_regime.csv"))

cat("\nRead: if the post-2015 break in A/B's [0,+1] counterpart (R/12, R/13)\n",
    "also shows up at [-5,+1] and [-5,0], it is not an artefact of the narrow\n",
    "[0,+1] window; if it only appears in [0,+1] and washes out once five days\n",
    "of pre-announcement drift are folded in, that is itself informative about\n",
    "how anticipated the surprise component of these announcements is.\n", sep = "")

message("\nSaved: output/tables/window_wide_baseline.csv, window_wide_regime.csv")
