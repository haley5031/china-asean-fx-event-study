# =============================================================================
# 11_numeraire.R
# Supervisor response, point 1: "It would be more logic to use the RMB - Asean
# currency itself ... as it will give a cleaner effect."
#
# This script does four things.
#
#   A. ROLL ROBUSTNESS. Re-estimates the preferred specification with the
#      rolled shock series, so the weekend/holiday treatment is shown not to
#      drive the result.
#
#   B. NUMERAIRE DECOMPOSITION. The ASEAN/RMB cross rate is constructed, not
#      traded: LCU/CNY = (LCU/USD) / (CNY/USD), so in log returns
#
#            r_cross  =  r_usd  -  r_cny        (exactly)
#
#      and therefore, estimated on a common sample with the same fixed effects
#      and the same regressor,
#
#            beta_cross  =  beta_usd  -  beta_cny.
#
#      Switching numeraire does not purge a contaminant; it subtracts the
#      renminbi's own response to the PBoC surprise. Estimating all three
#      coefficients shows how much of any cross-rate result is the renminbi
#      moving rather than ASEAN currencies responding. That is the empirical
#      case for retaining the dollar as the headline numeraire.
#
#   C. RMB LEG AS A CONTROL. The same operation with a freely estimated rather
#      than imposed unit coefficient: USD outcome, CNY return as a regressor.
#
#   D. US MONETARY SIGNAL. Directly addresses the stated concern that the Fed
#      and the PBoC may move together, by controlling for the contemporaneous
#      change in the 2-year Treasury yield and the broad dollar index.
#
# Input : data-clean/reg_data_ext_main.csv   (from R/10)
# Output: output/tables/numeraire_*.csv, output/models_numeraire.rds
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

WINDOWS <- c("[0]" = "w0", "[0,+1]" = "w01", "[-1,+1]" = "wm11")

# =============================================================================
# A. Roll robustness: exact-date matching vs forward roll
# =============================================================================
roll_tab <- bind_rows(lapply(names(WINDOWS), function(wlab) {
  w <- WINDOWS[[wlab]]
  y <- paste0("usd_", w)
  bind_rows(
    coef_row(
      feols(as.formula(sprintf("%s ~ shock_1y | country", y)),
            cluster = ~date, data = panel),
      "shock_1y", "1Y", extra = list(window = wlab, matching = "exact date")
    ),
    coef_row(
      feols(as.formula(sprintf("%s ~ shock_1y_roll | country", y)),
            cluster = ~date, data = panel),
      "shock_1y_roll", "1Y", extra = list(window = wlab, matching = "rolled")
    ),
    coef_row(
      feols(as.formula(sprintf("%s ~ shock_5y | country", y)),
            cluster = ~date, data = panel),
      "shock_5y", "5Y", extra = list(window = wlab, matching = "exact date")
    ),
    coef_row(
      feols(as.formula(sprintf("%s ~ shock_5y_roll | country", y)),
            cluster = ~date, data = panel),
      "shock_5y_roll", "5Y", extra = list(window = wlab, matching = "rolled")
    )
  )
}))

cat("\n=== A. Weekend/holiday treatment: exact-date match vs forward roll ===\n")
cat("    (USD numeraire, country FE, SEs clustered by date)\n\n")
print(roll_tab[, c("window", "matching", "term", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(roll_tab, file.path(paths$out_tables, "numeraire_roll_robustness.csv"))

# Which shock series to carry into the decomposition. Set to "shock_1y_roll"
# if the roll is promoted to baseline; "shock_1y" reproduces the current draft.
SHOCK <- "shock_1y"

# =============================================================================
# B. Numeraire decomposition on a common sample
# =============================================================================
# The three regressions must share one estimation sample for the identity to
# hold exactly. The binding constraint is the cross rate, which is undefined
# whenever CNY/USD is missing (U.S. holidays), so the common sample is defined
# by the cross-rate outcome.
decomp <- bind_rows(lapply(names(WINDOWS), function(wlab) {

  w      <- WINDOWS[[wlab]]
  y_usd  <- paste0("usd_", w)
  y_cny  <- paste0("cny_", w)

  # The renminbi leg over the same window, constant across countries.
  # By construction  y_usd - y_cny  is exactly the CNY/USD window return.
  d <- panel %>%
    mutate(rmb_leg = .data[[y_usd]] - .data[[y_cny]]) %>%
    filter(!is.na(.data[[y_usd]]), !is.na(.data[[y_cny]]))

  f <- function(yv) feols(as.formula(sprintf("%s ~ %s | country", yv, SHOCK)),
                          cluster = ~date, data = d)

  m_usd <- f(y_usd)
  m_cny <- f(y_cny)
  m_rmb <- f("rmb_leg")

  bind_rows(
    coef_row(m_usd, SHOCK, "beta_USD   (LCU per USD)",
             extra = list(window = wlab)),
    coef_row(m_rmb, SHOCK, "beta_CNY   (CNY per USD, RMB's own response)",
             extra = list(window = wlab)),
    coef_row(m_cny, SHOCK, "beta_cross (LCU per CNY)",
             extra = list(window = wlab))
  )
}))

cat("\n\n=== B. Numeraire decomposition:  beta_cross = beta_USD - beta_CNY ===\n")
cat("    Common sample per window; country FE; SEs clustered by date.\n\n")
print(decomp[, c("window", "term", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)

# Verify the identity numerically, window by window.
cat("\n--- identity check ---\n")
for (wlab in names(WINDOWS)) {
  b <- decomp[decomp$window == wlab, ]
  lhs <- b$coef[3]                 # beta_cross
  rhs <- b$coef[1] - b$coef[2]     # beta_USD - beta_CNY
  cat(sprintf("  %-8s beta_cross = %+8.4f   beta_USD - beta_CNY = %+8.4f   diff = %.2e\n",
              wlab, lhs, rhs, abs(lhs - rhs)))
}

# Share of the cross-rate coefficient attributable to the renminbi itself.
cat("\n--- share of beta_cross accounted for by the renminbi's own move ---\n")
for (wlab in names(WINDOWS)) {
  b <- decomp[decomp$window == wlab, ]
  if (abs(b$coef[3]) > 1e-10) {
    cat(sprintf("  %-8s  -beta_CNY / beta_cross = %6.1f%%\n",
                wlab, 100 * (-b$coef[2]) / b$coef[3]))
  }
}
write_csv(decomp, file.path(paths$out_tables, "numeraire_decomposition.csv"))

# --- Renminbi response as a clean time-series regression ---------------------
# The panel version above weights each date by the number of countries observed.
# This is the same object estimated once per date, which is how it should be
# read substantively: does the renminbi itself move on PBoC surprise days?
rmb_ts <- panel %>%
  mutate(rmb_leg_w01 = usd_w01 - cny_w01) %>%
  filter(!is.na(rmb_leg_w01)) %>%
  distinct(date, .keep_all = TRUE) %>%
  select(date, rmb_leg_w01, all_of(SHOCK))

m_rmb_ts <- lm(as.formula(sprintf("rmb_leg_w01 ~ %s", SHOCK)), data = rmb_ts)
cat("\n\n--- Renminbi's own [0,+1] response to the surprise (date-level OLS) ---\n")
print(summary(m_rmb_ts)$coefficients, digits = 4)

# =============================================================================
# C. RMB leg as an estimated control, USD outcome retained
# =============================================================================
fml <- function(rhs) as.formula(sprintf("usd_w01 ~ %s | country",
                                        paste(c(SHOCK, rhs), collapse = " + ")))

m_c1 <- feols(fml(NULL),           cluster = ~date, data = panel)
m_c2 <- feols(fml("ctrl_cny_w01"), cluster = ~date, data = panel)

cat("\n\n=== C. USD outcome, renminbi leg as an estimated control ===\n")
print(etable(list("USD baseline" = m_c1, "+ CNY control" = m_c2),
       cluster = ~date, digits = 3))

# =============================================================================
# D. US monetary signal and dollar-leg controls
# =============================================================================
# Addresses the concern that the Fed and the PBoC may move at the same time.
# d_ust_2y is the daily change in the 2-year Treasury yield in basis points: a
# direct, calendar-free proxy for contemporaneous U.S. monetary news that needs
# no FOMC date list. ret_broad_dollar absorbs common dollar movement.
m_d1 <- feols(fml(NULL),             cluster = ~date, data = panel)
m_d2 <- feols(fml("ctrl_ust2y_w01"), cluster = ~date, data = panel)
m_d3 <- feols(fml(c("ctrl_ust2y_w01", "ctrl_dollar_w01")),
              cluster = ~date, data = panel)
m_d4 <- feols(fml(c("ctrl_ust2y_w01", "ctrl_dollar_w01",
                    "ctrl_vix_w01", "ctrl_brent_w01")),
              cluster = ~date, data = panel)

cat("\n\n=== D. Controlling for the U.S. signal and global factors ===\n")
print(etable(list("(1) baseline"   = m_d1,
            "(2) + UST 2Y"   = m_d2,
            "(3) + dollar"   = m_d3,
            "(4) + VIX, oil" = m_d4),
       cluster = ~date, digits = 3, fitstat = c("n", "r2", "wr2")))

cat("\nNote: R2 above is the within-R2 for the FE models. The jump between\n",
    "column (1) and column (4) is the answer to the explanatory-power query:\n",
    "daily FX returns are near-unpredictable from the shock alone, but common\n",
    "global factors do explain a material share of the variance.\n")

# =============================================================================
# Save
# =============================================================================
saveRDS(
  list(roll = roll_tab, decomp = decomp, rmb_ts = m_rmb_ts,
       cny_control = list(m_c1, m_c2),
       us_controls = list(m_d1, m_d2, m_d3, m_d4)),
  file.path(paths$output_root, "models_numeraire.rds")
)

message("\nSaved: output/tables/numeraire_*.csv and output/models_numeraire.rds")
