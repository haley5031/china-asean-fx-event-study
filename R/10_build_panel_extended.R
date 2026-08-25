# =============================================================================
# 10_build_panel_extended.R
# Builds the extended estimation panel used for the supervisor-response
# revisions. Three additions over R/03_build_panel.R:
#
#   1. WEEKEND / HOLIDAY ROLLING.  R/03 merges shocks onto FX dates with an
#      exact-date left_join, so announcements landing on a weekend or an ASEAN
#      market holiday are dropped. This script keeps that exact-match series
#      (shock_1y, shock_5y) and adds a rolled series (shock_1y_roll,
#      shock_5y_roll) that maps each announcement forward to the next trading
#      day, so both can be estimated and compared.
#
#   2. RMB NUMERAIRE. Constructs the ASEAN-5 / renminbi cross rate as
#         LCU per CNY = (LCU per USD) / (CNY per USD)
#      and its log return, alongside the existing USD-quoted return.
#
#   3. CONTROLS. Merges the external daily series (VIX, broad dollar, 2Y
#      Treasury, Brent) for the explanatory-power checks.
#
#   4. WIDE/FORWARD WINDOWS. Adds [-5,+1] and [-5,0] outcome windows
#      (usd_w5p1/cny_w5p1, usd_w5m0/cny_w5m0) alongside [0], [0,+1], [-1,+1],
#      for the window-robustness extension in R/23.
#
# Input : data-clean/fx_returns_wide.csv
#         data-clean/policy_shocks_main.csv
#         data-clean/external_daily.csv        (from R/09_external_data.R)
# Output: data-clean/reg_data_ext.csv           (full-coverage extended panel)
#         data-clean/reg_data_ext_main.csv      (2008-2020 estimation sample)
#         output/tables/roll_reconciliation.csv (event-count audit)
#
# Nothing here overwrites the existing pipeline outputs. R/03 and R/04 remain
# the baseline; this panel is a superset.
# =============================================================================

source("R/00_setup.R")

fx_returns  <- read_csv(file.path(paths$clean, "fx_returns_wide.csv"),
                        show_col_types = FALSE)
policy_main <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"),
                        show_col_types = FALSE)
external    <- read_csv(file.path(paths$clean, "external_daily.csv"),
                        show_col_types = FALSE)

if (any(duplicated(policy_main$date)))
  stop("policy_shocks_main.csv has duplicate dates.")

# =============================================================================
# 1. Trading calendar and the roll
# =============================================================================
# The FX trading calendar is every date on which at least one ASEAN-5 rate is
# observed. Country-specific national holidays leave that country's return NA
# on a date that is otherwise in the calendar; those drop out of estimation
# naturally, exactly as they do in the baseline.
trading_days <- fx_returns %>%
  filter(if_any(all_of(ASEAN5), ~ !is.na(.x))) %>%
  pull(date) %>%
  as.Date() %>%
  sort() %>%
  unique()

roll_map <- roll_to_trading_day(policy_main$date, trading_days)
stopifnot(nrow(roll_map) == nrow(policy_main))

policy_roll <- policy_main %>%
  mutate(matched_date = roll_map$matched_date,
         roll_days    = roll_map$roll_days) %>%
  filter(!is.na(matched_date))

# Two announcements can roll onto the same trading day (e.g. a Saturday and a
# Sunday announcement both landing on the Monday). Their surprises are summed:
# the swap market prices the combined news when it reopens, so the total
# repricing is the object the FX return should be regressed on.
policy_rolled <- policy_roll %>%
  group_by(date = matched_date) %>%
  summarise(
    shock_1y_roll = sum(shock_1y, na.rm = TRUE),
    shock_5y_roll = sum(shock_5y, na.rm = TRUE),
    n_events_roll = n(),
    max_roll_days = max(roll_days, na.rm = TRUE),
    .groups = "drop"
  )

# Exact-match series: replicates the current baseline merge exactly.
policy_exact <- policy_main %>%
  filter(date %in% trading_days) %>%
  select(date, shock_1y, shock_5y)

# --- Reconciliation audit ----------------------------------------------------
recon <- data.frame(
  step = c(
    "Main event dates in cleaned policy file",
    "Exact match to an FX trading day",
    "Rolled forward to a trading day (within cap)",
    "  of which already on a trading day (roll = 0)",
    "  of which rolled 1 day",
    "  of which rolled 2+ days",
    "Unmatched even after rolling",
    "Exact-match dates inside 2008-2020",
    "Rolled dates inside 2008-2020",
    "Exact-match dates inside sample with nonzero 1Y surprise",
    "Rolled dates inside sample with nonzero 1Y surprise"
  ),
  n = c(
    n_distinct(policy_main$date),
    sum(policy_main$date %in% trading_days),
    nrow(policy_rolled),
    sum(policy_roll$roll_days == 0),
    sum(policy_roll$roll_days == 1),
    sum(policy_roll$roll_days >= 2),
    sum(is.na(roll_map$matched_date)),
    sum(policy_exact$date >= SAMPLE_START & policy_exact$date <= SAMPLE_END),
    sum(policy_rolled$date >= SAMPLE_START & policy_rolled$date <= SAMPLE_END),
    sum(policy_exact$date >= SAMPLE_START & policy_exact$date <= SAMPLE_END &
          policy_exact$shock_1y != 0),
    sum(policy_rolled$date >= SAMPLE_START & policy_rolled$date <= SAMPLE_END &
          policy_rolled$shock_1y_roll != 0)
  ),
  stringsAsFactors = FALSE
)

cat("\n=== Event-date reconciliation: exact match vs. forward roll ===\n")
print(recon, row.names = FALSE)
write_csv(recon, file.path(paths$out_tables, "roll_reconciliation.csv"))

if (any(is.na(roll_map$matched_date))) {
  message("\nAnnouncements unmatched even after rolling (beyond the ",
          MAX_ROLL_DAYS, "-day cap):")
  print(roll_map[is.na(roll_map$matched_date), c("event_date")], row.names = FALSE)
}

# =============================================================================
# 2. RMB cross rates
# =============================================================================
# LCU per CNY = (LCU per USD) / (CNY per USD).
#
# In log returns this is exactly additive:
#     r_cross = r_usd - r_cny
# so switching numeraire does not remove a contaminant -- it subtracts the
# renminbi's own response to the announcement. That identity is the object of
# the decomposition in R/11.
#
# Note the coverage cost: DEXCHUS is missing on U.S. holidays, so the cross
# rate is undefined on dates where the ASEAN market traded but the U.S. did
# not. The observation counts printed below quantify that loss.
# All external transformations are computed HERE, on the FX trading calendar,
# not on FRED's calendar in R/09. This matters: lag() must step to the previous
# ASEAN trading day in every series, or "yesterday" means a different date in
# the cross rate than in the renminbi leg and the identity r_cross = r_usd -
# r_cny fails. (It did, on the first run: 4.5e-01 rather than ~1e-15.)
ext_levels <- external %>%
  select(date, cny_per_usd, vix, broad_dollar, ust_2y, brent)

fx_wide <- fx_returns %>%
  select(date, all_of(ASEAN5)) %>%
  left_join(ext_levels, by = "date") %>%
  arrange(date)

for (cc in ASEAN5) {
  fx_wide[[paste0("cross_", cc)]] <- fx_wide[[cc]] / fx_wide$cny_per_usd
}

fx_wide <- fx_wide %>%
  mutate(across(all_of(ASEAN5),
                ~ 100 * (log(.x) - log(lag(.x))),
                .names = "retusd_{.col}")) %>%
  mutate(across(all_of(paste0("cross_", ASEAN5)),
                ~ 100 * (log(.x) - log(lag(.x))),
                .names = "ret{.col}")) %>%
  mutate(
    ret_cny          = 100 * (log(cny_per_usd)  - log(lag(cny_per_usd))),
    ret_broad_dollar = 100 * (log(broad_dollar) - log(lag(broad_dollar))),
    ret_brent        = 100 * (log(brent)        - log(lag(brent))),
    d_vix            = vix    - lag(vix),
    d_ust_2y         = 100 * (ust_2y - lag(ust_2y))   # pct pts -> bps
  )
# -> retusd_idr ... and retcross_idr ...

daily_ctrl <- fx_wide %>%
  select(date, ret_cny, d_vix, ret_broad_dollar, ret_brent, d_ust_2y)

# =============================================================================
# 3. Long panel
# =============================================================================
long_usd <- fx_wide %>%
  select(date, starts_with("retusd_")) %>%
  pivot_longer(-date, names_to = "country", values_to = "fx_return_usd") %>%
  mutate(country = sub("^retusd_", "", country))

long_cny <- fx_wide %>%
  select(date, starts_with("retcross_")) %>%
  pivot_longer(-date, names_to = "country", values_to = "fx_return_cny") %>%
  mutate(country = sub("^retcross_", "", country))

long_lvl <- fx_wide %>%
  select(date, all_of(ASEAN5)) %>%
  pivot_longer(-date, names_to = "country", values_to = "fx_lcu_per_usd")

panel <- long_lvl %>%
  left_join(long_usd, by = c("date", "country")) %>%
  left_join(long_cny, by = c("date", "country")) %>%
  arrange(country, date)

stopifnot(nrow(panel) == nrow(fx_wide) * length(ASEAN5))

# =============================================================================
# 4. Merge shocks (both matching rules) and controls
# =============================================================================
panel <- panel %>%
  left_join(policy_exact,  by = "date") %>%
  left_join(policy_rolled, by = "date") %>%
  left_join(daily_ctrl, by = "date") %>%
  mutate(
    event_day      = if_else(is.na(shock_1y),      0L, 1L),
    event_day_roll = if_else(is.na(shock_1y_roll), 0L, 1L),
    shock_1y       = coalesce(shock_1y,      0),
    shock_5y       = coalesce(shock_5y,      0),
    shock_1y_roll  = coalesce(shock_1y_roll, 0),
    shock_5y_roll  = coalesce(shock_5y_roll, 0),
    n_events_roll  = coalesce(n_events_roll, 0L),
    post_split     = if_else(date >= SPLIT_DATE, 1L, 0L)
  )

# =============================================================================
# 5. Event windows, for both numeraires
# =============================================================================
# [0]      r_t
# [0,+1]   r_t + r_{t+1}
# [-1,+1]  r_{t-1} + r_t + r_{t+1}
# [-5,+1]  r_{t-5} + ... + r_t + r_{t+1}   (wide/forward window robustness)
# [-5,0]   r_{t-5} + ... + r_t
panel <- panel %>%
  group_by(country) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    usd_w0   = fx_return_usd,
    usd_w01  = fx_return_usd + lead(fx_return_usd, 1),
    usd_wm11 = lag(fx_return_usd, 1) + fx_return_usd + lead(fx_return_usd, 1),
    usd_w5p1 = lag(fx_return_usd, 5) + lag(fx_return_usd, 4) +
               lag(fx_return_usd, 3) + lag(fx_return_usd, 2) +
               lag(fx_return_usd, 1) + fx_return_usd + lead(fx_return_usd, 1),
    usd_w5m0 = lag(fx_return_usd, 5) + lag(fx_return_usd, 4) +
               lag(fx_return_usd, 3) + lag(fx_return_usd, 2) +
               lag(fx_return_usd, 1) + fx_return_usd,
    cny_w0   = fx_return_cny,
    cny_w01  = fx_return_cny + lead(fx_return_cny, 1),
    cny_wm11 = lag(fx_return_cny, 1) + fx_return_cny + lead(fx_return_cny, 1),
    cny_w5p1 = lag(fx_return_cny, 5) + lag(fx_return_cny, 4) +
               lag(fx_return_cny, 3) + lag(fx_return_cny, 2) +
               lag(fx_return_cny, 1) + fx_return_cny + lead(fx_return_cny, 1),
    cny_w5m0 = lag(fx_return_cny, 5) + lag(fx_return_cny, 4) +
               lag(fx_return_cny, 3) + lag(fx_return_cny, 2) +
               lag(fx_return_cny, 1) + fx_return_cny,
    # Control returns aggregated over the same window as the outcome, so a
    # [0,+1] regression controls for [0,+1] movement in the control.
    ctrl_dollar_w01 = ret_broad_dollar + lead(ret_broad_dollar, 1),
    ctrl_vix_w01    = d_vix            + lead(d_vix, 1),
    ctrl_brent_w01  = ret_brent        + lead(ret_brent, 1),
    ctrl_ust2y_w01  = d_ust_2y         + lead(d_ust_2y, 1),
    ctrl_cny_w01    = ret_cny          + lead(ret_cny, 1)
  ) %>%
  ungroup()

write_csv(panel, file.path(paths$clean, "reg_data_ext.csv"))

panel_main <- panel %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END)

write_csv(panel_main, file.path(paths$clean, "reg_data_ext_main.csv"))

# =============================================================================
# 6. Cross-checks
# =============================================================================
# (a) The extended USD [0,+1] outcome must reproduce the baseline panel's
#     fx_return_01 exactly. If this fails, the two panels have diverged and
#     nothing downstream is comparable.
base_path <- file.path(paths$clean, "reg_data01_main.csv")
if (file.exists(base_path)) {
  base <- read_csv(base_path, show_col_types = FALSE) %>%
    select(date, country, fx_return_01)
  cmp <- panel_main %>%
    select(date, country, usd_w01) %>%
    inner_join(base, by = c("date", "country")) %>%
    filter(!is.na(usd_w01), !is.na(fx_return_01))
  d <- max(abs(cmp$usd_w01 - cmp$fx_return_01))
  message(sprintf("\nCheck (a) max|usd_w01 - baseline fx_return_01| = %.2e  (n = %d)",
                  d, nrow(cmp)))
  if (d > 1e-8) warning("Extended panel does not reproduce the baseline [0,+1] outcome.")
} else {
  message("\nCheck (a) skipped: run R/03_build_panel.R first to compare.")
}

# (b) The numeraire identity r_cross = r_usd - r_cny must hold to machine
#     precision. This is the arithmetic the decomposition in R/11 relies on.
idcheck <- panel_main %>%
  filter(!is.na(fx_return_cny), !is.na(fx_return_usd), !is.na(ret_cny))
d2 <- max(abs(idcheck$fx_return_cny - (idcheck$fx_return_usd - idcheck$ret_cny)))
message(sprintf("Check (b) max|r_cross - (r_usd - r_cny)| = %.2e  (n = %d)",
                d2, nrow(idcheck)))
if (d2 > 1e-8) stop("Numeraire identity fails -- check the cross-rate construction.")

# (b2) Roll-distance distribution. PBoC announcements frequently fall on
#      weekends, so a 1-day roll is a Sunday announcement and a 2-day roll a
#      Saturday one. Anything beyond 2 would indicate a holiday cluster and
#      deserves a look before the rolled series is used.
if (nrow(policy_roll)) {
  cat("\n=== Roll-distance distribution ===\n")
  print(table(`calendar days rolled` = policy_roll$roll_days))
}

# (c) Coverage cost of the RMB numeraire.
cov_tab <- panel_main %>%
  group_by(country) %>%
  summarise(
    n_usd_w01 = sum(!is.na(usd_w01)),
    n_cny_w01 = sum(!is.na(cny_w01)),
    lost      = n_usd_w01 - n_cny_w01,
    .groups   = "drop"
  )
cat("\n=== [0,+1] observations by numeraire (2008-2020) ===\n")
print(as.data.frame(cov_tab), row.names = FALSE)
write_csv(cov_tab, file.path(paths$out_tables, "numeraire_coverage.csv"))

message("\nSaved extended panel: ", nrow(panel_main), " rows (main sample).")
