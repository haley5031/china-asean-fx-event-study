# =============================================================================
# 25_announcement_dummy_placebo.R
# Two checks on whether the PBoC announcement DAY itself carries information
# beyond the measured surprise:
#
#   A. ANNOUNCEMENT-DAY DUMMY. Adds a dummy for "this is a PBoC announcement
#      day" alongside the continuous shock in the baseline [0,+1] USD-numeraire
#      FE spec. If ASEAN returns move differently on announcement days beyond
#      what the surprise explains, the dummy should be significant even after
#      conditioning on shock size -- evidence of an omitted announcement
#      effect (event-day volatility, market attention, unmeasured news) that
#      the continuous shock measure does not capture.
#
#   B. ZERO-SURPRISE PLACEBO. There are 9 in-sample announcement dates whose
#      measured 1Y surprise is exactly zero (R/10's roll_reconciliation.csv:
#      65 exact-match in-sample event dates, 56 with a nonzero 1Y surprise --
#      65 - 56 = 9). If the shock measure is doing its job, ASEAN returns on
#      these dates should look like ordinary non-event days: the announcement
#      happened, but nothing was priced. This is checked directly, with an
#      explicit statement of whether n = 9 has any power to detect a
#      plausible effect size.
#
# Input : data-clean/reg_data_ext_main.csv   (from R/10)
# Output: output/tables/announcement_dummy.csv
#         output/tables/placebo_zero_surprise.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

# =============================================================================
# A. Announcement-day dummy alongside the continuous surprise
# =============================================================================
specs <- list(
  list(shock = "1Y", matching = "exact",  shock_var = "shock_1y",      dummy_var = "event_day"),
  list(shock = "1Y", matching = "rolled", shock_var = "shock_1y_roll", dummy_var = "event_day_roll"),
  list(shock = "5Y", matching = "exact",  shock_var = "shock_5y",      dummy_var = "event_day"),
  list(shock = "5Y", matching = "rolled", shock_var = "shock_5y_roll", dummy_var = "event_day_roll")
)

announce_tab <- bind_rows(lapply(specs, function(s) {
  f <- as.formula(sprintf("usd_w01 ~ %s + %s | country", s$shock_var, s$dummy_var))
  m <- feols(f, cluster = ~date, data = panel)
  bind_rows(
    cbind(shock = s$shock, matching = s$matching,
          coef_row(m, s$shock_var, "shock")),
    cbind(shock = s$shock, matching = s$matching,
          coef_row(m, s$dummy_var, "announcement_day_dummy"))
  )
}))

cat("\n=== A. Announcement-day dummy alongside the continuous surprise, usd_w01 ===\n\n")
print(announce_tab[, c("shock", "matching", "term", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(announce_tab, file.path(paths$out_tables, "announcement_dummy.csv"))

dummy_rows <- announce_tab[announce_tab$term == "announcement_day_dummy", ]
if (any(dummy_rows$pval < 0.05)) {
  cat("\nAt least one announcement-day dummy is significant at 5%: ASEAN returns\n",
      "behave differently on PBoC announcement days beyond what the measured\n",
      "surprise explains, in that specification. Check which row(s) before\n",
      "concluding this generally.\n", sep = "")
} else {
  cat("\nNo announcement-day dummy is significant at 5% in any specification:\n",
      "conditional on the measured surprise, PBoC announcement days do not look\n",
      "different from other days.\n", sep = "")
}

# =============================================================================
# B. Zero-surprise placebo group
# =============================================================================
placebo_dates <- panel %>%
  filter(event_day == 1, shock_1y == 0) %>%
  distinct(date) %>%
  arrange(date) %>%
  pull(date)

n_placebo <- length(placebo_dates)
cat("\n\n=== B. Zero-surprise placebo group ===\n\n")
cat("In-sample announcement dates with measured 1Y surprise exactly zero:", n_placebo, "\n")
print(data.frame(placebo_date = as.character(placebo_dates)), row.names = FALSE)

panel_pb <- panel %>%
  mutate(is_placebo = as.integer(date %in% placebo_dates))

m_pb <- feols(usd_w01 ~ is_placebo | country, cluster = ~date, data = panel_pb)
pb_row <- coef_row(m_pb, "is_placebo", "placebo (zero-surprise announcement day)")

# Simple descriptive comparison alongside the regression: mean/SD of usd_w01
# on placebo dates vs. all other dates.
desc <- panel_pb %>%
  filter(!is.na(usd_w01)) %>%
  group_by(is_placebo) %>%
  summarise(n = n(), mean_ret = mean(usd_w01), sd_ret = sd(usd_w01), .groups = "drop") %>%
  mutate(group = if_else(is_placebo == 1, "placebo (zero-surprise)", "all other days")) %>%
  select(group, n, mean_ret, sd_ret)

cat("\nDescriptive: usd_w01 on placebo dates vs. all other dates\n\n")
print(as.data.frame(desc), row.names = FALSE, digits = 3)

placebo_tab <- cbind(pb_row, n_placebo_dates = n_placebo)
write_csv(placebo_tab, file.path(paths$out_tables, "placebo_zero_surprise.csv"))

cat("\nRegression: usd_w01 ~ is_placebo | country, cluster by date\n")
print(placebo_tab[, c("term", "coef", "se", "pval", "nobs", "n_placebo_dates")],
      row.names = FALSE, digits = 3)

# Power statement: with only n_placebo event dates (5 ASEAN currencies each,
# but clustered by date so the effective sample size for inference is the
# DATE count, not the row count), the minimum detectable effect at
# conventional power is large relative to typical daily FX volatility. This
# is stated explicitly rather than left to the SE column to speak for itself.
mde_qualitative <- pb_row$se * (1.96 + 0.84)   # ~80% power, 5% two-sided test
cat(sprintf(
  paste0(
    "\nWith %d placebo dates, the clustered SE on is_placebo is %.3f. At\n",
    "conventional 80%% power / 5%% significance, the smallest true effect this\n",
    "test could reliably detect is roughly %.2f%% -- large next to the typical\n",
    "daily FX move (0.3-0.7%%, per the descriptive statistics table). Read the\n",
    "point estimate and p-value above, but do not treat a null result here as\n",
    "evidence the zero-surprise dates are uninformative: with n = %d dates this\n",
    "test has little power to detect anything but a large effect.\n"
  ),
  n_placebo, pb_row$se, mde_qualitative, n_placebo
))

message("\nSaved: output/tables/announcement_dummy.csv, placebo_zero_surprise.csv")
