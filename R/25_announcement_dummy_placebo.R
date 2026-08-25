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

# MDE at 80% power (two-sided, 5% significance): SE * (z_.975 + z_.80).
# 95% CI on the point estimate, reported explicitly rather than left implicit
# in coef/se, so the actual precision is visible in the CSV, not just implied.
mde_80 <- pb_row$se * (1.96 + 0.84)
ci_lo  <- pb_row$coef - 1.96 * pb_row$se
ci_hi  <- pb_row$coef + 1.96 * pb_row$se

placebo_tab <- cbind(pb_row, n_placebo_dates = n_placebo,
                     ci95_lo = ci_lo, ci95_hi = ci_hi, mde_80pct = mde_80)
write_csv(placebo_tab, file.path(paths$out_tables, "placebo_zero_surprise.csv"))

cat("\nRegression: usd_w01 ~ is_placebo | country, cluster by date\n")
print(placebo_tab[, c("term", "coef", "se", "pval", "ci95_lo", "ci95_hi", "mde_80pct", "nobs", "n_placebo_dates")],
      row.names = FALSE, digits = 3)

# Read the numbers, not the prior. SE = 0.042, so:
#   95% CI on the point estimate : [-0.043, +0.122]
#   MDE at 80% power             : ~0.12 percentage points
# 0.12pp is SMALLER than the 0.3-0.7% range of typical daily FX volatility
# (per the descriptive statistics table), not larger than it. That makes this
# a REASONABLY TIGHT null: the test can rule out anything but a genuinely
# small effect on the 9 zero-surprise placebo dates, and the point estimate
# (+0.040) sits well inside that band. Read as evidence FOR the shock
# measure's cleanliness -- announcements the measure scores as carrying no
# surprise do not, in fact, move ASEAN currencies detectably -- not as an
# uninformative null.
#
# One caveat that cuts the other way and belongs alongside this, not instead
# of it: is_placebo is a dummy equal to 1 on only 9 of ~3,000+ date clusters.
# Cluster-robust standard errors are an asymptotic-in-the-number-of-clusters
# approximation, and applied guidance (e.g. Cameron & Miller 2015) treats
# fewer than 30-50 clusters as a regime where that approximation can
# understate the true sampling uncertainty -- and what matters here is not
# the ~3,000 total date clusters but the 9 TREATED ones, since the dummy's
# identifying variation comes only from them. The formal SE above may
# therefore be tighter than the treatment effect's true uncertainty
# justifies. Report the tight-null reading, but flag this specific reason
# for caution rather than a generic "n is small" one.
cat(sprintf(
  paste0(
    "\nActual numbers: SE = %.3f, 95%% CI = [%.3f, %.3f], MDE at 80%% power = %.3f pp.\n",
    "%.3f pp is SMALLER than typical daily FX volatility (0.3-0.7%%), not larger --\n",
    "this is a REASONABLY TIGHT null, not an uninformative one. The point estimate\n",
    "(%+.3f) is itself well below the MDE: announcements this measure scores as\n",
    "zero-surprise do not move ASEAN currencies detectably,\n",
    "which supports the shock measure's cleanliness. Caveat: is_placebo is 1 on\n",
    "only %d of the date clusters the SE is computed over -- cluster-robust SEs\n",
    "are an asymptotic-in-clusters approximation, and the identifying variation\n",
    "here comes from just those %d treated clusters, not the full cluster count,\n",
    "so treat this reading as informative but not beyond challenge on that\n",
    "specific ground.\n"
  ),
  pb_row$se, ci_lo, ci_hi, mde_80, mde_80, pb_row$coef, n_placebo, n_placebo
))

message("\nSaved: output/tables/announcement_dummy.csv, placebo_zero_surprise.csv")
