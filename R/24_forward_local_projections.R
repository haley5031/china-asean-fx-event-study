# =============================================================================
# 24_forward_local_projections.R
# A colleague of the supervisor's: the event window may be too narrow because
# FX markets anticipate policy moves. This script estimates the cumulative FX
# response at [0,+h] for h = 0, 1, ..., 10 trading days (a Jorda-style local
# projection), one regression per horizon, country FE, SEs clustered by date.
# Companion to R/23 (which adds two more fixed windows, [-5,+1]/[-5,0], to the
# existing ladder); this script traces out the FULL horizon path instead.
#
# cum_h = r_t + r_{t+1} + ... + r_{t+h}    (USD numeraire; h = 0 is usd_w0,
#                                            h = 1 is usd_w01)
#
# Built directly from fx_return_usd (not from R/10's pre-built window columns,
# which only go out to h = 1) using the same lead-and-sum convention.
#
# Input : data-clean/reg_data_ext_main.csv   (from R/10)
# Output: output/tables/lp_horizon_response.csv
#         output/figures/fig_lp_coefficient_path.{pdf,png}   (via R/fig_lp_path.R)
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

H_MAX  <- 10L
SHOCKS <- c("1Y" = "shock_1y", "5Y" = "shock_5y")

# --- Build cum_0 ... cum_10, within country, date-ordered ---------------------
for (h in 0:H_MAX) {
  panel <- panel %>%
    group_by(country) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(!!paste0("cum_h", h) := Reduce(`+`, lapply(0:h, function(k) lead(fx_return_usd, k)))) %>%
    ungroup()
}

# --- Sanity check: h = 0 and h = 1 must reproduce R/10's usd_w0 / usd_w01 ----
chk0 <- panel %>% filter(!is.na(cum_h0), !is.na(usd_w0))
chk1 <- panel %>% filter(!is.na(cum_h1), !is.na(usd_w01))
d0 <- max(abs(chk0$cum_h0 - chk0$usd_w0))
d1 <- max(abs(chk1$cum_h1 - chk1$usd_w01))
message(sprintf("Check: max|cum_h0 - usd_w0| = %.2e, max|cum_h1 - usd_w01| = %.2e", d0, d1))
if (d0 > 1e-8 || d1 > 1e-8)
  stop("LP cumulative-return construction does not reproduce R/10's fixed windows.")

# =============================================================================
# LP regressions: one per horizon x shock measure
# =============================================================================
lp <- bind_rows(lapply(names(SHOCKS), function(slab) {
  s <- SHOCKS[[slab]]
  bind_rows(lapply(0:H_MAX, function(h) {
    y <- paste0("cum_h", h)
    d <- panel %>% filter(!is.na(.data[[y]]))
    m <- feols(as.formula(sprintf("%s ~ %s | country", y, s)),
               cluster = ~date, data = d)
    ct <- as.data.frame(coeftable(m))
    est <- ct[s, "Estimate"]
    se  <- ct[s, "Std. Error"]
    data.frame(
      shock = slab, h = h,
      coef  = est, se = se,
      pval  = ct[s, grep("^Pr", names(ct))[1]],
      ci90_lo = est - 1.645 * se, ci90_hi = est + 1.645 * se,
      ci95_lo = est - 1.960 * se, ci95_hi = est + 1.960 * se,
      nobs  = nobs(m),
      row.names = NULL
    )
  }))
}))

cat("\n=== Forward local projections: cumulative [0,+h] response, h = 0..", H_MAX, " ===\n\n", sep = "")
cat("    USD numeraire, country FE, SEs clustered by date.\n\n")
print(lp, row.names = FALSE, digits = 3)
write_csv(lp, file.path(paths$out_tables, "lp_horizon_response.csv"))

cat("\nRead: a coefficient path that keeps growing (in the same direction as the\n",
    "[0,+1] estimate) out to h = 10 without reverting says the [0,+1] window\n",
    "understates the full response; a path that peaks early and flattens or\n",
    "reverts says [0,+1] already captures most of it. Widening SEs at longer\n",
    "horizons are expected -- overlapping windows across events and more\n",
    "trading days of unrelated news accumulate noise -- so read the band, not\n",
    "just the point estimate, at h > 3-4.\n", sep = "")

message("\nSaved: output/tables/lp_horizon_response.csv")
