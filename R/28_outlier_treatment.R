# =============================================================================
# 28_outlier_treatment.R
# Leave-one-out (R/13, R/22, R/26) answers "does one date carry this."  This
# script instead asks for an estimate that does not depend on the answer, by
# re-estimating three specifications under two general outlier treatments,
# reported alongside the unadjusted estimate:
#
#   WINSORIZED. The shock variable is capped/floored at the 1st/99th
#   percentile of its OWN nonzero-surprise distribution (zero-surprise dates
#   are untouched). Estimated with the usual feols/cluster-by-date machinery,
#   since winsorizing is a data transformation, not an estimator change.
#
#   ROBUST REGRESSION (MM-estimator, robustbase::lmrob). Downweights
#   influential observations directly in the estimator rather than by
#   deletion. lmrob has no built-in cluster-robust SE, so inference here uses
#   a CLUSTER BOOTSTRAP BY EVENT DATE (same technique as R/19's mediation
#   bootstrap, B = 500 rather than R/19's 2,000 -- reduced for tractability
#   given the number of specifications below, not for a different reason):
#   each replication resamples the distinct dates with replacement, refits
#   lmrob, and the empirical SD of the bootstrap coefficients is reported as
#   the SE (with a normal-approximation p-value, consistent with how every
#   other table in this project reports p-values).
#
# THREE SPECIFICATIONS, each under {unadjusted, winsorized, robust}:
#   A. Baseline average response: usd_w01 ~ shock | country (1Y/5Y, exact/rolled)
#   B. Regime interaction: usd_w01 ~ shock * post_split | country (same 4)
#   C. Instrument-type split (R/16/R/17's construction, shock_1y only, as in
#      the original): price/quantity pre-split level and post-minus-pre
#      difference.
#
# Input : data-clean/reg_data_ext_main.csv, data-clean/policy_shocks_main.csv
# Output: output/tables/outlier_baseline.csv
#         output/tables/outlier_regime_interaction.csv
#         output/tables/outlier_instrument_type.csv
# =============================================================================

source("R/00_setup.R")
library(robustbase)

set.seed(20150811)  # same convention as R/19: the regime split date, not tuned
# R/19's mediation bootstrap uses B = 2000 for a single specification. Each
# lmrob() fit here takes ~2 seconds on a no-interaction formula (needs
# lmrob.control(setting = "KS2014") -- without it, lmrob's S-estimator shows
# local breakdown on the country dummies, verified directly) and ~7 seconds
# on an interaction formula when the fast large-n path fails (~40% of
# resamples, verified directly: "Fast S large n strategy failed", fixed with
# fast.s.large.n = Inf, tried only as a fallback -- see lmrob_safe() below).
# This script fits many more specifications than R/19 does at a slower
# per-fit cost, so B is reduced to 100 here for tractability. Documented as
# a real reduction, not a silent one.
B <- 100

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

policy <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"),
                   show_col_types = FALSE) %>%
  select(date, shock_1y, isdRRR, isdRevrepo, isdLDR, isdMLF)

SHOCKS <- list(
  list(label = "1Y", matching = "exact",  var = "shock_1y"),
  list(label = "1Y", matching = "rolled", var = "shock_1y_roll"),
  list(label = "5Y", matching = "exact",  var = "shock_5y"),
  list(label = "5Y", matching = "rolled", var = "shock_5y_roll")
)

# --- Winsorize a shock variable at the 1st/99th pct of its OWN nonzero
#     EVENT-DATE distribution. The panel repeats each date's shock value once
#     per country (~5x), so computing quantiles directly on the panel column
#     over-weights whichever event happens to have full 5-country coverage and,
#     with as few as 56 distinct nonzero events, can make the empirical 1st/
#     99th percentile land exactly ON the most extreme event itself (verified:
#     it did, on the first attempt -- bounds came back as exactly min/max, so
#     "winsorizing" changed nothing). Quantiles are computed on the DISTINCT
#     per-date series (one row per event), then applied to the full panel column.
winsorize_shock <- function(data, shockvar) {
  events <- data %>% distinct(date, .keep_all = TRUE) %>% pull(.data[[shockvar]])
  nz <- events[events != 0]
  bounds <- quantile(nz, c(0.01, 0.99), na.rm = TRUE, type = 7)
  pmin(pmax(data[[shockvar]], bounds[1]), bounds[2])
}

lmrob_ctrl      <- lmrob.control(setting = "KS2014")
# The interaction specifications (shock * post_split) occasionally trip
# lmrob's large-n fast-S partitioning strategy ("Fast S large n strategy
# failed" -- verified directly: happens on ~40% of cluster-bootstrap
# resamples of this specific formula). Retry with fast.s.large.n = Inf
# (robustbase's own documented fix) only when the fast path fails, so the
# common case stays fast and only the resamples that need it pay the cost.
lmrob_ctrl_slow <- lmrob.control(setting = "KS2014", fast.s.large.n = Inf)
lmrob_safe <- function(fml, data) {
  m <- tryCatch(lmrob(fml, data = data, control = lmrob_ctrl), error = function(e) NULL)
  if (is.null(m)) m <- tryCatch(lmrob(fml, data = data, control = lmrob_ctrl_slow), error = function(e) NULL)
  m
}

# --- Cluster bootstrap by date for an lmrob fit; returns coef + boot SE + p
#     for one or more TERMS at once, sharing the same B resampled fits across
#     all requested terms (so a formula with two terms of interest costs one
#     bootstrap run, not two).
robust_boot_multi <- function(fml, data, terms, B = 200) {
  m0 <- lmrob_safe(fml, data)
  if (is.null(m0)) return(NULL)
  point <- coef(m0)[terms]

  d_split <- split(data, data$date)
  dates_pool <- names(d_split)
  nD <- length(dates_pool)

  boots <- matrix(NA_real_, nrow = B, ncol = length(terms), dimnames = list(NULL, terms))
  for (b in seq_len(B)) {
    samp <- sample(dates_pool, nD, replace = TRUE)
    bd <- bind_rows(d_split[samp])
    mb <- lmrob_safe(fml, bd)
    if (!is.null(mb)) {
      ct <- coef(mb)
      hit <- terms %in% names(ct)
      boots[b, hit] <- ct[terms[hit]]
    }
  }

  bind_rows(lapply(terms, function(tm) {
    v  <- boots[, tm]
    ok <- !is.na(v)
    se <- sd(v[ok])
    data.frame(term = tm, coef = unname(point[tm]), se = se,
               pval = 2 * pnorm(-abs(unname(point[tm]) / se)),
               n_boot_ok = sum(ok), nobs = nrow(data), row.names = NULL)
  }))
}

robust_boot <- function(fml, data, term, B = 200) robust_boot_multi(fml, data, term, B)

# =============================================================================
# A. Baseline average response
# =============================================================================
cat("\n=== A. Baseline average response, unadjusted / winsorized / robust (MM) ===\n\n")

baseline <- bind_rows(lapply(SHOCKS, function(s) {
  d0 <- panel %>% filter(!is.na(usd_w01))
  d  <- d0 %>% mutate(shock_w = winsorize_shock(d0, s$var))

  m_raw <- feols(as.formula(sprintf("usd_w01 ~ %s | country", s$var)), cluster = ~date, data = d)
  m_win <- feols(usd_w01 ~ shock_w | country, cluster = ~date, data = d)
  r_rob <- robust_boot(as.formula(sprintf("usd_w01 ~ %s + country", s$var)), d, s$var, B)

  bind_rows(
    cbind(shock = s$label, matching = s$matching, treatment = "unadjusted",
          coef_row(m_raw, s$var, s$var)),
    cbind(shock = s$label, matching = s$matching, treatment = "winsorized (1/99 pct)",
          coef_row(m_win, "shock_w", s$var)),
    cbind(shock = s$label, matching = s$matching, treatment = "robust (MM, cluster boot.)",
          data.frame(term = s$var, coef = r_rob$coef, se = r_rob$se, pval = r_rob$pval, nobs = r_rob$nobs))
  )
}))

print(baseline[, c("shock", "matching", "treatment", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(baseline, file.path(paths$out_tables, "outlier_baseline.csv"))

# =============================================================================
# B. Regime interaction
# =============================================================================
cat("\n\n=== B. Regime interaction (shock x post_split), unadjusted / winsorized / robust (MM) ===\n\n")

regime <- bind_rows(lapply(SHOCKS, function(s) {
  d0 <- panel %>% filter(!is.na(usd_w01))
  d  <- d0 %>% mutate(shock_w = winsorize_shock(d0, s$var))

  int_term  <- paste0(s$var, ":post_split")
  m_raw <- feols(as.formula(sprintf("usd_w01 ~ %s * post_split | country", s$var)),
                cluster = ~date, data = d)
  m_win <- feols(usd_w01 ~ shock_w * post_split | country, cluster = ~date, data = d)
  r_rob <- robust_boot(as.formula(sprintf("usd_w01 ~ %s * post_split + country", s$var)),
                       d, int_term, B)

  bind_rows(
    cbind(shock = s$label, matching = s$matching, treatment = "unadjusted",
          coef_row(m_raw, int_term, int_term)),
    cbind(shock = s$label, matching = s$matching, treatment = "winsorized (1/99 pct)",
          coef_row(m_win, "shock_w:post_split", int_term)),
    cbind(shock = s$label, matching = s$matching, treatment = "robust (MM, cluster boot.)",
          data.frame(term = int_term, coef = r_rob$coef, se = r_rob$se, pval = r_rob$pval, nobs = r_rob$nobs))
  )
}))

print(regime[, c("shock", "matching", "treatment", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(regime, file.path(paths$out_tables, "outlier_regime_interaction.csv"))

# =============================================================================
# C. Instrument-type split (shock_1y only, matching R/16/R/17's own scope)
# =============================================================================
cat("\n\n=== C. Instrument-type split, unadjusted / winsorized / robust (MM) ===\n\n")

SHOCK <- "shock_1y"
events <- policy %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END, .data[[SHOCK]] != 0) %>%
  mutate(
    quantity = isdRRR | isdLDR,
    price    = isdRevrepo | isdMLF,
    type = case_when(
      quantity & !price ~ "quantity",
      price & !quantity ~ "price",
      quantity & price  ~ "mixed",
      TRUE              ~ "unflagged"
    )
  )

build_inst_pnl <- function(shockvec_name, shockvec) {
  panel %>%
    mutate(!!shockvec_name := shockvec) %>%
    left_join(select(events, date, type), by = "date") %>%
    mutate(
      type        = coalesce(type, "none"),
      shock_price = if_else(type == "price",    .data[[shockvec_name]], 0),
      shock_qty   = if_else(type == "quantity", .data[[shockvec_name]], 0)
    ) %>%
    filter(!is.na(usd_w01))
}

pnl_raw <- build_inst_pnl(SHOCK, panel[[SHOCK]])
pnl_win <- build_inst_pnl(SHOCK, winsorize_shock(panel, SHOCK))

inst_cells <- function(pnl, treatment_label, use_lmrob = FALSE) {
  pre <- filter(pnl, post_split == 0)

  if (!use_lmrob) {
    m_pre <- feols(usd_w01 ~ shock_price + shock_qty | country, cluster = ~date, data = pre)
    m_int <- feols(usd_w01 ~ shock_price + shock_qty + shock_price:post_split +
                              shock_qty:post_split + post_split | country,
                   cluster = ~date, data = pnl)
    bind_rows(
      coef_row(m_pre, "shock_price", "price",    extra = list(treatment = treatment_label, column = "pre-split level")),
      coef_row(m_pre, "shock_qty",   "quantity", extra = list(treatment = treatment_label, column = "pre-split level")),
      coef_row(m_int, "shock_price:post_split", "price",    extra = list(treatment = treatment_label, column = "post minus pre")),
      coef_row(m_int, "shock_qty:post_split",   "quantity", extra = list(treatment = treatment_label, column = "post minus pre"))
    )
  } else {
    r_pre <- robust_boot_multi(usd_w01 ~ shock_price + shock_qty + country, pre,
                               c("shock_price", "shock_qty"), B)
    r_int <- robust_boot_multi(usd_w01 ~ shock_price + shock_qty + shock_price:post_split +
                                          shock_qty:post_split + post_split + country,
                               pnl, c("shock_price:post_split", "shock_qty:post_split"), B)
    bind_rows(
      data.frame(treatment = treatment_label, column = "pre-split level",
                 term = c("price", "quantity"),
                 coef = r_pre$coef, se = r_pre$se, pval = r_pre$pval, nobs = r_pre$nobs),
      data.frame(treatment = treatment_label, column = "post minus pre",
                 term = c("price", "quantity"),
                 coef = r_int$coef, se = r_int$se, pval = r_int$pval, nobs = r_int$nobs)
    )
  }
}

instrument <- bind_rows(
  inst_cells(pnl_raw, "unadjusted"),
  inst_cells(pnl_win, "winsorized (1/99 pct)"),
  inst_cells(pnl_raw, "robust (MM, cluster boot.)", use_lmrob = TRUE)
) %>%
  mutate(column = factor(column, levels = c("pre-split level", "post minus pre"))) %>%
  arrange(term, column, factor(treatment, levels = c("unadjusted", "winsorized (1/99 pct)", "robust (MM, cluster boot.)")))

print(instrument[, c("term", "column", "treatment", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(instrument, file.path(paths$out_tables, "outlier_instrument_type.csv"))

# =============================================================================
# Read: is the post-2015 regime result robust to outlier treatment the way
# leave-one-out (R/26) showed it partially is, or does it fail here too?
# =============================================================================
cat("\n\n=== Summary: does each result survive outlier treatment? ===\n\n")
for (s in SHOCKS) {
  r <- regime %>% filter(shock == s$label, matching == s$matching)
  cat(sprintf("Regime interaction, %s %s: unadjusted p=%.4f, winsorized p=%.4f, robust p=%.4f\n",
              s$label, s$matching,
              r$pval[r$treatment == "unadjusted"],
              r$pval[r$treatment == "winsorized (1/99 pct)"],
              r$pval[r$treatment == "robust (MM, cluster boot.)"]))
}
qty_diff <- instrument %>% filter(term == "quantity", column == "post minus pre")
cat("\nInstrument-type quantity, post minus pre:\n")
for (i in seq_len(nrow(qty_diff))) {
  cat(sprintf("  %-28s coef = %+.3f, p = %.4f\n", qty_diff$treatment[i], qty_diff$coef[i], qty_diff$pval[i]))
}

message("\nSaved: output/tables/outlier_baseline.csv, outlier_regime_interaction.csv, outlier_instrument_type.csv")
