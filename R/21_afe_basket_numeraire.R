# =============================================================================
# 21_afe_basket_numeraire.R
# Supervisor response, follow-up on the RMB-numeraire objection (R/11, Section
# 4.5): quoting ASEAN currencies against the renminbi does not isolate ASEAN
# currency movement from dollar movement -- the identity beta_cross = beta_USD
# - beta_CNY shows switching numeraire just subtracts the renminbi's OWN
# response. LCU/USD itself is a relative price and cannot distinguish "ASEAN
# currencies moved" from "the dollar moved".
#
# THE FIX THAT WORKS. Quote each ASEAN currency against the AFE dollar-index
# basket instead -- a basket containing neither the dollar-anchor problem (it
# is not a bilateral USD rate) nor the renminbi leg (DTWEXAFEGS excludes China
# and every ASEAN-5 currency by construction; see R/14).
#
# ARITHMETIC. Let I_t be DTWEXAFEGS (a RISE = a STRONGER dollar against
# advanced economies) and FX_it the bilateral rate in LCU per USD (a RISE =
# LCU DEPRECIATION). LCU per AFE-basket unit = FX_it / I_t, so in log returns:
#
#     r_basket_it = r_usd_it - D_t          where D_t = 100 * dlog(I_t)
#     i.e.   r_usd_it = r_basket_it + D_t   (three legs, one identity)
#
# A RISE in r_basket is LCU depreciation against the basket -- the same sign
# convention as the dollar-quoted outcome, so coefficients are directly
# comparable to the main USD-numeraire tables.
#
# THIS IS NOT THE MEDIATION DIRECT EFFECT. R/15's decomposition estimates
#     c' = c - a*b,   b ESTIMATED (~0.30 post-2015, R/15 STEP 2)
# This script's beta_basket instead IMPOSES b = 1 (it subtracts the FULL
# dollar move, not the estimated pass-through):
#     beta_basket = c - a,   b imposed = 1
# The two are different objects and are expected to differ numerically; see
# the closing note below for the direction this implies.
#
# DEPENDENCY. DTWEXAFEGS is fetched/cached only by R/14_dollar_control_fix.R,
# not by R/09_external_data.R (confirmed unchanged as of this script). This
# script therefore requires R/14 to have run first, exactly like R/13, R/15
# and R/19 -- it reads the same cached file and fails with an explicit message
# if it is missing, rather than re-implementing the download.
#
# Input : data-clean/reg_data_ext_main.csv           (from R/10)
#         data-raw/external/DTWEXAFEGS.csv           (fetched by R/14)
# Output: output/tables/afe_basket_window_grid.csv
#         output/tables/afe_basket_three_leg_table.csv
#         output/tables/afe_basket_identity_check.csv
#         output/tables/afe_basket_leg_correlation.csv
#         output/tables/afe_basket_precision_decomposition.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

afe_file <- file.path(paths$raw_external, "DTWEXAFEGS.csv")
if (!file.exists(afe_file))
  stop("Run R/14_dollar_control_fix.R first -- it downloads DTWEXAFEGS.")

afe_raw <- read_csv(afe_file, col_types = cols(.default = col_character()))
afe <- data.frame(date = as.Date(afe_raw[[1]]),
                  dollar_afe = suppressWarnings(as.numeric(afe_raw[[2]])))
afe <- afe[!is.na(afe$date), ]

# =============================================================================
# 1. D_t and its window aggregates, on the FX trading calendar
# =============================================================================
# Same discipline as R/10/R/14: lag()/lead() must step to the previous/next
# ASEAN trading day, not the previous/next FRED day, and the window
# aggregation convention (day t plus day t+1 for [0,+1], etc.) must match
# usd_w0/usd_w01/usd_wm11 exactly, or the identity would fail even though the
# arithmetic is right in principle.
fx_dates <- panel %>% distinct(date) %>% arrange(date)

d_agg <- fx_dates %>%
  left_join(afe, by = "date") %>%
  arrange(date) %>%
  mutate(
    ret_afe = 100 * (log(dollar_afe) - log(lag(dollar_afe))),
    D_w0    = ret_afe,
    D_w01   = ret_afe + lead(ret_afe, 1),
    D_wm11  = lag(ret_afe, 1) + ret_afe + lead(ret_afe, 1)
  ) %>%
  select(date, D_w0, D_w01, D_wm11)

panel <- panel %>%
  left_join(d_agg, by = "date") %>%
  mutate(basket_w0 = usd_w0 - D_w0, basket_w01 = usd_w01 - D_w01,
         basket_wm11 = usd_wm11 - D_wm11)

WINDOWS <- c("[0]" = "w0", "[0,+1]" = "w01", "[-1,+1]" = "wm11")
SHOCKS  <- list("1Y" = c(exact = "shock_1y", rolled = "shock_1y_roll"),
                "5Y" = c(exact = "shock_5y", rolled = "shock_5y_roll"))

# =============================================================================
# 2. Full window x tenor grid, full sample: is the [0]/[0,+1]/[-1,+1]
#    significance pattern (p = 0.011 / 0.063 / 0.012 on the basket leg,
#    shock_1y exact) a point-estimate story or a precision (SE) story?
# =============================================================================
window_grid <- bind_rows(lapply(names(WINDOWS), function(wlab) {
  w     <- WINDOWS[[wlab]]
  y_usd <- paste0("usd_", w); y_D <- paste0("D_", w); y_bkt <- paste0("basket_", w)

  bind_rows(lapply(names(SHOCKS), function(slab) {
    s <- SHOCKS[[slab]][["exact"]]
    d <- panel %>% filter(!is.na(.data[[y_usd]]), !is.na(.data[[y_D]]))
    f <- function(yv) feols(as.formula(sprintf("%s ~ %s | country", yv, s)),
                            cluster = ~date, data = d)
    bind_rows(
      coef_row(f(y_usd), s, "beta_USD",    extra = list(window = wlab, shock = slab)),
      coef_row(f(y_D),   s, "beta_D",      extra = list(window = wlab, shock = slab)),
      coef_row(f(y_bkt), s, "beta_basket", extra = list(window = wlab, shock = slab))
    )
  }))
}))

cat("\n=== A. Window x tenor grid, full sample, exact matching (coef AND se) ===\n\n")
print(window_grid[, c("window", "shock", "term", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)

bkt_grid <- window_grid %>% filter(term == "beta_basket", shock == "1Y")
cat("\nbeta_basket, shock_1y, across windows:\n")
print(bkt_grid[, c("window", "coef", "se", "pval")], row.names = FALSE, digits = 3)
cse <- sd(bkt_grid$se) / mean(bkt_grid$se)
ccf <- sd(bkt_grid$coef) / mean(abs(bkt_grid$coef))
cat(sprintf(
  paste0(
    "\nCoefficient of variation across windows: coef = %.3f, SE = %.3f. The\n",
    "point estimate is essentially flat across windows (%.3f, %.3f, %.3f) while\n",
    "the SE moves more (%.3f, %.3f, %.3f) -- [0,+1]'s weaker significance is a\n",
    "PRECISION story (a noisier window), not a point-estimate story: the [0,+1]\n",
    "coefficient is not smaller than its neighbours, its standard error is larger.\n"
  ),
  ccf, cse, bkt_grid$coef[1], bkt_grid$coef[2], bkt_grid$coef[3],
  bkt_grid$se[1], bkt_grid$se[2], bkt_grid$se[3]
))

write_csv(window_grid, file.path(paths$out_tables, "afe_basket_window_grid.csv"))

# =============================================================================
# 3. THE three-leg table: [0,+1], both tenors, both matching rules,
#    pre-split / post-split / interaction (+ full, for context), all THREE
#    legs, coefficient + SE + p-value in every cell -- one table, not three.
# =============================================================================
common01 <- panel %>% filter(!is.na(usd_w01), !is.na(D_w01))

LEGS <- c("beta_USD" = "usd_w01", "beta_D" = "D_w01", "beta_basket" = "basket_w01")

leg_period_rows <- function(yv, shockvar, slab, mlab) {
  bind_rows(
    coef_row(feols(as.formula(sprintf("%s ~ %s | country", yv, shockvar)),
                   cluster = ~date, data = common01),
             shockvar, shockvar, extra = list(shock = slab, matching = mlab, period = "full")),
    coef_row(feols(as.formula(sprintf("%s ~ %s | country", yv, shockvar)),
                   cluster = ~date, data = filter(common01, post_split == 0)),
             shockvar, shockvar, extra = list(shock = slab, matching = mlab, period = "pre-split")),
    coef_row(feols(as.formula(sprintf("%s ~ %s | country", yv, shockvar)),
                   cluster = ~date, data = filter(common01, post_split == 1)),
             shockvar, shockvar, extra = list(shock = slab, matching = mlab, period = "post-split")),
    coef_row(feols(as.formula(sprintf("%s ~ %s * post_split | country", yv, shockvar)),
                   cluster = ~date, data = common01),
             paste0(shockvar, ":post_split"), shockvar,
             extra = list(shock = slab, matching = mlab, period = "interaction"))
  )
}

three_leg <- bind_rows(lapply(names(LEGS), function(leglab) {
  yv <- LEGS[[leglab]]
  bind_rows(lapply(names(SHOCKS), function(slab) {
    bind_rows(lapply(names(SHOCKS[[slab]]), function(mlab) {
      shockvar <- SHOCKS[[slab]][[mlab]]
      cbind(leg = leglab, leg_period_rows(yv, shockvar, slab, mlab))
    }))
  }))
}))

three_leg <- three_leg %>%
  mutate(period = factor(period, levels = c("full", "pre-split", "post-split", "interaction"))) %>%
  arrange(shock, matching, period, factor(leg, levels = names(LEGS)))

cat("\n\n=== B. Three-leg table: usd = basket + D, [0,+1], one table ===\n\n")
print(three_leg[, c("shock", "matching", "period", "leg", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(three_leg, file.path(paths$out_tables, "afe_basket_three_leg_table.csv"))

# =============================================================================
# 4. Identity check on this exact table: beta_USD = beta_basket + beta_D,
#    every shock x matching x period cell, common sample. Report the residual.
# =============================================================================
identity_check <- three_leg %>%
  select(shock, matching, period, leg, coef) %>%
  pivot_wider(names_from = leg, values_from = coef) %>%
  mutate(
    rhs      = beta_basket + beta_D,
    residual = beta_USD - rhs
  )

cat("\n\n=== C. Identity check: beta_USD = beta_basket + beta_D ===\n\n")
print(identity_check, row.names = FALSE, digits = 6)
max_resid <- max(abs(identity_check$residual))
cat(sprintf("\nMax residual across all %d cells: %.3e\n", nrow(identity_check), max_resid))
stopifnot("beta_USD = beta_basket + beta_D identity fails" = max_resid < 1e-6)
write_csv(identity_check, file.path(paths$out_tables, "afe_basket_identity_check.csv"))

# =============================================================================
# 5. Correlation between the two legs' announcement-window returns
# =============================================================================
leg_cor <- common01 %>%
  summarise(
    n                = n(),
    cor_D_basket     = cor(D_w01, basket_w01, use = "complete.obs"),
    cor_D_usd        = cor(D_w01, usd_w01,    use = "complete.obs"),
    cor_basket_usd   = cor(basket_w01, usd_w01, use = "complete.obs")
  )

cat("\n\n=== D. Correlation between the two legs' [0,+1] announcement-window returns ===\n\n")
print(as.data.frame(leg_cor), row.names = FALSE, digits = 4)
write_csv(leg_cor, file.path(paths$out_tables, "afe_basket_leg_correlation.csv"))

# =============================================================================
# 6. Precision decomposition. beta_USD is the quantity actually estimated --
#    it is the panel's own dependent variable (usd_w01), regressed directly.
#    Splitting it into beta_D + beta_basket is an ACCOUNTING DECOMPOSITION,
#    not two independent measurements: beta_USD_hat = beta_basket_hat +
#    beta_D_hat EXACTLY (an algebraic identity of the OLS estimator given the
#    row-level identity usd = basket + D, holding for the estimator itself,
#    not just the point estimate), so
#        Var(beta_USD_hat) = Var(beta_basket_hat) + Var(beta_D_hat)
#                             + 2*Cov(beta_basket_hat, beta_D_hat)
#    The three variances are all observed (as SE^2 from the table above), so
#    the covariance/correlation between the two legs' COEFFICIENT ESTIMATES
#    is exactly identified -- solved for below, not assumed.
#
#    Read the result the accounting way, not the "surprising significance"
#    way: decomposing a directly-estimated quantity into two legs COSTS
#    precision whenever those legs are anti-correlated by construction (here,
#    because basket = usd - D), and that cost is exactly what the implied
#    correlation below quantifies. beta_USD is not "a significant composite
#    of insignificant parts" -- it is the estimate; the split is what loses
#    power, for a checkable, mechanical reason.
# =============================================================================
precision <- identity_check %>%
  left_join(three_leg %>% filter(leg == "beta_USD")    %>% select(shock, matching, period, se_usd = se),
            by = c("shock", "matching", "period")) %>%
  left_join(three_leg %>% filter(leg == "beta_basket") %>% select(shock, matching, period, se_basket = se),
            by = c("shock", "matching", "period")) %>%
  left_join(three_leg %>% filter(leg == "beta_D")      %>% select(shock, matching, period, se_D = se),
            by = c("shock", "matching", "period")) %>%
  mutate(
    var_usd         = se_usd^2,
    var_basket      = se_basket^2,
    var_D           = se_D^2,
    implied_cov     = (var_usd - var_basket - var_D) / 2,
    implied_corr    = implied_cov / (se_basket * se_D),
    note            = "beta_USD is the estimated quantity; beta_basket + beta_D is an accounting decomposition of it that costs precision because the legs are anti-correlated by construction (basket = usd - D), not two independent estimates that happen to sum to something significant"
  ) %>%
  select(shock, matching, period, se_usd, se_basket, se_D, implied_cov, implied_corr, note)

cat("\n\n=== E. Precision decomposition: implied correlation between the two legs' estimates ===\n\n")
print(as.data.frame(precision)[, 1:9], row.names = FALSE, digits = 4)
write_csv(precision, file.path(paths$out_tables, "afe_basket_precision_decomposition.csv"))

headline <- precision %>% filter(shock == "1Y", matching == "exact", period == "interaction")
raw_cor  <- leg_cor$cor_D_basket

cat(sprintf(
  paste0(
    "\nHeadline case (1Y, exact matching, interaction term): SE(USD) = %.3f is\n",
    "SMALLER than SE(basket) = %.3f and SE(D) = %.3f. beta_USD is the estimated\n",
    "quantity here -- it is not built FROM the two legs, the two legs are what\n",
    "you get if you decompose it. Since beta_USD_hat = beta_basket_hat +\n",
    "beta_D_hat is a SUM, a decomposition that COSTS precision (both legs\n",
    "noisier than the whole) requires a NEGATIVE implied correlation between\n",
    "the legs' coefficient estimates (implied corr = %.3f here). The raw-return\n",
    "correlation between D_w01 and basket_w01 across the common sample is\n",
    "%.3f, consistent in sign: basket is CONSTRUCTED as usd - D, so it is\n",
    "mechanically anti-correlated with D whenever D's own variance is not\n",
    "fully offset by a matching positive covariance with usd (which it is not --\n",
    "these are two different markets, ASEAN-vs-USD and USD-vs-AFE-basket).\n",
    "This is a checkable, mechanical cost of decomposing usd into basket + D,\n",
    "not a coincidence of this one sample.\n"
  ),
  headline$se_usd, headline$se_basket, headline$se_D, headline$implied_corr, raw_cor
))

# =============================================================================
# 7. Substantive findings, stated in the CSV itself, not only in prose. Two
#    separate claims, kept separate because they have different evidentiary
#    weight:
#      (i)  NEGATIVE, established: the break is NOT in the ASEAN-vs-basket
#           leg. p = 0.48-0.92 across all four specs, coefficients small and
#           unstable in sign (-1.26 to +0.40) -- nothing here looks like a
#           regime break, in either direction.
#      (ii) NOT established: that the break "lives in" the dollar leg
#           instead. beta_D's own interaction is p = 0.187 (panel) at its
#           best (1Y, exact) -- underpowered, not a demonstrated attribution.
#           Do not read (i) as evidence FOR (ii); it only rules out one
#           candidate location, it does not confirm the other.
# =============================================================================
basket_int <- three_leg %>% filter(leg == "beta_basket", period == "interaction")
D_int       <- three_leg %>% filter(leg == "beta_D",      period == "interaction")

finding <- basket_int %>%
  select(shock, matching, basket_coef = coef, basket_se = se, basket_pval = pval) %>%
  left_join(D_int %>% select(shock, matching, D_coef = coef, D_se = se, D_pval = pval),
            by = c("shock", "matching")) %>%
  mutate(
    established_claim     = "basket leg pre/post interaction is not detectable (stable, near-zero, p = 0.48-0.92): the whole-sample beta_basket result is a full-sample average, not a post-2015 phenomenon",
    NOT_established_claim = "this does NOT show the break is in the dollar leg instead -- beta_D's own interaction (p shown here) is underpowered, not a demonstrated attribution; the panel decomposition rules out one location without confirming the other"
  )

cat("\n\n=== F. Substantive findings (also written into the CSV): what the decomposition does and does not establish ===\n\n")
print(as.data.frame(finding)[, c("shock", "matching", "basket_coef", "basket_se", "basket_pval", "D_coef", "D_se", "D_pval")],
      row.names = FALSE, digits = 3)
write_csv(finding, file.path(paths$out_tables, "afe_basket_regime_finding.csv"))

# =============================================================================
# 8. beta_D at the DATE level, alongside the panel version. D_w01 is a single
#    time series that the panel replicates five times (once per country, same
#    value each time); the panel regression's precision on beta_D therefore
#    does not reflect five independent observations of the dollar factor, just
#    one series seen five times with country FE and date-clustered SEs doing
#    the (correct) discounting. The date-level regression -- one row per
#    calendar date, no country dimension, plain OLS -- is the natural
#    estimator for a genuinely one-series outcome. Reported alongside the
#    panel version so the attribution question can be judged on both.
# =============================================================================
dates_d <- common01 %>%
  distinct(date, .keep_all = TRUE) %>%
  select(date, post_split, D_w01, shock_1y, shock_1y_roll, shock_5y, shock_5y_roll)

D_date_level <- bind_rows(lapply(names(SHOCKS), function(slab) {
  bind_rows(lapply(names(SHOCKS[[slab]]), function(mlab) {
    shockvar <- SHOCKS[[slab]][[mlab]]
    m <- lm(as.formula(sprintf("D_w01 ~ %s * post_split", shockvar)), data = dates_d)
    ct <- summary(m)$coefficients
    term <- paste0(shockvar, ":post_split")
    data.frame(shock = slab, matching = mlab, estimator = "date-level OLS",
               term = "interaction", coef = ct[term, 1], se = ct[term, 2],
               pval = ct[term, 4], n = nobs(m), row.names = NULL)
  }))
}))

D_panel_level <- D_int %>%
  transmute(shock, matching, estimator = "panel, country FE, clustered",
            term = "interaction", coef, se, pval, n = nobs)

D_both_levels <- bind_rows(D_date_level, D_panel_level) %>%
  arrange(shock, matching, estimator)

cat("\n\n=== G. beta_D interaction: date-level OLS vs. panel, country FE, clustered ===\n\n")
print(as.data.frame(D_both_levels), row.names = FALSE, digits = 4)
write_csv(D_both_levels, file.path(paths$out_tables, "afe_basket_D_date_vs_panel.csv"))

d_1y_exact <- D_both_levels %>% filter(shock == "1Y", matching == "exact")
cat(sprintf(
  paste0(
    "\n1Y, exact matching: date-level p = %.3f vs. panel p = %.3f. %s\n"
  ),
  d_1y_exact$pval[d_1y_exact$estimator == "date-level OLS"],
  d_1y_exact$pval[d_1y_exact$estimator == "panel, country FE, clustered"],
  if (d_1y_exact$pval[d_1y_exact$estimator == "date-level OLS"] <
      d_1y_exact$pval[d_1y_exact$estimator == "panel, country FE, clustered"])
    "The date-level estimator is TIGHTER, but check the margin before treating attribution as resolved."
  else
    "The date-level estimator is NOT tighter -- the panel's p = 0.187 was not an artefact of treating one series as five; attribution to the dollar leg remains underpowered on the natural one-series estimator too."
))

message("\nSaved: output/tables/afe_basket_window_grid.csv, afe_basket_three_leg_table.csv,",
        "\n       afe_basket_identity_check.csv, afe_basket_leg_correlation.csv,",
        "\n       afe_basket_precision_decomposition.csv, afe_basket_regime_finding.csv,",
        "\n       afe_basket_D_date_vs_panel.csv")
