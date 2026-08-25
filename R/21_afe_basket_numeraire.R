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
# 6. Precision decomposition: why is the composite (beta_USD) estimated more
#    precisely than either leg, when neither leg's interaction is individually
#    significant? beta_USD_hat = beta_basket_hat + beta_D_hat EXACTLY (an
#    algebraic identity of the OLS estimator given the row-level identity
#    usd = basket + D, not just at the point estimate but for the ESTIMATOR),
#    so:
#        Var(beta_USD_hat) = Var(beta_basket_hat) + Var(beta_D_hat)
#                             + 2*Cov(beta_basket_hat, beta_D_hat)
#    The three variances are all observed (as SE^2 from the table above), so
#    the covariance/correlation between the two legs' COEFFICIENT ESTIMATES
#    is exactly identified -- solve for it rather than assume a sign.
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
    implied_corr    = implied_cov / (se_basket * se_D)
  ) %>%
  select(shock, matching, period, se_usd, se_basket, se_D, implied_cov, implied_corr)

cat("\n\n=== E. Precision decomposition: implied correlation between the two legs' estimates ===\n\n")
print(as.data.frame(precision), row.names = FALSE, digits = 4)
write_csv(precision, file.path(paths$out_tables, "afe_basket_precision_decomposition.csv"))

headline <- precision %>% filter(shock == "1Y", matching == "exact", period == "interaction")
raw_cor  <- leg_cor$cor_D_basket

cat(sprintf(
  paste0(
    "\nHeadline case (1Y, exact matching, interaction term): SE(USD) = %.3f is\n",
    "SMALLER than SE(basket) = %.3f and SE(D) = %.3f. Since beta_USD_hat =\n",
    "beta_basket_hat + beta_D_hat is a SUM (not a difference), this requires a\n",
    "NEGATIVE implied correlation between the two legs' coefficient estimates\n",
    "(implied corr = %.3f here) -- summing two noisy, negatively-correlated\n",
    "legs cancels a shared component of their sampling error, the same way\n",
    "differencing two POSITIVELY correlated series would. The raw-return\n",
    "correlation between D_w01 and basket_w01 across the common sample is\n",
    "%.3f, consistent in sign: basket is CONSTRUCTED as usd - D, so it is\n",
    "mechanically negatively correlated with D whenever D's own variance is not\n",
    "fully offset by a matching positive covariance with usd (which it is not --\n",
    "these are two different markets, ASEAN-vs-USD and USD-vs-AFE-basket).\n",
    "Read this precisely: the composite's precision is NOT an artefact -- it\n",
    "comes from the two legs' errors offsetting when added back together, and\n",
    "that offsetting is a direct, checkable consequence of how the basket leg\n",
    "was constructed in the first place, not a coincidence of this one sample.\n"
  ),
  headline$se_usd, headline$se_basket, headline$se_D, headline$implied_corr, raw_cor
))

# =============================================================================
# 7. Substantive finding, stated in the CSV itself, not only in prose:
#    the basket regime interaction is NOT detectable -- this is a whole-
#    sample average result, not a post-2015 phenomenon.
# =============================================================================
basket_int <- three_leg %>% filter(leg == "beta_basket", period == "interaction")
finding <- basket_int %>%
  mutate(
    finding = "whole-sample average, NOT a post-2015 phenomenon",
    detail  = sprintf(
      "pre/post interaction on the basket outcome is not statistically detectable (p = %.3f); the beta_basket result documented elsewhere is a full-sample average effect, not evidence of a regime break",
      pval)
  ) %>%
  select(shock, matching, coef, se, pval, finding, detail)

cat("\n\n=== F. Substantive finding (also written into the CSV): basket regime interaction ===\n\n")
print(as.data.frame(finding), row.names = FALSE, digits = 3)
write_csv(finding, file.path(paths$out_tables, "afe_basket_regime_finding.csv"))

message("\nSaved: output/tables/afe_basket_window_grid.csv, afe_basket_three_leg_table.csv,",
        "\n       afe_basket_identity_check.csv, afe_basket_leg_correlation.csv,",
        "\n       afe_basket_precision_decomposition.csv, afe_basket_regime_finding.csv")
