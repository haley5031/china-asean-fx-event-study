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
# Output: output/tables/afe_basket_decomposition.csv
#         output/tables/afe_basket_regime_split.csv
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
# usd_w0/usd_w01/usd_wm11 exactly, or beta_basket = beta_USD - beta_D would
# fail even though the arithmetic is right in principle.
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

panel <- panel %>% left_join(d_agg, by = "date")

WINDOWS <- c("[0]" = "w0", "[0,+1]" = "w01", "[-1,+1]" = "wm11")
SHOCKS  <- c("1Y" = "shock_1y", "5Y" = "shock_5y")

# =============================================================================
# 2. Decomposition on a common sample per window:
#    beta_basket = beta_USD - beta_D  (b = 1 imposed, exact identity)
# =============================================================================
decomp <- bind_rows(lapply(names(WINDOWS), function(wlab) {

  w     <- WINDOWS[[wlab]]
  y_usd <- paste0("usd_", w)
  y_D   <- paste0("D_", w)

  d <- panel %>%
    mutate(basket = .data[[y_usd]] - .data[[y_D]]) %>%
    filter(!is.na(.data[[y_usd]]), !is.na(.data[[y_D]]))

  f <- function(yv) feols(as.formula(sprintf("%s ~ shock_1y | country", yv)),
                          cluster = ~date, data = d)

  m_usd    <- f(y_usd)
  m_D      <- f(y_D)
  m_basket <- f("basket")

  bind_rows(
    coef_row(m_usd,    "shock_1y", "beta_USD    (LCU per USD)",       extra = list(window = wlab)),
    coef_row(m_D,      "shock_1y", "beta_D      (AFE index's own response)", extra = list(window = wlab)),
    coef_row(m_basket, "shock_1y", "beta_basket (LCU per AFE-basket unit)",  extra = list(window = wlab))
  )
}))

cat("\n=== A. AFE-basket decomposition:  beta_basket = beta_USD - beta_D (b = 1 imposed) ===\n")
cat("    Common sample per window; country FE; SEs clustered by date.\n\n")
print(decomp[, c("window", "term", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)

# Hard assertion: the identity must hold to machine precision, exactly as
# Table 9 verifies Equation (16) for the RMB cross rate in R/11.
cat("\n--- identity check ---\n")
resid_max <- 0
for (wlab in names(WINDOWS)) {
  b <- decomp[decomp$window == wlab, ]
  lhs <- b$coef[3]                 # beta_basket
  rhs <- b$coef[1] - b$coef[2]     # beta_USD - beta_D
  d   <- abs(lhs - rhs)
  resid_max <- max(resid_max, d)
  cat(sprintf("  %-8s beta_basket = %+8.4f   beta_USD - beta_D = %+8.4f   diff = %.2e\n",
              wlab, lhs, rhs, d))
}
cat(sprintf("\nMax residual across windows: %.3e\n", resid_max))
stopifnot(
  "beta_basket = beta_USD - beta_D identity fails" = resid_max < 1e-6
)

write_csv(decomp, file.path(paths$out_tables, "afe_basket_decomposition.csv"))

# =============================================================================
# 3. Regime split on the basket outcome, [0,+1], both tenors
# =============================================================================
basket_w01 <- panel %>%
  mutate(basket_w01 = usd_w01 - D_w01) %>%
  filter(!is.na(basket_w01))

split_tab <- bind_rows(lapply(names(SHOCKS), function(slab) {
  s <- SHOCKS[[slab]]
  f <- as.formula(sprintf("basket_w01 ~ %s | country", s))
  bind_rows(
    coef_row(feols(f, cluster = ~date, data = basket_w01),
             s, s, extra = list(shock = slab, period = "full")),
    coef_row(feols(f, cluster = ~date, data = filter(basket_w01, post_split == 0)),
             s, s, extra = list(shock = slab, period = "pre-split")),
    coef_row(feols(f, cluster = ~date, data = filter(basket_w01, post_split == 1)),
             s, s, extra = list(shock = slab, period = "post-split"))
  )
}))

int_tab <- bind_rows(lapply(names(SHOCKS), function(slab) {
  s <- SHOCKS[[slab]]
  f <- as.formula(sprintf("basket_w01 ~ %s * post_split | country", s))
  m <- feols(f, cluster = ~date, data = basket_w01)
  coef_row(m, paste0(s, ":post_split"), s,
           extra = list(shock = slab, period = "interaction"))
}))

regime_tab <- bind_rows(split_tab, int_tab)

cat("\n\n=== B. Regime split on the AFE-basket outcome, [0,+1] ===\n\n")
print(regime_tab[, c("shock", "period", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(regime_tab, file.path(paths$out_tables, "afe_basket_regime_split.csv"))

# =============================================================================
# Closing note: read this against R/15's mediation decomposition, not as a
# contradiction of it.
# =============================================================================
b_full <- decomp$coef[decomp$window == "[0,+1]" & decomp$term == "beta_basket (LCU per AFE-basket unit)"]
b_se   <- decomp$se[decomp$window == "[0,+1]"   & decomp$term == "beta_basket (LCU per AFE-basket unit)"]

cat("\n\n--- interpretation ---\n")
cat(sprintf(
  "beta_basket, [0,+1], full sample, shock_1y: %+.3f  (SE = %.3f, reported prominently\n",
  b_full, b_se))
cat("because path a -- the shock's effect on the dollar factor -- is only marginally\n",
    "significant on the preferred panel estimator (R/15 STEP 2: p = 0.060 post-2015),\n",
    "and beta_basket inherits that imprecision.)\n\n", sep = "")
cat("This outcome imposes b = 1 (subtracts the FULL dollar move) where R/15's\n",
    "mediation decomposition estimates b ~ 0.30 post-2015. So beta_basket is NOT\n",
    "the mediation direct effect c': with post-2015 c ~ -1.51 and a ~ -3.41 (R/15\n",
    "STEP 2, AFE dollar), c - a ~ +1.90 -- ASEAN currencies DEPRECIATING against\n",
    "the AFE basket on a Chinese tightening surprise, post-2015. If the interaction\n",
    "term above is negative and significant (pre-split less negative / less positive\n",
    "than post-split, i.e. the basket response becomes more positive after 2015),\n",
    "that is what Section 4.8's mediation result PREDICTS, not a contradiction of\n",
    "it -- but it complicates the risk-appetite reading in Section 5.2, because it\n",
    "says ASEAN currencies weaken against a basket that is neither the dollar nor\n",
    "the renminbi when China tightens. Check the sign and significance above before\n",
    "writing this up either way.\n", sep = "")

message("\nSaved: output/tables/afe_basket_decomposition.csv, afe_basket_regime_split.csv")
