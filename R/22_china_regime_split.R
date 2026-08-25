# =============================================================================
# 22_china_regime_split.R
# Supervisor response: "China kept a fixed e over part of your sample, but
# gradually loosened that over time. That requires a different type of
# monetary policy. Would it not be more relevant to consider that?"
#
# The point is a trilemma one: while the RMB was effectively pegged, PBoC
# policy was partly constrained by the dollar, so a Chinese surprise in 2009
# is a different object from one in 2018. R/12's pre/post-2015 split lumps
# two China exchange-rate regimes together on the pre side. This script splits
# the sample into THREE China exchange-rate regimes instead of two:
#
#   R1  2008-01-01 to 2010-06-18   crisis re-peg at ~6.83 CNY/USD
#   R2  2010-06-19 to 2015-08-10   managed float, widening bands
#   R3  2015-08-11 to 2020-12-31   post-fixing-reform (= R/12's "post-split")
#
# THREE things are reported, none inferred from the others:
#   (a) each regime's OWN response, estimated directly (not as a level term
#       plus an interaction that has to be added by hand);
#   (b) the R2-vs-R1 and R3-vs-R2 differences, each estimated DIRECTLY on its
#       own pairwise subsample with its own standard error -- the same idea as
#       Equation (9)'s shock x post_split interaction in Section 4.7, just
#       applied to adjacent regime pairs instead of one split;
#   (c) a joint F-test (Wald) that all three regimes' responses are equal.
#
# All three are run for shock_1y and shock_5y, under both the exact-date and
# forward-rolled matching rules (R/10). Event counts per regime are reported
# first: if a regime has too few nonzero surprises, that is flagged rather
# than treated as an estimable difference.
#
# Input : data-clean/reg_data_ext_main.csv   (from R/10)
# Output: output/tables/china_regime_event_counts.csv
#         output/tables/china_regime_own_response.csv
#         output/tables/china_regime_diffs.csv
#         output/tables/china_regime_ftest.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

R1_END <- as.Date("2010-06-18")
R2_END <- as.Date("2015-08-10")   # SPLIT_DATE (2015-08-11) starts R3

stopifnot(R2_END + 1 == SPLIT_DATE)

panel <- panel %>%
  mutate(
    china_regime = case_when(
      date <= R1_END              ~ "R1",
      date <= R2_END               ~ "R2",
      TRUE                         ~ "R3"
    ),
    r1 = as.integer(china_regime == "R1"),
    r2 = as.integer(china_regime == "R2"),
    r3 = as.integer(china_regime == "R3")
  )

SHOCKS <- list(
  list(label = "1Y", matching = "exact",  var = "shock_1y"),
  list(label = "1Y", matching = "rolled", var = "shock_1y_roll"),
  list(label = "5Y", matching = "exact",  var = "shock_5y"),
  list(label = "5Y", matching = "rolled", var = "shock_5y_roll")
)

# =============================================================================
# 0. Event counts per regime
# =============================================================================
# "Event day" for the rolled series uses event_day_roll / the *_roll shock;
# for the exact series, event_day / the plain shock. Nonzero-surprise dates
# only -- a zero-surprise announcement carries no identifying variation.
event_counts <- bind_rows(lapply(SHOCKS, function(s) {
  ev_col <- if (s$matching == "exact") "event_day" else "event_day_roll"
  panel %>%
    filter(.data[[ev_col]] == 1, .data[[s$var]] != 0) %>%
    distinct(date, .keep_all = TRUE) %>%
    group_by(china_regime) %>%
    summarise(n_events = n(),
              mean_abs = mean(abs(.data[[s$var]])),
              .groups = "drop") %>%
    mutate(shock = s$label, matching = s$matching)
}))

MIN_EVENTS <- 5L
event_counts <- event_counts %>%
  mutate(flag_low_power = n_events < MIN_EVENTS) %>%
  select(shock, matching, china_regime, n_events, mean_abs, flag_low_power) %>%
  arrange(shock, matching, china_regime)

cat("\n=== 0. Nonzero-surprise event counts per China exchange-rate regime ===\n\n")
print(as.data.frame(event_counts), row.names = FALSE, digits = 3)
write_csv(event_counts, file.path(paths$out_tables, "china_regime_event_counts.csv"))

low_power <- event_counts %>% filter(flag_low_power)
if (nrow(low_power)) {
  cat("\nFLAG: the following regime/shock/matching cells have fewer than",
      MIN_EVENTS, "nonzero-surprise event dates and should be reported as\n",
      "underpowered, not as a noisy-but-estimable coefficient:\n")
  print(as.data.frame(low_power), row.names = FALSE)
} else {
  cat("\nNo regime/shock/matching cell falls below the", MIN_EVENTS, "-event floor.\n")
}

# =============================================================================
# 1. Each regime's own response (direct, own SE per regime)
# =============================================================================
# No main "shock" term is included -- shock is fully interacted with all
# three regime dummies (r1, r2, r3), so each interaction coefficient IS that
# regime's own slope, not a deviation from a reference. r2/r3 mains absorb any
# level shift; r1's level is the implicit reference absorbed by the country FE.
own_response <- bind_rows(lapply(SHOCKS, function(s) {
  v <- s$var
  f <- as.formula(sprintf("usd_w01 ~ r2 + r3 + %s:r1 + %s:r2 + %s:r3 | country",
                          v, v, v))
  m <- feols(f, cluster = ~date, data = panel)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  bind_rows(lapply(c("r1", "r2", "r3"), function(rg) {
    # R's terms() reorders "A:B" to "B:A" when B (here, the regime dummy)
    # first appears earlier in the formula than A (the shock) -- true for
    # r2/r3 (they also appear as main effects) but not r1 (no main effect,
    # so "shock:r1" keeps its written order). Match either order.
    hit <- ct[ct$term %in% c(paste0(v, ":", rg), paste0(rg, ":", v)), , drop = FALSE]
    if (!nrow(hit)) return(NULL)
    data.frame(shock = s$label, matching = s$matching, regime = toupper(rg),
               coef = hit$Estimate[1], se = hit$`Std. Error`[1],
               pval = hit[[grep("^Pr", names(hit))[1]]][1],
               nobs = nobs(m), row.names = NULL)
  }))
}))

cat("\n\n=== 1. Each regime's own response, usd_w01, country FE, clustered by date ===\n\n")
print(own_response, row.names = FALSE, digits = 3)
write_csv(own_response, file.path(paths$out_tables, "china_regime_own_response.csv"))

# =============================================================================
# 2. R2-vs-R1 and R3-vs-R2 differences, each estimated directly with its own SE
# =============================================================================
# Same logic as Equation (9)'s shock x post_split: fit shock * hi_dummy on the
# TWO-regime subsample only, so the interaction term IS the adjacent-regime
# difference with its own standard error -- not a subtraction of two
# separately-estimated coefficients from the three-regime model above.
pairwise_diff <- function(v, lo_regime, hi_regime, contrast_label) {
  d <- panel %>%
    filter(china_regime %in% c(lo_regime, hi_regime)) %>%
    mutate(hi = as.integer(china_regime == hi_regime))
  f <- as.formula(sprintf("usd_w01 ~ %s * hi | country", v))
  m <- feols(f, cluster = ~date, data = d)
  coef_row(m, paste0(v, ":hi"), v,
           extra = list(contrast = contrast_label))
}

diffs <- bind_rows(lapply(SHOCKS, function(s) {
  v <- s$var
  bind_rows(
    cbind(shock = s$label, matching = s$matching,
          pairwise_diff(v, "R1", "R2", "R2 - R1")),
    cbind(shock = s$label, matching = s$matching,
          pairwise_diff(v, "R2", "R3", "R3 - R2"))
  )
}))

cat("\n\n=== 2. Adjacent-regime differences, estimated directly on the pairwise subsample ===\n\n")
print(diffs[, c("shock", "matching", "contrast", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(diffs, file.path(paths$out_tables, "china_regime_diffs.csv"))

# =============================================================================
# 3. Joint F-test: are all three regimes' responses equal?
# =============================================================================
# Standard reference-coded interaction (R1 as reference, via case_when's
# level order taken alphabetically by factor()) -- shock_1y:china_regimeR2 and
# shock_1y:china_regimeR3 are the deviations from R1's slope. Jointly testing
# both equal to zero is exactly the null that all three regimes share one
# response, the same wald()-on-a-regex approach as R/20's H2 test.
ftest <- bind_rows(lapply(SHOCKS, function(s) {
  v <- s$var
  d <- panel %>% mutate(china_regime = factor(china_regime, levels = c("R1", "R2", "R3")))
  f <- as.formula(sprintf("usd_w01 ~ %s * china_regime | country", v))
  m <- feols(f, cluster = ~date, data = d)
  w <- wald(m, paste0(v, ":china_regime"))
  data.frame(shock = s$label, matching = s$matching,
             F_stat = w$stat, df1 = w$df1, df2 = w$df2, pval = w$p,
             row.names = NULL)
}))

cat("\n\n=== 3. Joint F-test: H0: R1 response = R2 response = R3 response ===\n\n")
print(ftest, row.names = FALSE, digits = 4)
write_csv(ftest, file.path(paths$out_tables, "china_regime_ftest.csv"))

for (i in seq_len(nrow(ftest))) {
  r <- ftest[i, ]
  cat(sprintf("  %s, %-6s: F(%d, %.0f) = %.3f, p = %.4f  -> %s\n",
              r$shock, r$matching, r$df1, r$df2, r$F_stat, r$pval,
              if (r$pval < 0.05) "reject equal-response null at 5%"
              else "cannot reject equal-response null at 5%"))
}

message("\nSaved: output/tables/china_regime_*.csv")
