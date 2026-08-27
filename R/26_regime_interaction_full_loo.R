# =============================================================================
# 26_regime_interaction_full_loo.R
# R/13 check 3 (Appendix B's influence check) drops each of the 18 POST-split
# nonzero-surprise events in turn and re-estimates the post-split coefficient
# alone. It never ran the same check on the 38 PRE-split events -- and R/22's
# leave-one-out (this branch's own prior finding) showed the pre-split own
# response is 98% carried by a single date (26 Nov 2008). Since the regime
# interaction is estimated as (post response) MINUS (pre response), an
# inflated pre-period coefficient inflates the interaction whether or not the
# post side is stable. This script closes that gap directly.
#
# SPECIFICATION (Equation 7): usd_w01 ~ shock * post_split | country, cluster
# by date -- the exact specification the interaction term is reported from
# elsewhere in the thesis. Every one of the 56 (1Y, exact) / 71 (1Y, rolled) /
# 61 (5Y, exact) / 76 (5Y, rolled) nonzero-surprise event dates -- pre AND
# post, not post only -- is dropped in turn (that date's rows removed
# entirely, all countries, same convention as R/13/R/22), and BOTH the
# interaction term and the main "shock" term (= the pre-split coefficient in
# this treatment-coded specification, since post_split = 0 is the reference
# level) are recorded for each drop.
#
# Input : data-clean/reg_data_ext_main.csv   (from R/10)
# Output: output/tables/regime_interaction_full_loo.csv
#         output/tables/regime_interaction_excl_nov2008.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

SHOCKS <- list(
  list(label = "1Y", matching = "exact",  var = "shock_1y",      ev = "event_day"),
  list(label = "1Y", matching = "rolled", var = "shock_1y_roll", ev = "event_day_roll"),
  list(label = "5Y", matching = "exact",  var = "shock_5y",      ev = "event_day"),
  list(label = "5Y", matching = "rolled", var = "shock_5y_roll", ev = "event_day_roll")
)

nov2008 <- as.Date("2008-11-26")

loo_interaction <- function(v, evcol) {
  events <- panel %>%
    filter(.data[[evcol]] == 1, .data[[v]] != 0) %>%
    distinct(date, .keep_all = TRUE) %>%
    mutate(side = if_else(date >= SPLIT_DATE, "post", "pre")) %>%
    select(date, shock = !!v, side) %>%
    arrange(date)

  fml <- as.formula(sprintf("usd_w01 ~ %s * post_split | country", v))
  int_term <- paste0(v, ":post_split")

  m_base <- feols(fml, cluster = ~date, data = panel)
  ct_base <- as.data.frame(coeftable(m_base))
  base_int <- ct_base[int_term, "Estimate"]
  base_pre <- ct_base[v, "Estimate"]

  loo <- bind_rows(lapply(seq_len(nrow(events)), function(i) {
    d  <- events$date[i]
    dd <- filter(panel, date != d)
    m  <- feols(fml, cluster = ~date, data = dd)
    ct <- as.data.frame(coeftable(m))
    pcol <- grep("^Pr", names(ct))[1]
    data.frame(
      dropped_date  = as.character(d), side = events$side[i], shock_dropped = events$shock[i],
      int_coef = ct[int_term, "Estimate"], int_se = ct[int_term, "Std. Error"], int_pval = ct[int_term, pcol],
      pre_coef = ct[v, "Estimate"],        pre_se = ct[v, "Std. Error"],        pre_pval = ct[v, pcol],
      int_shift = ct[int_term, "Estimate"] - base_int,
      pre_shift = ct[v, "Estimate"] - base_pre,
      row.names = NULL, stringsAsFactors = FALSE
    )
  }))

  list(base_int = base_int, base_pre = base_pre, loo = loo, n_events = nrow(events))
}

results <- lapply(SHOCKS, function(s) {
  r <- loo_interaction(s$var, s$ev)
  cbind(shock = s$label, matching = s$matching, r$loo)
})
full_loo <- bind_rows(results)

cat("\n=== A. Full leave-one-out on the regime interaction, ALL nonzero-surprise events ===\n\n")
cat("    usd_w01 ~ shock * post_split | country, cluster by date.\n\n")

for (s in SHOCKS) {
  key <- paste(s$label, s$matching)
  d <- full_loo %>% filter(shock == s$label, matching == s$matching)
  base_row <- feols(as.formula(sprintf("usd_w01 ~ %s * post_split | country", s$var)),
                    cluster = ~date, data = panel)
  ct <- as.data.frame(coeftable(base_row))
  base_int <- ct[paste0(s$var, ":post_split"), "Estimate"]
  base_pre <- ct[s$var, "Estimate"]

  cat(sprintf("--- %s: baseline interaction = %+.3f, baseline pre-split coef = %+.3f (n = %d events: %d pre, %d post) ---\n",
              key, base_int, base_pre, nrow(d), sum(d$side == "pre"), sum(d$side == "post")))

  most_infl_pre  <- d %>% filter(side == "pre")  %>% slice_max(abs(int_shift), n = 1)
  most_infl_post <- d %>% filter(side == "post") %>% slice_max(abs(int_shift), n = 1)

  cat(sprintf("  Interaction range across all drops : [%.3f, %.3f]; p < 0.05 in %d of %d drops.\n",
              min(d$int_coef), max(d$int_coef), sum(d$int_pval < 0.05), nrow(d)))
  cat(sprintf("  Most influential PRE-side date  : %s (shock = %+.3f), interaction shifts by %+.3f\n",
              most_infl_pre$dropped_date, most_infl_pre$shock_dropped, most_infl_pre$int_shift))
  cat(sprintf("  Most influential POST-side date : %s (shock = %+.3f), interaction shifts by %+.3f\n\n",
              most_infl_post$dropped_date, most_infl_post$shock_dropped, most_infl_post$int_shift))
}

write_csv(full_loo, file.path(paths$out_tables, "regime_interaction_full_loo.csv"))

# =============================================================================
# B. 26 Nov 2008 excluded, as its own row -- interaction AND pre-split coef
# =============================================================================
excl_nov <- bind_rows(lapply(SHOCKS, function(s) {
  r <- full_loo %>% filter(shock == s$label, matching == s$matching,
                           dropped_date == as.character(nov2008))
  if (!nrow(r)) return(NULL)
  data.frame(
    shock = s$label, matching = s$matching,
    term = c("interaction (shock x post_split)", "pre-split coefficient (main term)"),
    coef = c(r$int_coef, r$pre_coef), se = c(r$int_se, r$pre_se), pval = c(r$int_pval, r$pre_pval),
    row.names = NULL
  )
}))

cat("\n\n=== B. 26 Nov 2008 excluded: interaction and pre-split coefficient, each as its own row ===\n\n")
print(excl_nov, row.names = FALSE, digits = 4)
write_csv(excl_nov, file.path(paths$out_tables, "regime_interaction_excl_nov2008.csv"))

cat("\n\n--- Plain answer: does the interaction survive at 5% without 26 Nov 2008? ---\n\n")
for (s in SHOCKS) {
  key <- paste0(s$label, ", ", s$matching)
  r <- excl_nov %>% filter(shock == s$label, matching == s$matching, term == "interaction (shock x post_split)")
  base <- full_loo %>% filter(shock == s$label, matching == s$matching)
  base_int_pval <- ct <- NULL
  m0 <- feols(as.formula(sprintf("usd_w01 ~ %s * post_split | country", s$var)), cluster = ~date, data = panel)
  ct0 <- as.data.frame(coeftable(m0))
  p_base <- ct0[paste0(s$var, ":post_split"), grep("^Pr", names(ct0))[1]]
  cat(sprintf("  %-14s baseline p = %.4f  ->  excl. 26 Nov 2008 p = %.4f  ->  %s\n",
              key, p_base, r$pval,
              if (r$pval < 0.05) "SURVIVES at 5%" else "DOES NOT survive at 5%"))
}

message("\nSaved: output/tables/regime_interaction_full_loo.csv, regime_interaction_excl_nov2008.csv")
