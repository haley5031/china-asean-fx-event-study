# =============================================================================
# 12_sample_split.R
# Supervisor response: "I would use a split that is relevant because of changes
# in Chinese monetary policy. The most relevant one is the collapse of the
# housing bubble China faced, and that massively moved the PBC to intervene in
# the banking and financial sector."
#
# Break point: SPLIT_DATE (set in R/00_setup.R, default 2015-08-11).
#
# The break is chosen ex ante, on institutional grounds, and is unusually well
# identified because three things coincide within roughly a year:
#
#   (i)   the 2014-15 Chinese property downturn -- the in-sample analogue of the
#         episode raised in supervision, the later Evergrande collapse falling
#         outside a sample ending in 2020 -- and the sustained PBoC easing and
#         banking-sector intervention it triggered. This is visible directly in
#         the shock series as the run of negative surprises from late 2014
#         through 2015 (Figure 2 in the draft).
#   (ii)  the 11 August 2015 RMB fixing reform, after which CNY/USD stops being
#         near-constant. This matters mechanically for the numeraire question:
#         before the reform the USD and RMB cross-rate outcomes are close to
#         identical up to a drift, so they can only diverge afterwards.
#   (iii) Das and Song (2022): interest-rate liberalisation "largely completed"
#         with the removal of the deposit-rate ceiling in 2015, after which the
#         DR007-based framework matures.
#
# Reporting all three justifications matters: it makes the split a design
# choice defensible before seeing results, not a break selected on the data.
#
# Input : data-clean/reg_data_ext_main.csv   (from R/10)
# Output: output/tables/split_*.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

SHOCK <- "shock_1y"

pre  <- filter(panel, date <  SPLIT_DATE)
post <- filter(panel, date >= SPLIT_DATE)

# --- How much identifying variation sits on each side? -----------------------
ev <- panel %>%
  filter(event_day == 1, .data[[SHOCK]] != 0) %>%
  distinct(date, .keep_all = TRUE) %>%
  mutate(period = if_else(date < SPLIT_DATE, "pre", "post")) %>%
  group_by(period) %>%
  summarise(n_events  = n(),
            mean_abs  = mean(abs(.data[[SHOCK]])),
            max_abs   = max(abs(.data[[SHOCK]])),
            .groups   = "drop")

cat("\n=== Nonzero surprises either side of ", format(SPLIT_DATE), " ===\n", sep = "")
print(as.data.frame(ev), row.names = FALSE, digits = 3)
write_csv(ev, file.path(paths$out_tables, "split_event_counts.csv"))
cat("\nRead the event counts before the coefficients: if one side carries only a\n",
    "handful of surprises, a difference across periods is a power difference,\n",
    "not necessarily a structural one.\n", sep = "")

# =============================================================================
# 1. Split estimates, both numeraires
# =============================================================================
split_tab <- bind_rows(lapply(c("usd_w01", "cny_w01"), function(y) {
  f <- as.formula(sprintf("%s ~ %s | country", y, SHOCK))
  num <- if (grepl("^usd", y)) "LCU per USD" else "LCU per CNY"
  bind_rows(
    coef_row(feols(f, cluster = ~date, data = panel),
             SHOCK, SHOCK, extra = list(numeraire = num, period = "full")),
    coef_row(feols(f, cluster = ~date, data = pre),
             SHOCK, SHOCK, extra = list(numeraire = num, period = "pre-split")),
    coef_row(feols(f, cluster = ~date, data = post),
             SHOCK, SHOCK, extra = list(numeraire = num, period = "post-split"))
  )
}))

cat("\n\n=== Average response by period and numeraire, [0,+1] ===\n\n")
print(split_tab[, c("numeraire", "period", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(split_tab, file.path(paths$out_tables, "split_by_period.csv"))

# =============================================================================
# 2. Formal test of the break: shock x post interaction
# =============================================================================
# Estimated on the pooled sample so the difference across periods gets a
# standard error, rather than being eyeballed from two separate regressions.
m_int_usd <- feols(
  as.formula(sprintf("usd_w01 ~ %s * post_split | country", SHOCK)),
  cluster = ~date, data = panel
)
m_int_cny <- feols(
  as.formula(sprintf("cny_w01 ~ %s * post_split | country", SHOCK)),
  cluster = ~date, data = panel
)

cat("\n\n=== Test of the structural break (shock x post interaction) ===\n")
print(etable(list("USD numeraire" = m_int_usd, "RMB numeraire" = m_int_cny),
       cluster = ~date, digits = 3))
cat("\nThe interaction term is the object of interest: it tests whether the\n",
    "response differs across the two policy regimes. The level term is the\n",
    "pre-split response; the post-split response is level + interaction.\n", sep = "")

# =============================================================================
# 3. Does the Singapore result survive the split?
# =============================================================================
# The one significant cross-country difference in the draft. If it is a genuine
# regime feature it should appear on the side of the split where Singapore's
# basket would plausibly carry a larger RMB weight -- that is, post-2015.
subsamples <- list(full = panel, pre = pre, post = post)

het_split <- bind_rows(lapply(names(subsamples), function(lab) {
  dd <- subsamples[[lab]]
  m <- feols(
    as.formula(sprintf("usd_w01 ~ %s * i(country, ref = '%s') | country",
                       SHOCK, REF_COUNTRY)),
    cluster = ~date, data = dd
  )
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  sg <- ct[grepl("sgd", ct$term), , drop = FALSE]
  if (!nrow(sg)) return(NULL)
  data.frame(
    period = lab,
    term   = sg$term,
    coef   = sg[["Estimate"]],
    se     = sg[["Std. Error"]],
    pval   = sg[[grep("^Pr", names(sg))[1]]],
    row.names = NULL, stringsAsFactors = FALSE
  )
}))

cat("\n\n=== Singapore vs Indonesia, by period ([0,+1], USD numeraire) ===\n\n")
print(het_split, row.names = FALSE, digits = 3)
write_csv(het_split, file.path(paths$out_tables, "split_singapore.csv"))

message("\nSaved: output/tables/split_*.csv")
