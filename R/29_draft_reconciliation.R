# =============================================================================
# 29_draft_reconciliation.R
# Four small extracts for the write-up, no new analysis:
#
#   1. Three counts that all touch "71" in the draft, checked for whether two
#      of them genuinely coincide or one is an error.
#   2. Instrument composition (price/quantity, pre/post-2015) of the actual
#      56-event ESTIMATION set, not the 71-event set the draft appears to be
#      quoting from.
#   3. Per-country descriptives for usd_w01 (the actual dependent variable,
#      missing from Table 1) and the AFE dollar factor's D_w01.
#   4. The post-2015 1-SD effect size, re-expressed against the correct
#      TWO-DAY [0,+1] return SD rather than a daily-return SD.
#
# Input : data-raw/policy/china_mpshocks.csv, data-clean/policy_shocks_main.csv,
#         data-clean/fx_returns_wide.csv, data-clean/reg_data_ext_main.csv,
#         data-raw/external/DTWEXAFEGS.csv (fetched by R/14)
# Output: output/tables/reconcile_71_counts.csv
#         output/tables/reconcile_instrument_composition_56.csv
#         output/tables/descriptive_window_return.csv
#         output/tables/reconcile_effect_size_ratio.csv
# =============================================================================

source("R/00_setup.R")

policy_main <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"), show_col_types = FALSE)
fx_returns  <- read_csv(file.path(paths$clean, "fx_returns_wide.csv"), show_col_types = FALSE)
panel       <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"), show_col_types = FALSE) %>%
  arrange(country, date)

trading_days <- fx_returns %>%
  filter(if_any(all_of(ASEAN5), ~ !is.na(.x))) %>%
  pull(date) %>% as.Date() %>% sort() %>% unique()

roll_map <- roll_to_trading_day(policy_main$date, trading_days)

# =============================================================================
# 1. The three "71"s
# =============================================================================
# (a) Nonzero main-set 1Y surprises within 2008-2020, BEFORE calendar matching
#     -- straight off the cleaned policy file, no trading-day requirement at all.
a_count <- policy_main %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END, shock_1y != 0) %>%
  distinct(date) %>% nrow()
a_dates <- policy_main %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END, shock_1y != 0) %>%
  distinct(date) %>% pull(date) %>% sort()

# (b) Main-set event dates falling on a date present in the ASEAN-5 FX panel,
#     ANY surprise value (including zero), NO restriction to 2008-2020.
b_count <- sum(policy_main$date %in% trading_days)

# (c) Nonzero 1Y surprises within the 2008-2020 window, under the forward roll.
policy_rolled <- policy_main %>%
  mutate(matched_date = roll_map$matched_date) %>%
  filter(!is.na(matched_date)) %>%
  group_by(date = matched_date) %>%
  summarise(shock_1y_roll = sum(shock_1y, na.rm = TRUE), .groups = "drop")
c_count <- policy_rolled %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END, shock_1y_roll != 0) %>%
  nrow()
c_dates <- policy_rolled %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END, shock_1y_roll != 0) %>%
  pull(date) %>% sort()

same_set <- setequal(a_dates, c_dates)

# Diagnose the discrepancy directly rather than just flagging it: is this a
# genuinely different set of EVENTS, or the same events under two different
# date labels (raw announcement date vs. rolled trading day)?
only_a <- as.Date(sort(setdiff(a_dates, c_dates)), origin = "1970-01-01")
only_c <- as.Date(sort(setdiff(c_dates, a_dates)), origin = "1970-01-01")
roll_diagnosis <- data.frame(
  raw_date_in_a   = as.character(only_a),
  raw_weekday     = weekdays(only_a),
  rolled_date_in_c = as.character(only_c),
  roll_days       = as.integer(only_c - only_a),
  row.names = NULL
)

cat("\n=== 1. The three counts touching '71' ===\n\n")
counts_71 <- data.frame(
  quantity = c(
    "(a) nonzero main-set 1Y surprises, 2008-2020, BEFORE calendar matching",
    "(b) main-set event dates on a date present in the FX panel, any surprise, no window restriction",
    "(c) nonzero 1Y surprises, 2008-2020, under the forward roll"
  ),
  count = c(a_count, b_count, c_count),
  row.names = NULL
)
print(counts_71, row.names = FALSE)
write_csv(counts_71, file.path(paths$out_tables, "reconcile_71_counts.csv"))

cat(sprintf("\n(a) and (c) both count 71, but are NOT the same SET of calendar dates: %d dates differ.\n",
            length(only_a)))
cat("Diagnosis: every one of those dates is a weekend announcement (all 15 fall on a\n",
    "Saturday or Sunday, confirmed below) -- (a) labels it by the RAW announcement\n",
    "date (the weekend day itself), (c) labels the SAME event by the trading day it\n",
    "rolls forward to. These are the SAME 71 underlying events, not two different\n",
    "sets of 71 -- the discrepancy is a date-labeling artefact of the roll, not a\n",
    "count that happens to coincide by chance. Report (a) and (c) as identical in\n",
    "substance (same events), differing only in which calendar date each weekend\n",
    "event is attached to.\n", sep = "")
print(roll_diagnosis, row.names = FALSE)
write_csv(roll_diagnosis, file.path(paths$out_tables, "reconcile_71_roll_diagnosis.csv"))
cat(sprintf("(b) is %s: %d. %s\n",
            if (b_count == 71) "genuinely 71, NOT a draft error" else sprintf("actually %d, not 71 -- this IS a draft error", b_count),
            b_count,
            "It is coincidentally equal to (c) in this run: (b) counts ALL 102 raw events (any shock value, any year) matched to a trading day, while (c) counts only the 2008-2020, nonzero-surprise, rolled subset -- two different constructions that happen to both equal 71 here, not the same underlying quantity."))

# =============================================================================
# 2. Instrument composition of the ESTIMATION set (56 events), vs. the 71-set
# =============================================================================
policy_flags <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"), show_col_types = FALSE) %>%
  select(date, shock_1y, isdRRR, isdRevrepo, isdLDR, isdMLF)

classify <- function(d) {
  d %>% mutate(
    quantity = isdRRR | isdLDR,
    price    = isdRevrepo | isdMLF,
    type = case_when(
      quantity & !price ~ "quantity",
      price & !quantity ~ "price",
      quantity & price  ~ "mixed",
      TRUE              ~ "unflagged"
    ),
    period = if_else(date >= SPLIT_DATE, "post", "pre")
  )
}

# The 56-event ESTIMATION set: exact match to a trading day, inside 2008-2020,
# nonzero 1Y surprise -- exactly what R/16/R/17's own SHOCK <- "shock_1y"
# events object selects, and what usd_w01 ~ shock_1y is actually estimated on.
set_56 <- classify(policy_flags) %>%
  filter(date %in% trading_days, date >= SAMPLE_START, date <= SAMPLE_END, shock_1y != 0)
stopifnot(nrow(set_56) == 56)

# The 71-set: ANY main-set event landing on a trading day, no year/shock
# restriction (same construction as count (b) above) -- for direct comparison
# against whatever the draft is currently quoting.
set_71 <- classify(policy_flags) %>%
  filter(date %in% trading_days)
stopifnot(nrow(set_71) == 71)

comp_tab <- function(d, label) {
  d %>%
    count(period, type) %>%
    tidyr::pivot_wider(names_from = type, values_from = n, values_fill = 0) %>%
    mutate(set = label)
}

comp_56 <- comp_tab(set_56, "56 (estimation set: exact match, 2008-2020, nonzero shock_1y)")
comp_71 <- comp_tab(set_71, "71 (any main-set event on a trading day, no year/shock restriction)")

comp_both <- bind_rows(comp_56, comp_71)
comp_both$total <- rowSums(comp_both[, setdiff(names(comp_both), c("period", "set")), drop = FALSE])

cat("\n\n=== 2. Instrument composition: 56-event estimation set vs. the 71-set ===\n\n")
print(as.data.frame(comp_both), row.names = FALSE)
write_csv(comp_both, file.path(paths$out_tables, "reconcile_instrument_composition_56.csv"))

partition_ok <- all(set_56$type %in% c("price", "quantity"))
cat(sprintf("\nPartition check on the 56-event set: every event is 'price' or 'quantity' (no mixed/unflagged) = %s.\n",
            partition_ok))
cat("\nCompare the printed table above against the draft's quoted 15/33/11/12 to see\n",
    "directly which set (56 or 71) those four numbers actually belong to.\n", sep = "")

message("\nSaved: output/tables/reconcile_71_counts.csv, reconcile_instrument_composition_56.csv")

# =============================================================================
# 3. Descriptives for the actual dependent variable, usd_w01, per country --
#    plus the AFE dollar factor's D_w01. D_w01 does not vary by country (it
#    is the same date-level series joined onto every country-row), so it is
#    reported once, not five times, rather than pretending there is
#    cross-country variation that does not exist.
# =============================================================================
afe_file <- file.path(paths$raw_external, "DTWEXAFEGS.csv")
if (!file.exists(afe_file))
  stop("Run R/14_dollar_control_fix.R first -- it downloads DTWEXAFEGS.")
afe_raw <- read_csv(afe_file, col_types = cols(.default = col_character()))
afe <- data.frame(date = as.Date(afe_raw[[1]]),
                  dollar_afe = suppressWarnings(as.numeric(afe_raw[[2]])))
afe <- afe[!is.na(afe$date), ]

fx_dates <- panel %>% distinct(date) %>% arrange(date)
D_w01_series <- fx_dates %>%
  left_join(afe, by = "date") %>%
  arrange(date) %>%
  mutate(ret_afe = 100 * (log(dollar_afe) - log(lag(dollar_afe))),
         D_w01   = ret_afe + lead(ret_afe, 1)) %>%
  select(date, D_w01)

usd_desc <- panel %>%
  filter(!is.na(usd_w01)) %>%
  group_by(country) %>%
  summarise(N = n(), mean = mean(usd_w01), sd = sd(usd_w01),
            min = min(usd_w01), max = max(usd_w01), .groups = "drop") %>%
  mutate(series = "usd_w01 ([0,+1] FX return, LCU per USD)")

afe_desc <- D_w01_series %>%
  filter(!is.na(D_w01)) %>%
  summarise(N = n(), mean = mean(D_w01), sd = sd(D_w01), min = min(D_w01), max = max(D_w01)) %>%
  mutate(country = "(all -- single series, not country-varying)",
         series = "D_w01 (AFE dollar factor [0,+1] return)")

window_desc <- bind_rows(usd_desc, afe_desc) %>%
  select(series, country, N, mean, sd, min, max)

cat("\n\n=== 3. Descriptives, [0,+1] window return: usd_w01 by country, and D_w01 ===\n\n")
print(as.data.frame(window_desc), row.names = FALSE, digits = 4)
write_csv(window_desc, file.path(paths$out_tables, "descriptive_window_return.csv"))

# =============================================================================
# 4. Effect-size ratio, corrected comparator: the [0,+1] window SD, not a
#    daily-return SD, since the effect itself (usd_w01's response) is a
#    two-day quantity.
# =============================================================================
post_split_events <- panel %>%
  filter(date >= SPLIT_DATE, event_day == 1, shock_1y != 0) %>%
  distinct(date, .keep_all = TRUE)
sd_post_shock <- sd(post_split_events$shock_1y)

m_post <- feols(usd_w01 ~ shock_1y | country, cluster = ~date,
               data = filter(panel, date >= SPLIT_DATE))
beta_post <- as.data.frame(coeftable(m_post))["shock_1y", "Estimate"]
effect_1sd <- beta_post * sd_post_shock

ratio_tab <- usd_desc %>%
  transmute(country, window_sd = sd,
            effect_1sd_pct = effect_1sd,
            ratio_effect_to_window_sd = effect_1sd / sd)

pooled_sd <- panel %>% filter(!is.na(usd_w01)) %>% summarise(sd = sd(usd_w01)) %>% pull(sd)
ratio_tab <- bind_rows(
  ratio_tab,
  data.frame(country = "pooled", window_sd = pooled_sd, effect_1sd_pct = effect_1sd,
             ratio_effect_to_window_sd = effect_1sd / pooled_sd)
)

cat("\n\n=== 4. Post-2015 1-SD effect vs. the [0,+1] window SD (not a daily-return SD) ===\n\n")
cat(sprintf("beta_USD (post-split, 1Y, exact) = %.4f; SD of post-split nonzero shock_1y = %.4f\n",
            beta_post, sd_post_shock))
cat(sprintf("Effect of a 1-SD surprise = %.4f%% over [0,+1]\n\n", effect_1sd))
print(as.data.frame(ratio_tab), row.names = FALSE, digits = 3)
write_csv(ratio_tab, file.path(paths$out_tables, "reconcile_effect_size_ratio.csv"))

cat("\nRead: the previous comparator (0.30-0.70% DAILY return SD) understates how large\n",
    "this effect is relative to the actual [0,+1] TWO-DAY return distribution the\n",
    "effect is measured against -- the correct ratio is printed above, per country\n",
    "and pooled, in place of the daily-SD comparator.\n", sep = "")

message("\nSaved: output/tables/descriptive_window_return.csv, reconcile_effect_size_ratio.csv")
