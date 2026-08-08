# =============================================================================
# 13_regime_stress_tests.R
# The sample split in R/12 produced the strongest result in the project: a
# significant negative post-2015 response under the USD numeraire, no
# corresponding break under the RMB numeraire, and an interaction term
# significant at the 0.1% level.
#
# A result that large, resting on 18 post-split announcement dates, has to be
# attacked before it is written up. This script runs six checks. Each is
# designed so that FAILING it would mean the result should be reported as
# suggestive rather than as a finding.
#
#   1. Windows and shock measures. Does the break survive [0] and [-1,+1], the
#      5Y measure, and the rolled (weekend-corrected) shock series?
#   2. Controls. Does it survive a dollar-strength control, which alone
#      explains a material share of daily return variance? Run on a COMMON
#      SAMPLE so the columns are nested and the coefficient path is
#      interpretable. The preferred control is the advanced-foreign-economies
#      (AFE) dollar index, not the broad dollar index: the broad index
#      (DTWEXBGS) has the ASEAN-5 currencies AND the renminbi as constituents,
#      so conditioning on it puts a transformation of the dependent variable
#      on the right-hand side, and partially absorbs the renminbi's own
#      response -- which is a transmission channel here (R/11, R/15), not a
#      confounder. The AFE index (DTWEXAFEGS) excludes both, so it measures
#      general dollar strength without mechanically containing either the
#      channel or the outcome. The broad-dollar columns are still reported,
#      last, explicitly labelled as misspecified, so the contrast is visible
#      rather than assumed away.
#   3. Influence. Drop each post-split event date in turn. If one date carries
#      the result, that is a fragility to report, not a finding.
#   4. Alternative split dates. If the break is real it should not be
#      knife-edge on 2015-08-11.
#   5. Mechanism. Estimate beta_USD, beta_CNY and beta_cross separately by
#      period, to test directly whether the post-2015 USD response is the
#      renminbi's own response transmitting to ASEAN currencies.
#   6. Economic magnitude. A significant coefficient on a small surprise can
#      still be an economically trivial exchange-rate move.
#
# Input : data-clean/reg_data_ext_main.csv
#         data-raw/external/DTWEXAFEGS.csv   (fetched by R/14)
# Output: output/tables/regime_*.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

SHOCK   <- "shock_1y"
WINDOWS <- c("[0]" = "w0", "[0,+1]" = "w01", "[-1,+1]" = "wm11")

int_term <- function(m, shock) {
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  hit <- ct[grepl("post_split", ct$term) & grepl(shock, ct$term), , drop = FALSE]
  if (!nrow(hit)) return(NULL)
  data.frame(coef = hit$Estimate[1], se = hit$`Std. Error`[1],
             pval = hit[[grep("^Pr", names(hit))[1]]][1],
             nobs = nobs(m), row.names = NULL)
}

# =============================================================================
# 1. Windows, shock measures, and matching rule
# =============================================================================
grid <- expand.grid(
  window   = names(WINDOWS),
  shock    = c("shock_1y", "shock_5y", "shock_1y_roll", "shock_5y_roll"),
  numer    = c("usd", "cny"),
  stringsAsFactors = FALSE
)

chk1 <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  y <- paste0(g$numer, "_", WINDOWS[[g$window]])
  m <- feols(as.formula(sprintf("%s ~ %s * post_split | country", y, g$shock)),
             cluster = ~date, data = panel)
  r <- int_term(m, g$shock)
  if (is.null(r)) return(NULL)
  cbind(numeraire = g$numer, window = g$window, shock = g$shock, r)
}))

cat("\n=== 1. Interaction (shock x post-2015) across windows, measures, numeraires ===\n\n")
print(chk1, row.names = FALSE, digits = 3)
write_csv(chk1, file.path(paths$out_tables, "regime_windows.csv"))
cat("\nRead: the break is credible only if it is confined to the USD numeraire\n",
    "and appears in more than one window/measure. A break that shows up only in\n",
    "[0,+1] with the 1Y shock is one specification, not a finding.\n", sep = "")

# =============================================================================
# 2. Controls, on a nested common sample
# =============================================================================
# The control ladder in R/11 section D lost observations column by column
# (13,017 -> 12,313), so the coefficient path mixed the effect of adding a
# control with the effect of changing the sample. Here the sample is fixed to
# rows where every control -- UST 2Y, AFE dollar, broad dollar, VIX, Brent --
# is observed, so every column below, preferred and misspecified alike, is
# genuinely nested on one common sample.

# --- AFE dollar control (same construction as R/14/R/15/R/19) ---------------
afe_file <- file.path(paths$raw_external, "DTWEXAFEGS.csv")
if (!file.exists(afe_file))
  stop("Run R/14_dollar_control_fix.R first -- it downloads DTWEXAFEGS.")

afe_raw <- read_csv(afe_file, col_types = cols(.default = col_character()))
afe <- data.frame(date = as.Date(afe_raw[[1]]),
                  dollar_afe = suppressWarnings(as.numeric(afe_raw[[2]])))
afe <- afe[!is.na(afe$date), ]

afe_ctrl <- panel %>% distinct(date) %>% arrange(date) %>%
  left_join(afe, by = "date") %>%
  mutate(ret_dollar_afe = 100 * (log(dollar_afe) - log(lag(dollar_afe))),
         ctrl_afe_w01   = ret_dollar_afe + lead(ret_dollar_afe, 1)) %>%
  select(date, ctrl_afe_w01)

panel <- panel %>% left_join(afe_ctrl, by = "date")

ctrl_vars <- c("ctrl_ust2y_w01", "ctrl_afe_w01", "ctrl_dollar_w01",
               "ctrl_vix_w01", "ctrl_brent_w01")

common <- panel %>%
  filter(!is.na(usd_w01), if_all(all_of(ctrl_vars), ~ !is.na(.x)))

message("Common-sample rows for the control ladder: ", nrow(common))

fml_c <- function(rhs) as.formula(sprintf(
  "usd_w01 ~ %s * post_split%s | country",
  SHOCK, if (length(rhs)) paste0(" + ", paste(rhs, collapse = " + ")) else ""))

m1 <- feols(fml_c(NULL),                                       cluster = ~date, data = common)
m2 <- feols(fml_c("ctrl_ust2y_w01"),                           cluster = ~date, data = common)
m3 <- feols(fml_c(c("ctrl_ust2y_w01", "ctrl_afe_w01")),        cluster = ~date, data = common)
m4 <- feols(fml_c(c("ctrl_ust2y_w01", "ctrl_afe_w01",
                    "ctrl_vix_w01", "ctrl_brent_w01")),        cluster = ~date, data = common)
m5 <- feols(fml_c(c("ctrl_ust2y_w01", "ctrl_dollar_w01")),     cluster = ~date, data = common)
m6 <- feols(fml_c(c("ctrl_ust2y_w01", "ctrl_dollar_w01",
                    "ctrl_vix_w01", "ctrl_brent_w01")),        cluster = ~date, data = common)

cat("\n\n=== 2. Does the break survive controls? (common sample, nested) ===\n")
print(etable(list(
    "(1) no controls"                                = m1,
    "(2) + UST 2Y"                                    = m2,
    "(3) + UST 2Y + AFE dollar"                       = m3,
    "(4) + UST 2Y + AFE dollar + VIX + Brent"         = m4,
    "(5) + UST 2Y + broad dollar [misspecified]"      = m5,
    "(6) + UST 2Y + broad dollar + VIX + Brent [misspecified]" = m6),
  cluster = ~date, digits = 3, fitstat = c("n", "r2", "wr2")))
cat("\nColumns (1)-(4) are the preferred ladder: the AFE dollar index is a\n",
    "dollar-strength control that does not contain the ASEAN-5 currencies or\n",
    "the renminbi. Columns (5)-(6) repeat (3)-(4) with the broad dollar index\n",
    "in place of AFE, reported only for contrast -- see the header note above\n",
    "for why they are labelled misspecified. If the interaction survives\n",
    "column (3), the post-2015 response is not simply ASEAN currencies\n",
    "tracking general dollar movement.\n", sep = "")

# =============================================================================
# 3. Influence: leave-one-event-out over the post-split announcements
# =============================================================================
post_events <- panel %>%
  filter(date >= SPLIT_DATE, event_day == 1, .data[[SHOCK]] != 0) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

cat("\n\n=== 3. Leave-one-out over the ", nrow(post_events),
    " post-split event dates ===\n", sep = "")

loo <- bind_rows(lapply(seq_len(nrow(post_events)), function(i) {
  d  <- post_events$date[i]
  m  <- feols(as.formula(sprintf("usd_w01 ~ %s | country", SHOCK)),
              cluster = ~date,
              data = filter(panel, date >= SPLIT_DATE, date != d))
  ct <- as.data.frame(coeftable(m))
  data.frame(dropped_date = as.character(d),
             shock_dropped = post_events[[SHOCK]][i],
             coef = ct[SHOCK, "Estimate"],
             se   = ct[SHOCK, "Std. Error"],
             pval = ct[SHOCK, grep("^Pr", names(ct))[1]],
             row.names = NULL, stringsAsFactors = FALSE)
}))

full_post <- feols(as.formula(sprintf("usd_w01 ~ %s | country", SHOCK)),
                   cluster = ~date, data = filter(panel, date >= SPLIT_DATE))
base_coef <- as.data.frame(coeftable(full_post))[SHOCK, "Estimate"]

cat("\nPost-split coefficient with all events: ", round(base_coef, 3), "\n\n", sep = "")
print(loo[order(loo$coef), ], row.names = FALSE, digits = 3)
write_csv(loo, file.path(paths$out_tables, "regime_leave_one_out.csv"))

cat(sprintf("\nRange across leave-one-out: [%.3f, %.3f];  p-value stays below 0.05 in %d of %d drops.\n",
            min(loo$coef), max(loo$coef), sum(loo$pval < 0.05), nrow(loo)))
cat("If dropping a single date moves the coefficient outside the others' range,\n",
    "or flips significance, the result is driven by that date and must be\n",
    "reported that way.\n", sep = "")

# =============================================================================
# 4. Alternative split dates
# =============================================================================
alt_dates <- as.Date(c("2014-06-30",  # before the property downturn bites
                       "2014-11-22",  # first LDR cut of the easing cycle
                       "2015-01-01",  # calendar-year alternative
                       "2015-08-11",  # RMB fixing reform (baseline)
                       "2016-01-01",  # calendar-year alternative
                       "2016-06-30")) # after the reform settles

chk4 <- bind_rows(lapply(alt_dates, function(dd) {
  p2 <- panel %>% mutate(ps = if_else(date >= dd, 1L, 0L))
  m  <- feols(as.formula(sprintf("usd_w01 ~ %s * ps | country", SHOCK)),
              cluster = ~date, data = p2)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  hit <- ct[grepl(":ps", ct$term), , drop = FALSE]
  n_post <- p2 %>% filter(ps == 1, event_day == 1, .data[[SHOCK]] != 0) %>%
    distinct(date) %>% nrow()
  data.frame(split_date = as.character(dd), n_post_events = n_post,
             interaction = hit$Estimate[1], se = hit$`Std. Error`[1],
             pval = hit[[grep("^Pr", names(hit))[1]]][1],
             row.names = NULL, stringsAsFactors = FALSE)
}))

cat("\n\n=== 4. Sensitivity to the split date ===\n\n")
print(chk4, row.names = FALSE, digits = 3)
write_csv(chk4, file.path(paths$out_tables, "regime_alt_splits.csv"))
cat("\nA real regime change should show a break across a band of nearby dates,\n",
    "strengthening as the split approaches the true break. A result confined to\n",
    "exactly one date is a coincidence, not a regime.\n", sep = "")

# =============================================================================
# 5. Mechanism: the decomposition, by period
# =============================================================================
# Tests the interpretation directly. If the post-2015 USD response is ASEAN
# currencies co-moving with a renminbi that has itself become policy-responsive,
# then post-split beta_CNY should be large and negative while beta_cross stays
# near zero.
mech <- bind_rows(lapply(c("pre", "post"), function(per) {
  d0 <- if (per == "pre") filter(panel, date < SPLIT_DATE)
        else               filter(panel, date >= SPLIT_DATE)
  d0 <- d0 %>%
    mutate(rmb_leg = usd_w01 - cny_w01) %>%
    filter(!is.na(usd_w01), !is.na(cny_w01))
  f <- function(y) feols(as.formula(sprintf("%s ~ %s | country", y, SHOCK)),
                         cluster = ~date, data = d0)
  bind_rows(
    coef_row(f("usd_w01"), SHOCK, "beta_USD",   extra = list(period = per)),
    coef_row(f("rmb_leg"), SHOCK, "beta_CNY",   extra = list(period = per)),
    coef_row(f("cny_w01"), SHOCK, "beta_cross", extra = list(period = per))
  )
}))

cat("\n\n=== 5. Decomposition by period ([0,+1], common sample within period) ===\n\n")
print(mech[, c("period", "term", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(mech, file.path(paths$out_tables, "regime_mechanism.csv"))

for (per in c("pre", "post")) {
  b <- mech[mech$period == per, ]
  cat(sprintf("  %-5s identity: %+.3f - (%+.3f) = %+.3f  (beta_cross = %+.3f)\n",
              per, b$coef[1], b$coef[2], b$coef[1] - b$coef[2], b$coef[3]))
}

# =============================================================================
# 6. Economic magnitude
# =============================================================================
# A large coefficient on a small surprise can still be an economically trivial
# move. Scale by the actual dispersion of post-split surprises.
sd_post <- panel %>%
  filter(date >= SPLIT_DATE, event_day == 1, .data[[SHOCK]] != 0) %>%
  distinct(date, .keep_all = TRUE) %>%
  summarise(sd = sd(.data[[SHOCK]]), mean_abs = mean(abs(.data[[SHOCK]]))) 

post_coef <- mech$coef[mech$period == "post" & mech$term == "beta_USD"]

cat("\n\n=== 6. Economic magnitude, post-split ===\n")
cat(sprintf("  SD of post-split 1Y surprises      : %.4f (pp of swap rate)\n", sd_post$sd))
cat(sprintf("  beta_USD, post-split               : %.3f\n", post_coef))
cat(sprintf("  FX response to a 1-SD surprise     : %.4f%% over [0,+1]\n",
            post_coef * sd_post$sd))
cat(sprintf("  ... to the largest post-split surprise (0.15): %.4f%%\n",
            post_coef * 0.15))
cat("\nCompare against typical daily FX volatility (roughly 0.3-0.7%% per the\n",
    "descriptive statistics). Report the magnitude alongside the p-value: a\n",
    "statistically detectable move well inside one day's normal variation is a\n",
    "different claim from an economically important one.\n", sep = "")

message("\nSaved: output/tables/regime_*.csv")
