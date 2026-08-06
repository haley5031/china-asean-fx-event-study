# =============================================================================
# 18_fomc_regime_robustness.R
# The H3 regime result (R/12, "Test of the structural break") is the
# shock_1y x post_split interaction on usd_w01, [0,+1] window, 2008-2020,
# country fixed effects, clustered by date. Two obvious contamination
# concerns for a result built on PBoC announcement dates:
#
#   1. FOMC PROXIMITY. If a PBoC announcement falls within a day of an FOMC
#      meeting, the [0,+1] FX return may be picking up the U.S. policy
#      surprise rather than (or in addition to) the Chinese one. If the
#      break survives dropping those dates, it is not an FOMC-timing
#      artefact.
#   2. 2020. The sample runs through 2020-05-29, so it includes the March
#      2020 COVID FX turmoil. If the break survives dropping all of 2020,
#      it is not an artefact of that one episode.
#
# FOMC dates: data-raw/external/fomc_meeting_dates_2008_2020.csv. See
# docs/source_log.md for provenance -- that file is a manually compiled list
# (this environment cannot reach federalreserve.gov to fetch it
# programmatically) and is flagged there for verification against the
# official calendar before use in the thesis.
#
# Input : data-clean/reg_data_ext_main.csv
#         data-raw/external/fomc_meeting_dates_2008_2020.csv
# Output: output/tables/regime_fomc_2020_robustness.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

fomc <- read_csv(file.path(paths$raw_external, "fomc_meeting_dates_2008_2020.csv"),
                 show_col_types = FALSE)

SHOCK <- "shock_1y"

# =============================================================================
# 1. Flag event dates within 1 trading day of an FOMC meeting
# =============================================================================
# FOMC dates are U.S. calendar dates; ASEAN markets first have a chance to
# price the decision on the next ASEAN trading day, so each FOMC date is
# rolled onto the FX trading calendar with the same roll_to_trading_day()
# used throughout for PBoC announcements (R/00). "Within 1 trading day" then
# means the event date's position in the trading calendar is at most 1 away
# from an FOMC-mapped trading day (0 = same day, 1 = the session immediately
# before or after).
trading_days <- panel %>% distinct(date) %>% arrange(date) %>% pull(date)

fomc_roll <- roll_to_trading_day(fomc$date, trading_days, max_roll = 5)
if (any(is.na(fomc_roll$matched_date)))
  message("Note: ", sum(is.na(fomc_roll$matched_date)),
          " FOMC date(s) could not be matched to a trading day within 5 days.")

fomc_td  <- sort(unique(fomc_roll$matched_date[!is.na(fomc_roll$matched_date)]))
fomc_idx <- match(fomc_td, trading_days)

dates_all <- data.frame(date = trading_days, idx = seq_along(trading_days))
dates_all$fomc_adjacent <- vapply(
  dates_all$idx, function(i) any(abs(i - fomc_idx) <= 1), logical(1)
)

panel <- panel %>% left_join(dates_all %>% select(date, fomc_adjacent), by = "date")

fomc_adjacent_events <- panel %>%
  filter(event_day == 1, .data[[SHOCK]] != 0, fomc_adjacent) %>%
  distinct(date) %>%
  arrange(date)

cat("\n=== Event dates within 1 trading day of an FOMC meeting ===\n\n")
print(as.data.frame(fomc_adjacent_events), row.names = FALSE)
cat(sprintf("\n%d of %d nonzero-shock event dates in the 2008-2020 sample are FOMC-adjacent.\n",
            nrow(fomc_adjacent_events),
            n_distinct(panel$date[panel$event_day == 1 & panel[[SHOCK]] != 0])))

# =============================================================================
# 2. Three samples: baseline, excl. FOMC-adjacent, excl. 2020
# =============================================================================
panel_excl_fomc <- panel %>% filter(!(date %in% fomc_adjacent_events$date))
panel_excl_2020 <- panel %>% filter(lubridate::year(date) != 2020)

n_2020_events <- panel %>%
  filter(event_day == 1, .data[[SHOCK]] != 0, lubridate::year(date) == 2020) %>%
  distinct(date) %>% nrow()
cat(sprintf("\n%d nonzero-shock event dates fall in 2020 and are dropped in specification (3).\n",
            n_2020_events))

fml <- as.formula(sprintf("usd_w01 ~ %s * post_split | country", SHOCK))

m_base       <- feols(fml, cluster = ~date, data = panel)
m_excl_fomc  <- feols(fml, cluster = ~date, data = panel_excl_fomc)
m_excl_2020  <- feols(fml, cluster = ~date, data = panel_excl_2020)

cat("\n\n=== Regime interaction (shock_1y x post_split): baseline vs. robustness ===\n")
print(etable(
  list("(1) Baseline"              = m_base,
       "(2) Excl. FOMC-adjacent"   = m_excl_fomc,
       "(3) Excl. 2020"            = m_excl_2020),
  cluster = ~date, digits = 3, fitstat = c("n", "r2", "wr2")
))

# =============================================================================
# 3. Comparison table
# =============================================================================
int_term <- paste0(SHOCK, ":post_split")

fomc_2020_tab <- bind_rows(
  coef_row(m_base,      int_term, "shock_1y x post_split",
           extra = list(spec = "(1) baseline")),
  coef_row(m_excl_fomc, int_term, "shock_1y x post_split",
           extra = list(spec = "(2) excl. FOMC-adjacent")),
  coef_row(m_excl_2020, int_term, "shock_1y x post_split",
           extra = list(spec = "(3) excl. 2020"))
)

cat("\n\n=== Comparison table ===\n\n")
print(fomc_2020_tab[, c("spec", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(fomc_2020_tab, file.path(paths$out_tables, "regime_fomc_2020_robustness.csv"))

cat("\nIf the interaction stays negative and significant in (2) and (3), the\n",
    "H3 regime break is not an FOMC-proximity artefact and is not being driven\n",
    "by the 2020 COVID episode alone.\n", sep = "")

message("\nSaved: output/tables/regime_fomc_2020_robustness.csv")
