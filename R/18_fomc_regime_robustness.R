# =============================================================================
# 18_fomc_regime_robustness.R
# The H3 regime result (R/12, "Test of the structural break") is the
# shock_1y x post_split interaction on usd_w01, [0,+1] window, 2008-2020,
# country fixed effects, clustered by date. Two obvious contamination
# concerns for a result built on PBoC announcement dates:
#
#   1. FOMC PROXIMITY. If a PBoC announcement falls within a day of an FOMC
#      decision, the [0,+1] FX return may be picking up the U.S. policy
#      surprise rather than (or in addition to) the Chinese one. If the
#      break survives dropping those dates, it is not an FOMC-timing
#      artefact.
#   2. 2020. The sample runs through 2020-05-29, so it includes the March
#      2020 COVID FX turmoil. If the break survives dropping all of 2020,
#      it is not an artefact of that one episode.
#
# FOMC dates: data-raw/external/fomc_meeting_dates_2008_2020.csv -- a
# verified list (meeting_start, meeting_end, decision_date, meeting_type,
# source, notes), 103 scheduled + 5 unscheduled meetings, 2008-2020. See
# docs/source_log.md for provenance. This supersedes an earlier
# memory-compiled version of this file that had real errors; do not
# regenerate it from a shock/policy series or from memory.
#
# WHICH DATE, AND WHICH MEETINGS.
#   - The relevant date is decision_date: the day the policy statement
#     reached markets. For most meetings that's the last day of the meeting,
#     but in four cases (three 2008 conference calls and the 2 March 2020
#     meeting) the statement followed the meeting by a day, and decision_date
#     captures that.
#   - The HEADLINE exclusion uses SCHEDULED meetings only. Scheduled FOMC
#     dates are pre-announced on a calendar fixed well in advance, so their
#     timing is exogenous to Chinese monetary policy -- that is what makes
#     excluding them a clean test of FOMC contamination. Unscheduled meetings
#     are called precisely when global financial conditions are already
#     disorderly (Jan/Oct 2008, Mar 2020), so dropping those dates would
#     selectively remove the periods where a global-shock confound is most
#     plausible and bias the robustness check in favour of the regime result.
#     The any-meeting cut is reported alongside as a more aggressive
#     sensitivity check, not as the headline.
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
                 show_col_types = FALSE) %>%
  mutate(decision_date = as.Date(decision_date))

stopifnot(all(fomc$meeting_type %in% c("scheduled", "unscheduled")))

SHOCK <- "shock_1y"

# =============================================================================
# 1. Flag event dates within 1 trading day of an FOMC decision
# =============================================================================
# decision_date is a U.S. calendar date; ASEAN markets first have a chance to
# price it on the next ASEAN trading day, so each decision_date is rolled
# onto the FX trading calendar with the same roll_to_trading_day() used
# throughout for PBoC announcements (R/00). "Within 1 trading day" means the
# event date's position in the trading calendar is at most 1 away from an
# FOMC-mapped trading day (0 = same day, 1 = the session immediately before
# or after) -- the same notion of adjacency the [0,+1] window itself uses,
# not calendar-day proximity.
trading_days <- panel %>% distinct(date) %>% arrange(date) %>% pull(date)

fomc_adjacent_flag <- function(decision_dates, trading_days, label) {
  roll <- roll_to_trading_day(decision_dates, trading_days, max_roll = 5)
  if (any(is.na(roll$matched_date)))
    message("Note: ", sum(is.na(roll$matched_date)), " ", label,
            " decision date(s) could not be matched to a trading day within 5 days.")

  fomc_td  <- sort(unique(roll$matched_date[!is.na(roll$matched_date)]))
  fomc_idx <- match(fomc_td, trading_days)

  out <- data.frame(date = trading_days, idx = seq_along(trading_days))
  out$adjacent <- vapply(out$idx, function(i) any(abs(i - fomc_idx) <= 1), logical(1))
  out %>% select(date, adjacent)
}

adj_sched <- fomc_adjacent_flag(fomc$decision_date[fomc$meeting_type == "scheduled"],
                                trading_days, "scheduled") %>%
  rename(fomc_adjacent_sched = adjacent)
adj_any   <- fomc_adjacent_flag(fomc$decision_date,
                                trading_days, "scheduled or unscheduled") %>%
  rename(fomc_adjacent_any = adjacent)

panel <- panel %>%
  left_join(adj_sched, by = "date") %>%
  left_join(adj_any,   by = "date")

events_sched <- panel %>%
  filter(event_day == 1, .data[[SHOCK]] != 0, fomc_adjacent_sched) %>%
  distinct(date) %>% arrange(date)
events_any <- panel %>%
  filter(event_day == 1, .data[[SHOCK]] != 0, fomc_adjacent_any) %>%
  distinct(date) %>% arrange(date)

n_events_total <- n_distinct(panel$date[panel$event_day == 1 & panel[[SHOCK]] != 0])

cat("\n=== Event dates within 1 trading day of a SCHEDULED FOMC decision (headline rule) ===\n\n")
print(as.data.frame(events_sched), row.names = FALSE)
cat(sprintf("\n%d of %d nonzero-shock event dates flagged under the scheduled-only rule.\n",
            nrow(events_sched), n_events_total))

cat("\n\n=== Event dates within 1 trading day of ANY FOMC decision, scheduled or unscheduled (sensitivity rule) ===\n\n")
print(as.data.frame(events_any), row.names = FALSE)
cat(sprintf("\n%d of %d nonzero-shock event dates flagged under the any-meeting rule.\n",
            nrow(events_any), n_events_total))

note_fmt <- paste0(
  "\nNOTE ON THE FLAG COUNT: an earlier, memory-compiled version of the FOMC\n",
  "date file (since replaced) flagged 9 of %d event dates using calendar-rolled\n",
  "meeting dates with no scheduled/unscheduled distinction and no verified\n",
  "decision-date correction. The verified file flags %d (scheduled-only) and\n",
  "%d (any-meeting) instead. %s\n"
)
cat(sprintf(
  note_fmt,
  n_events_total, nrow(events_sched), nrow(events_any),
  if (nrow(events_sched) != 9)
    "This is a real change in which dates are excluded, driven by corrected/added meeting dates and the decision-date (vs. meeting-date) keying -- not a coding difference in this script."
  else
    "The count happens to coincide with the earlier run despite the corrected dates; the underlying flagged dates should still be checked, not assumed identical."
))

# =============================================================================
# 2. Four samples: baseline, excl. scheduled-FOMC-adjacent, excl. 2020,
#    excl. any-FOMC-adjacent (sensitivity)
# =============================================================================
panel_excl_sched <- panel %>% filter(!(date %in% events_sched$date))
panel_excl_2020  <- panel %>% filter(lubridate::year(date) != 2020)
panel_excl_any   <- panel %>% filter(!(date %in% events_any$date))

n_2020_events <- panel %>%
  filter(event_day == 1, .data[[SHOCK]] != 0, lubridate::year(date) == 2020) %>%
  distinct(date) %>% nrow()
cat(sprintf("\n%d nonzero-shock event dates fall in 2020 and are dropped in specification (3).\n",
            n_2020_events))

fml <- as.formula(sprintf("usd_w01 ~ %s * post_split | country", SHOCK))

m_base        <- feols(fml, cluster = ~date, data = panel)
m_excl_sched  <- feols(fml, cluster = ~date, data = panel_excl_sched)
m_excl_2020   <- feols(fml, cluster = ~date, data = panel_excl_2020)
m_excl_any    <- feols(fml, cluster = ~date, data = panel_excl_any)

cat("\n\n=== Regime interaction (shock_1y x post_split): baseline vs. robustness ===\n")
print(etable(
  list("(1) Baseline"                        = m_base,
       "(2) Excl. scheduled-FOMC-adjacent"   = m_excl_sched,
       "(3) Excl. 2020"                       = m_excl_2020,
       "(4) Excl. any-FOMC-adjacent [sens.]" = m_excl_any),
  cluster = ~date, digits = 3, fitstat = c("n", "r2", "wr2")
))

# =============================================================================
# 3. Comparison table
# =============================================================================
int_term <- paste0(SHOCK, ":post_split")

fomc_2020_tab <- bind_rows(
  coef_row(m_base,       int_term, "shock_1y x post_split",
           extra = list(spec = "(1) baseline")),
  coef_row(m_excl_sched, int_term, "shock_1y x post_split",
           extra = list(spec = "(2) excl. scheduled-FOMC-adjacent [headline]")),
  coef_row(m_excl_2020,  int_term, "shock_1y x post_split",
           extra = list(spec = "(3) excl. 2020")),
  coef_row(m_excl_any,   int_term, "shock_1y x post_split",
           extra = list(spec = "(4) excl. any-FOMC-adjacent [sensitivity]"))
)

cat("\n\n=== Comparison table ===\n\n")
print(fomc_2020_tab[, c("spec", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(fomc_2020_tab, file.path(paths$out_tables, "regime_fomc_2020_robustness.csv"))

cat("\nIf the interaction stays negative and significant in (2) and (3), the\n",
    "H3 regime break is not an FOMC-proximity artefact (using the clean,\n",
    "pre-announced scheduled-meeting cut) and is not being driven by the 2020\n",
    "COVID episode alone. (4) is reported as a sensitivity check: it also\n",
    "drops dates around unscheduled meetings, which is a more aggressive and\n",
    "less clean cut since those meetings are themselves triggered by global\n",
    "financial stress.\n", sep = "")

message("\nSaved: output/tables/regime_fomc_2020_robustness.csv")
